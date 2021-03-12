; -*- mode: Lisp;-*-

;;;; mst.lips
;;;  OA: https://github.com/PhantoNa
;;;  Originally made for an university project

(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))

(defstruct heap id size actual-heap position-ht)


(defun is-graph (graph-id)
  (cond ((gethash graph-id *graphs*)
         t)
        (t nil)))

(defun new-graph (graph-id)
  (cond ((null graph-id)
         (error "Error graph name can't be NIL."))
        (t
         (or (is-graph graph-id)
             (and
              (setf
               (gethash graph-id *arcs*)
               (make-hash-table :test #'equal))
              (setf (gethash graph-id *graphs*) t)))
         graph-id)))

(defun new-vertex (graph-id vertex-id)
  (cond ((null (atom vertex-id))
         (error "Error non atomic vertex name: ~S." vertex-id))
        ((null vertex-id)
         (error "Error vertex name can't be NIL."))
        (t
         (new-graph graph-id)
         (or (gethash (list graph-id vertex-id) *vertices*)
             (setf
              (gethash (list graph-id vertex-id) *vertices*)
              t))
         (list 'vertex graph-id vertex-id))))

(defun new-arc (graph-id vertex-id vertex-id2 &optional (weight 1))
  (cond ((null (numberp weight))
         (error "Invalid non-numeric weight: ~S." weight))
        ((< weight 0)
         (error "Invalid negative weight: ~S." weight))
        ((equal vertex-id vertex-id2) nil)
        (t (new-graph graph-id)
           (new-vertex graph-id vertex-id)
           (new-vertex graph-id vertex-id2)
           (cond ((null (gethash vertex-id (gethash graph-id *arcs*)))
                  (setf
                   (gethash vertex-id (gethash graph-id *arcs*))
                   (list vertex-id (make-hash-table :test #'equal)))))
           (cond ((null (gethash vertex-id2 (gethash graph-id *arcs*)))
                  (setf
                   (gethash vertex-id2 (gethash graph-id *arcs*))
                   (list vertex-id2 (make-hash-table :test #'equal)))))
           (setf
            (gethash vertex-id (gethash graph-id *arcs*))
            (list
             vertex-id
             (second (gethash vertex-id (gethash graph-id *arcs*)))))
           (setf
            (gethash vertex-id2
                     (second (gethash vertex-id (gethash graph-id *arcs*))))
            (list vertex-id2 weight))
           (setf
            (gethash vertex-id2 (gethash graph-id *arcs*))
            (list
             vertex-id2
             (second (gethash vertex-id2 (gethash graph-id *arcs*)))))
           (setf
            (gethash vertex-id
                     (second (gethash vertex-id2 (gethash graph-id *arcs*))))
            (list vertex-id weight))
           (list 'arc vertex-id vertex-id2 weight))))

(defun delete-graph (graph-id)
  (cond ((null (is-graph graph-id)) nil)
        (t (remhash graph-id *graphs*)
           (remhash graph-id *arcs*)
           (maphash #'(lambda (k v)
                        (declare (ignore v))
                        (if (equal graph-id (second k))
                            (remhash k *vertices*)))
                    *vertices*)
           (maphash #'(lambda (k v)
                        (declare (ignore v))
                        (if (equal graph-id (first k))
                            (remhash k *vertex-keys*)))
                    *vertex-keys*)
           (maphash #'(lambda (k v)
                        (declare (ignore v))
                        (if (equal graph-id (first k))
                            (remhash k *previous*)))
                    *previous*)
           t)))

(defun graph-vertices (graph-id)
  (cond ((null (is-graph graph-id))
         (error "Error invalid graph ~S." graph-id))
        (t
         (let ((vlist ()))
           (maphash #'(lambda (k v)
                        (declare (ignore v))
                        (push (list 'vertex graph-id k) vlist))
                    (gethash graph-id *arcs*))
           vlist))))

(defun graph-arcs (graph-id)
  (cond ((null (is-graph graph-id))
         (error "Error invalid graph ~S." graph-id))
        (t
         (let ((arclist ()) (added-arc (make-hash-table :test #'equal)))
           (maphash #'(lambda (k v)
                        (maphash #'(lambda (k2 v2)
                                     (declare (ignore k2))
                                     (cond ((and
                                             (null
                                              (gethash
                                               (list k (first v2)) added-arc))
                                             (null
                                              (gethash
                                               (list (first v2) k) added-arc)))
                                            (setf
                                             (gethash
                                              (list k (first v2)) added-arc)
                                             t)
                                            (push
                                             (list
                                              'arc
                                              graph-id k
                                              (first v2)
                                              (second v2))
                                             arclist))))
                                 (second v)))
                    (gethash graph-id *arcs*))
           arclist))))

(defun graph-vertex-adjacent (graph-id vertex-id)
  (cond ((null (is-graph graph-id))
         (error "Error invalid graph ~S." graph-id))
        ((null (gethash (list graph-id vertex-id) *vertices*))
         (error "No vertex ~S in graph ~S." vertex-id graph-id))
        (t
         (let ((neighborslist ()))
           (maphash #'(lambda (k v)
                        (declare (ignore k))
                        (push
                         (list 'vertex graph-id (first v))
                         neighborslist))
                    (second (gethash vertex-id (gethash graph-id *arcs*))))
           neighborslist))))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (cond ((null (is-graph graph-id))
         (error "Error invalid graph ~S." graph-id))
        ((null (gethash (list graph-id vertex-id) *vertices*))
         (error "No vertex ~S in graph ~S." vertex-id graph-id))
        (t
         (let ((adjslist ()))
           (maphash #'(lambda (k v)
                        (declare (ignore k))
                        (push
                         (list 'arc vertex-id (first v) (second v))
                         adjslist))
                    (second (gethash vertex-id (gethash graph-id *arcs*))))
           adjslist))))

(defun graph-print (graph-id)
  (cond ((null (is-graph graph-id))
         (error "Error invalid graph ~S." graph-id))
        (t
         (let ((added-arc (make-hash-table :test #'equal)))
           (format t "~%[ VERTICES: ]~%")
           (maphash #'(lambda (k v)
                        (declare (ignore v))
                        (format t "(vertex ~S ~S)~%" graph-id k))
                    (gethash graph-id *arcs*))
           (format t "~%[   ARCS:   ]~%")
           (maphash #'(lambda (k v)
                        (maphash #'(lambda (k2 v2)
                                     (declare (ignore k2))
                                     (cond ((and
                                             (null
                                              (gethash
                                               (list k (first v2)) added-arc))
                                             (null
                                              (gethash
                                               (list (first v2) k) added-arc)))
                                            (setf
                                             (gethash
                                              (list k (first v2)) added-arc)
                                             t)
                                            (format t "(arc ~S ~S ~S ~S)~%"
                                                    graph-id k
                                                    (first v2)
                                                    (second v2)))))
                                 (second v)))
                    (gethash graph-id *arcs*))
           (values)))))

(defun new-heap (heap-id &optional (capacity 42))
  (cond ((null (numberp capacity))
         (error "Invalid non-numeric capacity: ~S." capacity))
        ((< capacity 0)
         (error "Invalid negative heap capacity: ~S." capacity))
        (t
         (or (gethash heap-id *heaps*)
             (setf
              (gethash heap-id *heaps*)
              (make-heap
               :id heap-id
               :size 0
               :actual-heap (make-array capacity :adjustable t)
               :position-ht (make-hash-table :test #'equal)))))))

(defun heap-delete (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    ;; null condition should return nil for consistence
    (cond ((null heap-rep) t)
          (t (remhash heap-id *heaps*)
             t))))

(defun heap-empty (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap ~S." heap-id))
          ((eql (heap-size heap-rep) 0) t)
          (t nil))))

(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))

(defun heap-head (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap ~S." heap-id))
          ((heap-empty heap-id)
           (error "Heap ~S empty." heap-id))
          (t
           (aref (heap-actual-heap heap-rep) 0)))))

(defun heap-elem (heap-id pos)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap rep ~S." heap-rep))
          ((< pos 0)
           (error "Negative position index heap."))
          ((> pos (heap-size heap-rep))
           (error "Position index out of bound (> heap-size)."))
          (t
           (aref (heap-actual-heap heap-rep) pos)))))

(defun heap-insert (heap-id k v)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond((or (null heap-rep)
              (null (numberp k))
              (gethash v (heap-position-ht heap-rep)))
          nil)
         (t
          (cond ((equal
                  (heap-size heap-rep)
                  (length (heap-actual-heap heap-rep)))
                 (setf
                  (gethash heap-id *heaps*)
                  (make-heap
                   :id heap-id
                   :size (heap-size heap-rep)
                   ;;array is adjusted by extending it by + ~1/2 size if full
                   :actual-heap (adjust-array
                                 (heap-actual-heap heap-rep)
                                 (list
                                  (+ (heap-size heap-rep)
                                     (+ (floor (heap-size heap-rep) 2) 1))))
                   :position-ht (heap-position-ht heap-rep)))))
          (setf
           (aref (heap-actual-heap heap-rep) (heap-size heap-rep))
           (list k v))
          (setf
           (gethash v (heap-position-ht heap-rep))
           (heap-size heap-rep))
          (setf
           (gethash heap-id *heaps*)
           (make-heap
            :id heap-id
            :size (+ (heap-size heap-rep) 1)
            :actual-heap (heap-actual-heap heap-rep)
            :position-ht (heap-position-ht heap-rep)))
          (heapify-up heap-id (heap-size heap-rep))
          t))))

(defun heapify-up (heap-id pos)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap ~S." heap-id))
          ((> pos (heap-size heap-rep))
           (error "Error pos > heap-size."))
          ((equal pos 0) t)
          ((<
            (first (heap-elem heap-id pos))
            (first (heap-elem heap-id (floor (- pos 1) 2))))
           (rotatef
            (aref (heap-actual-heap heap-rep) pos)
            (aref (heap-actual-heap heap-rep) (floor (- pos 1) 2)))
           (rotatef
            (gethash
             (second (heap-elem heap-id pos))
             (heap-position-ht heap-rep))
            (gethash
             (second (heap-elem heap-id (floor (- pos 1) 2)))
             (heap-position-ht heap-rep)))
           (heapify-up heap-id (floor (- pos 1) 2)))
          (t t))))

(defun heap-extract (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap ~S." heap-id))
          ((heap-empty heap-id)
           (error "Heap ~S is empty." heap-id))
          (t (cond ((<
                     (heap-size heap-rep)
                     (floor (length (heap-actual-heap heap-rep)) 2))
                    (setf
                     (gethash heap-id *heaps*)
                     (make-heap
                      :id heap-id
                      :size (heap-size heap-rep)
                      :actual-heap (adjust-array
                                    (heap-actual-heap heap-rep)
                                    ;;array is adjusted by ~ -1/4 if half empty
                                    (list
                                     (+ (heap-size heap-rep)
                                        (floor (heap-size heap-rep) 2))))
                      :position-ht (heap-position-ht heap-rep)))))
             (let ((extracted (heap-head heap-id)))
               (setf
                (aref (heap-actual-heap heap-rep) 0)
                (heap-elem heap-id (- (heap-size heap-rep) 1)))
               (setf
                (gethash
                 (second (heap-elem heap-id 0))
                 (heap-position-ht heap-rep))
                0)
               (setf
                (aref (heap-actual-heap heap-rep) (- (heap-size heap-rep) 1))
                nil)
               (setf
                (gethash heap-id *heaps*)
                (make-heap
                 :id heap-id
                 :size (- (heap-size heap-rep) 1)
                 :actual-heap (heap-actual-heap heap-rep)
                 :position-ht (heap-position-ht heap-rep)))
               (heapify-down heap-id 0)
               (remhash
                (second extracted)
                (heap-position-ht heap-rep))
               extracted)))))


(defun heapify-down (heap-id pos)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap ~S." heap-id))
          ((>= (+ (* pos 2) 1) (heap-size heap-rep)) t)
          ((and (not (null(heap-elem heap-id (+ (* pos 2) 2))))
                (<
                 (first (heap-elem heap-id (+ (* pos 2) 2)))
                 (first (heap-elem heap-id (+ (* pos 2) 1))))
                (>
                 (first (heap-elem heap-id pos))
                 (first (heap-elem heap-id (+ (* pos 2) 2)))))
           (rotatef
            (aref (heap-actual-heap heap-rep) pos)
            (aref (heap-actual-heap heap-rep) (+ (* pos 2) 2)))
           (rotatef
            (gethash
             (second (heap-elem heap-id pos))
             (heap-position-ht heap-rep))
            (gethash
             (second (heap-elem heap-id (+ (* pos 2) 2)))
             (heap-position-ht heap-rep)))
           (heapify-down heap-id (+ (* pos 2) 2)))
          ((>
            (first (heap-elem heap-id pos))
            (first (heap-elem heap-id (+ (* pos 2) 1))))
           (rotatef
            (aref (heap-actual-heap heap-rep) pos)
            (aref (heap-actual-heap heap-rep) (+ (* pos 2) 1)))
           (rotatef
            (gethash
             (second (heap-elem heap-id pos))
             (heap-position-ht heap-rep))
            (gethash
             (second (heap-elem heap-id (+ (* pos 2) 1)))
             (heap-position-ht heap-rep)))
           (heapify-down heap-id (+ (* pos 2) 1)))
          (t t))))

(defun heap-modify-key (heap-id new-key old-key v)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap ~S." heap-id))
          ((or (not (numberp new-key))
               (not (numberp old-key))
               (> new-key old-key)
               (< new-key 0)
               (null (gethash v (heap-position-ht heap-rep))))
           nil)
          (t
           (setf
            (aref
             (heap-actual-heap heap-rep)
             (gethash v (heap-position-ht heap-rep)))
            (list new-key v))
           (heapify-up heap-id (gethash v (heap-position-ht heap-rep)))
           t))))

(defun heap-print (heap-id)
  (let ((heap-rep (gethash heap-id *heaps*)))
    (cond ((null heap-rep)
           (error "Invalid heap ~S." heap-id))
          ((heap-empty heap-id) (format t "Heap empty."))
          (t (format t "~A~%" (heap-actual-heap heap-rep))
          (values)))))

(defun mst-vertex-key (graph-id vertex-id)
  (cond ((null (is-graph graph-id))
         (error "Invalid graph ~S." graph-id))
        ((not (gethash (list graph-id vertex-id) *vertex-keys*))
         (error "No vertex_key found for ~S in graph ~S." vertex-id graph-id))
        (t (let ((vkey (gethash (list graph-id vertex-id) *vertex-keys*)))
             vkey))))

(defun mst-previous (graph-id vertex-id)
  (cond ((null (is-graph graph-id))
         (error "Invalid graph ~S." graph-id))
        (t (let ((prev (gethash (list graph-id vertex-id) *previous*)))
           prev))))

(defun mst-prim (graph-id source)
  (cond ((null (is-graph graph-id))
         (error "Invalid graph ~S." graph-id))
        ((null (gethash (list graph-id source) *vertices*))
         (error "No vertex ~S in graph ~S." source graph-id))
        ((null (gethash source (gethash graph-id *arcs*)))
         (error "Vertex ~S in graph ~S is not connected." source graph-id))
        (t (maphash #'(lambda (k v)
                        (declare (ignore v))
                        (if (equal graph-id (first k))
                            (remhash k *vertex-keys*)))
                    *vertex-keys*)
           (maphash #'(lambda (k v)
                        (declare (ignore v))
                        (if (equal graph-id (first k))
                            (remhash k *previous*)))
                    *previous*)
           (heap-delete graph-id)
           (let ((vlist (graph-vertices graph-id)))
             (new-heap graph-id (length vlist))
             (heap-insert graph-id -1 source)
             (initialize-heap graph-id source vlist)
             (mst-prim-sub graph-id)))))

(defun mst-prim-sub (graph-id)
  (cond ((or (heap-empty graph-id)
             (equal (first (heap-head graph-id)) MOST-POSITIVE-DOUBLE-FLOAT))
         nil)
        (t (let ((extracted (second (heap-extract graph-id))))
             (update-keys
              graph-id
              extracted
              (graph-vertex-adjacent graph-id extracted))
             (mst-prim-sub graph-id)))))

(defun initialize-heap (heap-id source vs)
  (cond ((null vs) t)
        ((not (equal (third (first vs)) source))
         (heap-insert heap-id MOST-POSITIVE-DOUBLE-FLOAT (third (first vs)))
         (setf
          (gethash (list heap-id (third (first vs))) *vertex-keys*)
          MOST-POSITIVE-DOUBLE-FLOAT)
         (initialize-heap heap-id source (rest vs)))
        (t (initialize-heap heap-id source (rest vs)))))

(defun update-keys (heap-id parent vs)
  (let ((child (third (first vs)))
        (tail (rest vs))
        (heap-rep (gethash heap-id *heaps*)))
    (cond ((null vs) t)
          ((and
            (gethash child (heap-position-ht heap-rep))
            (gethash
             child
             (second (gethash parent (gethash heap-id *arcs*))))
            (<
             (second
              (gethash child (second
                              (gethash parent (gethash heap-id *arcs*)))))
             (mst-vertex-key heap-id child)))
           (setf
            (gethash (list heap-id child) *vertex-keys*)
            (second
             (gethash parent (second
                              (gethash child (gethash heap-id *arcs*))))))
           (setf (gethash (list heap-id child) *previous*) parent)
           (heap-modify-key heap-id
                            (second
                             (gethash child (second
                                             (gethash
                                              parent
                                              (gethash heap-id *arcs*)))))
                            MOST-POSITIVE-DOUBLE-FLOAT
                            child)
           (update-keys heap-id parent tail))
          (t (update-keys heap-id parent tail)))))


(defun mst-get (graph-id source)
  (flatten-arc-list (mst-get-nested graph-id source)))

(defun flatten-arc-list (l)
  (cond ((null l) nil)
        ((equal (first (first l)) 'arc)
         (cons (first l) (flatten-arc-list (rest l))))
        (t
         (append
          (flatten-arc-list (first l))
          (flatten-arc-list (rest l))))))

(defun mst-get-nested (graph-id source)
  (mst-prim graph-id source)
  (cond ((null (is-graph graph-id))
         (error "Invalid graph ~S." graph-id))
        ((null (gethash (list graph-id source) *vertices*))
         (error "No vertex ~S in graph ~S." source graph-id))
        (t
         (let ((vlist ()) (nested ()))
           (maphash #'(lambda (k v)
                        (declare (ignore k))
                        (cond ((equal
                                (mst-previous graph-id (first v))
                                source)
                               (push
                                (list 'arc graph-id source (first v) (second v))
                                vlist))))
                    (second (gethash source (gethash graph-id *arcs*))))
           (if (not (null vlist))
               (push (mst-child-launch graph-id (sort vlist #'arc<)) nested))
           (first nested)))))

(defun mst-child-launch (graph-id vlist)
  (let ((arclist ())(head (first vlist)))
    (cond ((null (is-graph graph-id))
           (error "Invalid graph ~S." graph-id))
          ((null vlist) nil)
          ((mst-previous graph-id (fourth head))
           (let ((mst-child (mst-child-launch graph-id (rest vlist)))
                 (mst-nested (mst-get-nested graph-id (fourth head))))
             (if (not (null mst-child)) (push mst-child arclist))
             (if (not (null mst-nested)) (push mst-nested arclist)))
           (push
            (list 'arc graph-id (third head) (fourth head) (fifth head))
            arclist))
          (t
           (let ((mst-child (mst-child-launch graph-id (rest vlist))))
             (if (not (null mst-child)) (push mst-child arclist)))
           (push
            (list 'arc graph-id (third head) (fourth head) (fifth head))
            arclist)))
    arclist))

(defun arc< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (fifth a) (fifth b))
         (let ((child-a (fourth a)) (child-b (fourth b)))
           (cond  ((and (numberp child-a) (null (numberp child-b))) t)
                  ((and (null (numberp child-a)) (numberp child-b)) nil)
                  ((and (numberp child-a) (numberp child-b)) (< child-a child-b))
                  (t (string< child-a child-b)))))
        (t (< (fifth a) (fifth b)))))

;;;; end of file -- mst.lisp --
