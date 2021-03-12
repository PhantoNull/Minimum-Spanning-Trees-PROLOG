# Minumum Spanning Tree in Lisp
![graph-image.jpg](graph-image.jpg?raw=true)
## Introduction

 A problem that often appears in various guises is connecting
  different "points" in an "equivalent" way, for example connecting them
  with threads but without creating loops.

 Another typical problem is to calculate the shortest route in a map
  point to point.

 There are several algorithms capable of solving these known problems
  as the "Minimum Spanning Tree".

 The purpose of this project is to implement **Prim's algorithm** for
  the solution of the MST problem for non-directed and connected graphs
  with non-negative weights.

 To proceed with the implementation of these algorithms it is necessary
  produce an implementation of a MINHEAP (or MIN-PRIORITY-QUEUE).
 *(We couldn't use default library for heap data structure and had to produce
  it ourselves)*

  My solution to the problem strives to maximize memory and time efficiency
    and at the same time fulfill API's assignment specifications.

 By avoiding storing arcs in hash-tables with key *(graph-id v1 v2)* and as
  value its *Weight* I tried to find a more efficient way to store data.

 Arcs data in my program are stored in nested-hash-tables structures
  to drastically improve efficiency in searching operations.
 In addition to this it also avoids increasing execution times caused
  by multiple graphs existing simultaneously.

 The downside to this implementation is that a "double" representation for
  arcs is inevitable.
 Moreover a temporary hash-table is needed to avoid double-arc
  output in functions graph-arcs and graph-print.

 *graphs* hash-table is optional since all graphs are also stored in first
  "layer" as entry-point keys for the nested hash-tables structure in *arcs*.
  I kept it just in case it was needed for unit-tests for the uni project.


# Instructions
This program outputs a Minimum Spanning tree for a given graph.

### [ Creating and editing a graph ]
  Before any operation can be done you need to have a graph to work on.
  You can create a graph by adding arcs with function:
```
    CL > (new-arc graph-name vertex1 vertex2 weight)
```
  vertex1 and vertex2 must be atoms. Weight must be numeric.

  If not existent vertices and graph will be created along with arc.
  Weight is optional and will be 1 if not specified.
  For more control over graphs creation you can use functions new_graph
   and new_vertex.
  As new_arc, new_vertex will create the graph if doesn't exists.
```
    CL > (new-graph graph-name)

    CL > (new-vertex graph-name vertex-name)
```


### [ Printing a graph ]
  To debug a state of any graph you may print it:
```
    CL > (graph-print graph-name)
```


### [ Calculating Mst ]
  MST will be given as a list of arcs obtained by pre-order visiting the
   MST with radix source (vertex-name) and sorted first by weight and then
   by lexicographical order for nodes on same depth.
  Source is the starting vertex of computation.
```
   CL > (mst-prim graph-name source)
        (mst-get graph-name source)
```


### [ Memory Management ]
   When a graph is no more needed, you may delete it with function:
```
   CL > (delete-graph graph-name)
```
   This will delete delete everything related to the graph in memory.


### [ More info for a given mst ]
   If you want more hierarchical informations on a MST arc-list you can get the not-flattened
    arc list with mst-get-nested function.
   It returns a pre-ordered list of nested arcs representing the MST.
```
   CL > (mst-get-nested graph-name source)
```


## Credits
OA: https://github.com/PhantoNa

Originally made for an university project, feel free to use.
