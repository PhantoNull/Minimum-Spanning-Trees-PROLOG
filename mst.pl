% -*- Mode: Prolog -*-

%%%% mst.lips
%%%  OA: https://github.com/PhantoNull
%%%  Originally made for an university project

:- use_module(library(csv)).

%%%% RESET PREVIOUS DATA IF PRESENT
:- retractall(graph(_)).
:- retractall(vertex(_, _)).
:- retractall(arc(_, _, _, _)).
:- retractall(vertex_previous(_, _, _)).
:- retractall(vertex_key(_, _, _)).

%%%% GRAPH PREDICATES
:- dynamic (graph/1).
:- dynamic (vertex/2).
:- dynamic (arc/4).

new_graph(G) :-
    nonvar(G),
    graph(G),
    !.

new_graph(G) :-
    nonvar(G),
    assert(graph(G)).

delete_graph(G) :-
    nonvar(G),
    graph(G),
    retract(graph(G)),
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retractall(vertex_previous(G, _, _)),
    retractall(vertex_key(G, _, _)).

new_vertex(G, V) :-
    nonvar(G),
    nonvar(V),
    vertex(G, V),
    !.

new_vertex(G, V) :-
    nonvar(G),
    nonvar(V),
    graph(G),
    assert(vertex(G, V)).

graph_vertices(G, Vs):-
    var(Vs),
    !,
    graph(G),
    findall(vertex(G, V), vertex(G, V), Vs).

graph_vertices(G, Vs):-
    graph(G),
    findall(vertex(G, V), vertex(G, V), FVs),
    permutation(Vs, FVs).

new_arc(G, U, V) :-
    new_arc(G, U, V, 1),
    !.

new_arc(_, V, V, _) :- !.

new_arc(G, U, V, W) :-
    (arc(G, U, V, W) ; arc(G, V, U, W)),
    !.

new_arc(G, U, V, W) :-
    number(W),
    W >= 0,
    (arc(G, U, V, W2) ; arc(G, V, U, W2)),
    W2 \= W,
    (retract(arc(G, U, V, W2)) ; retract(arc(G, V, U, W2))),
    new_arc(G, U, V, W),
    !.

new_arc(G, U, V, W) :-
    number(W),
    W >= 0,
    nonvar(G),
    nonvar(U),
    nonvar(V),
    graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, W)).

graph_arcs(G, Es) :-
    var(Es),
    !,
    graph(G),
    findall(arc(G, U, V, W), arc(G, U, V, W), Es).

graph_arcs(G, Es) :-
    graph(G),
    findall(arc(G, U, V, W), arc(G, U, V, W), FEs),
    permutation(Es, FEs).


vertex_neighbors(G, V, Ns):-
    var(Ns),
    !,
    vertex(G, V),
    findall(arc(G, V, U, W), arc(G, V, U, W), Xs),
    findall(arc(G, V, U, W), arc(G, U, V, W), Ys),
    append(Xs, Ys, Ns).

vertex_neighbors(G, V, Ns):-
    vertex(G, V),
    findall(arc(G, V, U, W), arc(G, V, U, W), Xs),
    findall(arc(G, V, U, W), arc(G, U, V, W), Ys),
    append(Xs, Ys, ENs),
    permutation(Ns, ENs).

adjs(G, V, Vs) :-
    var(Vs),
    !,
    vertex(G, V),
    findall(vertex(G, U), arc(G, V, U, _), Xs),
    findall(vertex(G, U), arc(G, U, V, _), Ys),
    append(Xs, Ys, Vs).

adjs(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, U), arc(G, V, U, _), Xs),
    findall(vertex(G, U), arc(G, U, V, _), Ys),
    append(Xs, Ys, FVs),
    permutation(Vs, FVs).


list_vertices(G):-
    graph(G),
    listing(vertex(G, _)).

list_arcs(G) :-
    graph(G),
    listing(arc(G, _U, _V, _W)).

list_graph(G) :-
    list_vertices(G),
    list_arcs(G).

read_graph(G, FileName) :-
    nonvar(G),
    nonvar(FileName),
    new_graph(G),
    delete_graph(G),
    new_graph(G),
    csv_read_file(FileName, Data, [functor(arc), arity(3), separator(0'\t)]),
    load_graph(G, Data).

load_graph(_, []) :- !.

load_graph(G, [A | As]) :-
    nonvar(G),
    nonvar(A),
    nonvar(As),
    arg(1, A, U),
    arg(2, A, V),
    arg(3, A, Weight),
    new_vertex(G, U),
    new_vertex(G, V),
    new_arc(G, U, V, Weight),
    load_graph(G, As).

write_graph(G, FileName) :-
    write_graph(G, FileName, 'graph'),
    !.

write_graph(G, FileName, Type) :-
    nonvar(G),
    nonvar(FileName),
    nonvar(Type),
    Type = 'graph',
    findall(arc(U, V, Weight), arc(G, U, V, Weight), As),
    csv_write_file(FileName, As, [functor(arc), arity(3), separator(0'\t)]),
    !.

write_graph(G, FileName, Type) :-
    nonvar(G),
    nonvar(FileName),
    nonvar(Type),
    Type = 'edge',
    remove_graph_name(G, GFix),
    csv_write_file(FileName, GFix, [functor(arc), arity(3), separator(0'\t)]).

remove_graph_name([], []) :-
    !.
remove_graph_name([G | Gs], [GFix | GFs]) :-
    nonvar(G),
    nonvar(Gs),
    nonvar(GFix),
    nonvar(GFs),
    arg(2, G, U),
    arg(3, G, V),
    arg(4, G, W),
    GFix = arc(U, V, W),
    remove_graph_name(Gs, GFs).

%%%% MIN-HEAP PREDICATES
:- dynamic (heap/2).
:- dynamic (heap_entry/4).

new_heap(H) :-
    nonvar(H),
    heap(H, _S),
    !.

new_heap(H) :-
    nonvar(H),
    assert(heap(H, 0)).

delete_heap(H) :-
    nonvar(H),
    heap(H, _),
    !,
    retractall(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).

heap_has_size(H, S) :-
    heap(H, S),
    !.

heap_empty(H) :-
    heap_has_size(H, 0),
    !.

heap_not_empty(H) :-
    heap_has_size(H, S),
    S > 0,
    !.

heap_head(H, K, V) :-
    heap(H, _S),
    heap_entry(H, 1, K, V),
    !.

heap_insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap_has_size(H, P),
    PNew is P + 1,
    assert(heap_entry(H, PNew, K, V)),
    retract(heap(H, P)),
    assert(heap(H, PNew)),
    heapify_up(H, PNew).

heap_extract(H, K, V) :-
    nonvar(H),
    heap_head(H, K, V),
    heap_has_size(H, S),
    retract(heap_entry(H, S, KL, VL)),
    retractall(heap_entry(H, 1, K, V)),
    assert(heap_entry(H, 1, KL, VL)),
    PNew is S - 1,
    retract(heap(H, S)),
    assert(heap(H, PNew)),
    heapify_down(H, 1).

heapify_up(_H, 1) :- !.

heapify_up(H, P) :-
    heap_entry(H, P, K, V),
    P > 1,
    Par is div(P, 2),
    heap_entry(H, Par, KPar, VPar),
    K < KPar,
    !,
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, Par, K, V)),
    retract(heap_entry(H, Par, KPar, VPar)),
    assert(heap_entry(H, P, KPar, VPar)),
    heapify_up(H, Par).

heapify_up(_H, _P) :- true.

heapify_down(H, P) :-
    heap_entry(H, P, K, V),
    L is P * 2,
    R is P * 2 + 1,
    heap_entry(H, L, Kl, _Vl),
    heap_entry(H, R, Kr, Vr),
    Kr < Kl,
    Kr < K,
    !,
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, R, K, V)),
    retract(heap_entry(H, R, Kr, Vr)),
    assert(heap_entry(H, P, Kr, Vr)),
    heapify_down(H, R).

heapify_down(H, P) :-
    heap_entry(H, P, K, V),
    L is P * 2,
    heap_entry(H, L, Kl, Vl),
    Kl < K,
    !,
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, L, K, V)),
    retract(heap_entry(H, L, Kl, Vl)),
    assert(heap_entry(H, P, Kl, Vl)),
    heapify_down(H, L).

heapify_down(_H, _P) :- true.

modify_key(H, NewKey, OldKey, V) :-
    nonvar(H),
    nonvar(NewKey),
    nonvar(OldKey),
    nonvar(V),
    heap(H, _S),
    NewKey < OldKey,
    NewKey >= 0,
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    heapify_up(H, P).

list_heap(H) :-
    listing(heap_entry(H, _P, _K, _V)).

%%%% PRIM & MST PREDICATES
:- dynamic (vertex_key/3).
:- dynamic (vertex_previous/3).

mst_prim(G, Source) :-
    nonvar(G),
    nonvar(Source),
    vertex(G, Source),
    new_heap(G),
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)),
    graph_vertices(G, Vs),
    heap_insert(G, -1, Source),
    initialize_heap(G, Source, Vs),
    mst_prim(G),
    !.

mst_prim(G) :-
    nonvar(G),
    heap_empty(G),
    !,
    delete_heap(G).

mst_prim(G) :-
    nonvar(G),
    heap_head(G, inf, _),
    !,
    delete_heap(G).

mst_prim(G) :-
    nonvar(G),
    heap_extract(G, _K, U),
    adjs(G, U, Vs),
    update_keys(G, U, Vs),
    mst_prim(G).

initialize_heap(_G, _Source, []):- !.
initialize_heap(G, Source, [N | Ns]) :-
    nonvar(G),
    nonvar(Source),
    nonvar(N),
    nonvar(Ns),
    arg(2, N, V),
    V \= Source,
    !,
    heap_insert(G, inf, V),
    assert(vertex_key(G, V, inf)),
    assert(vertex_previous(G, V, nil)),
    initialize_heap(G, Source, Ns).

initialize_heap(G, Source, [_ | Ns]) :-
    initialize_heap(G, Source, Ns).

update_keys(_G, _U, []) :- !.
update_keys(G, U, [N | Ns]) :-
    nonvar(G),
    nonvar(U),
    nonvar(N),
    nonvar(Ns),
    arg(2, N, V),
    heap_entry(G, _, _, V),
    arc(G, U, V, W),
    vertex_key(G, V, KV),
    W < KV,
    !,
    retract(vertex_key(G, V, KV)),
    assert(vertex_key(G, V, W)),
    retract(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)),
    modify_key(G, W, KV, V),
    update_keys(G, U, Ns).

update_keys(G, U, [N | Ns]) :-
    nonvar(G),
    nonvar(U),
    nonvar(N),
    nonvar(Ns),
    arg(2, N, V),
    heap_entry(G, _, _, V),
    arc(G, V, U, W),
    vertex_key(G, V, KV),
    W < KV,
    !,
    retract(vertex_key(G, V, KV)),
    assert(vertex_key(G, V, W)),
    retract(vertex_previous(G, V, _)),
    assert(vertex_previous(G, V, U)),
    modify_key(G, W, KV, V),
    update_keys(G, U, Ns).

update_keys(G, U, [_ | Ns]) :-
    nonvar(G),
    nonvar(U),
    nonvar(Ns),
    update_keys(G, U, Ns).

mst_get(G, Source, PreorderTree):-
    var(PreorderTree),
    !,
    vertex(G, Source),
    mst_prim(G, Source),
    mst_get_nested(G, Source, T),
    flatten(T, PreorderTree).

mst_get(G, Source, PreorderTree):-
    nonvar(PreorderTree),
    PreorderTree = [Arc | _],
    arg(2, Arc, Source),
    vertex(G, Source),
    mst_prim(G, Source),
    mst_get_nested(G, Source, T),
    flatten(T, PreorderTree).

mst_get_nested(G, U, T) :-
    vertex(G, U),
    findall([G, W, V, U], (vertex_previous(G, V, U), vertex_key(G, V, W)), Vs),
    Vs \= [],
    !,
    sort(Vs, Ss),
    mst_child_launch(G, Ss, T).

mst_get_nested(G, U, T) :-
    vertex(G, U),
    vertex_key(G, U, W),
    vertex_previous(G, U, UP),
    T = arc(G, UP, U, W).

mst_child_launch(_G, [], []) :-
    !.
mst_child_launch(G, [S | St], [T, T2 | Ts]) :-
    S = [G, W, V, U],
    vertex_previous(G, _, V),
    !,
    T = arc(G, U, V, W),
    mst_get_nested(G, V, T2),
    mst_child_launch(G, St, Ts).

mst_child_launch(G, [S | St], [T | Ts]) :-
    S = [G, W, V, U],
    T = arc(G, U, V, W),
    mst_child_launch(G, St, Ts).

%%%% end of file -- mst.pl --
