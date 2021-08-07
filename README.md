# Minumum Spanning Trees in Prolog
![graph-title.png](img/graph-title.png?raw=true)
## Introduction

 A problem that often appears in various guises is connecting
  different "points" in an "equivalent" way, for example connecting them
  with threads but without creating loops.

 Another typical problem is to calculate the shortest route in a map
  point to point.

 There are several algorithms capable of solving these known problems
  as the "Mininum Spanning Tree".

 The purpose of this project is to implement **Prim's algorithm** for
  the solution of the MST problem for non-directed and connected graphs
  with non-negative weights.

 To proceed with the implementation of these algorithms it is necessary
  produce an implementation of a MINHEAP (or MIN-PRIORITY-QUEUE).


 My solution to the problem ***strives to maximize memory and time efficiency
  while trying to offer a solid structure of predicates.***

 An example of this effort are predicates as *graph_arcs*, *graph_vertices* or *adjs*.
 To grant their robustness these predicates evaluate permutations of lists
  passed as arguments.
 To maximize efficiency permutation are taken into account only when
  lists are passed as non-var.


# Instructions
This program outputs a Minimum Spanning Tree (or more) for a given graph.
Works with unconnected graphs.

### [ Loading a graph ]
  Before any operation can be done you need to have a graph to work on.
  You can load a graph in memory by providing a .csv file with tab
  separator. Each row represent an arc:
    vertex1 vertex2 weight

  An [example of .csv graph file](graphs/graph_example.csv) is loaded in the repo.

  ![example-graph.png](img/example-graph.png?raw=true)

  Another randomly generated graph ([prim_benchmark.csv](graphs/prim_benchmark.csv)) is available for efficiency benchmarks.

  Alternatively it is possible to create and edit a graph manually
  through graph structure's predicates ***new_graph***, ***new_vertex*** & ***new_arc***.
  In this case, pre-existing conditions should be met for vertices or arcs
   to be declared. (Es. a graph should exist before declaring his vertex)

  If an arc is already present in knowledgebase, the weight will be simply
   updated with the last value given.

  Load a graph from file in interpreter by using read_graph predicate:
  ```Prolog
    read_graph(G, 'FilePath.csv').
  ```


### [ Saving a graph ]
  You may want to save a graph loaded in memory or after making changes to it.
  It is possible to save a graph to a file in two ways:

  1. **PASSING AN EXISTING GRAPH**  *(graph mode)*
  ```Prolog
     write_graph(graphname, 'FilePath.csv', 'graph').
  ```
  *Note: 3rd parameter is optional and 'graph' is its default value.*

  2. **PASSING A LIST OF ARCS** AS [arc(G, U, V, W), ...]  *(edge mode)*
  ```Prolog
     write_graph(LIST, 'FilePath.csv', 'edge').
  ```

  The file will be created in the enviroment working directory if absolute
   path is not provided.


### [ Calculating MST ]
  MST will be given as a list of arcs obtained by pre-order visiting the
   MST with radix Source (vertexname) and sorted first by weight and then
   by lexicographical order for nodes on same depth with a common parent.
  Source is the starting vertex of computation.
  An example of normal system interrogation may be:
```Prolog
   mst_get(graph_name, source, LIST).
```
  It is also possible to obtain all the possible mst from all loaded graphs
  in memory starting from different vertices by passing only variables:
```Prolog
   mst_get(G, S, LIST).
```
   It is possible to combine VAR and NONVAR parameter as wished to obtain
    any kind of combinational results.


### [ Memory management ]
  When a graph is no more needed, you may delete it with predicate:
```Prolog
   delete_graph(graphname).
```
  This will delete delete everything related to the graph in memory.


### [ HINT: Consulting long mst arc list ]
  Consulting a long list of arcs in the interpreter may be problematic.
  If you want to have access to a more readable format you may save
   mst_get and mst_get_nested result list to a .csv file:
```Prolog
   mst_get(g, v, L), write_graph(L, 'preorder.csv', 'edge').
```


## Credits
OA: https://github.com/PhantoNull

Feel free to use under MIT licensing.
