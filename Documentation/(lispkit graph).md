# LispKit Graph

Library `(lispkit graph)` provides a simple API for representing, manipulating, and reasoning about directed graphs.

Graphs are mutable objects encapsulating _nodes_ and _edges_. Since graphs organize nodes internally in a hashtable, each graph requires an equivalence and hash function upon creation.

Here is an example for creating and initializing a directed graph with library `(lispkit graph)`. Graph `g` consists of the nodes {1, 2, 3, 4, 5, 6, 7, 8} and the edges {1 &#8594; 2, 2 &#8594; 1, 2 &#8594; 4, 3 &#8594; 4, 4 &#8594; 5, 5 &#8594; 2, 5 &#8594; 8, 6 &#8594; 7, 7 &#8594; 6, 7 &#8594; 8, 2 &#8594; 8}.

```scheme
(define g (graph
            identity           ; node hash function
            =                  ; node equality function
            '(1 2 3 4 5 6 7 8) ; the nodes
            '((1 2)(2 1)(2 4)  ; the edges
              (3 4)(4 5)(5 2)
              (5 8)(6 7)(7 6)
              (7 8)(2 8))))
```

This is the graph that is implemented by this code:

![Graph g](x-devonthink-item://9B24E00D-A40F-45FA-8E4B-D19FFB0DD91D)

The following lines illustrate some of the features of library `(lispkit graph)`:

```scheme
(graph-out-degree g 2)      ⇒ 3
(graph-has-edge? g 3 1)     ⇒ #f
(graph-neighbors g 2)       ⇒ (8 4 1)
(graph-reachable? g 3 1)    ⇒ #t
(graph-reachable-nodes g 3) ⇒ (1 2 8 5 4 3)
```

There are also a few advanced algorithms for directed graphs implemented by the library:

```scheme
(graph-weakly-connected-components g)
⇒ ((1 2 6 8 7 5 4 3))  
(graph-strongly-connected-components g)
⇒ ((3) (5 2 4 1) (7 6) (8))
(graph-shortest-path g 3 8)
⇒ (3 4 5 8)
  3.0
```


## Constructors

**(make-graph _hash equiv_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates and returns a new empty graph object using _hash_ as the hash function and _equiv_ as the equivalence function for nodes. 

**(make-eq-graph)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new empty graph object using `eq-hash` as the hash function and `eq?` as the equivalence function for nodes.

**(make-eqv-graph)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new empty graph object using `eqv-hash` as the hash function and `eqv?` as the equivalence function for nodes.

**(make-equal-graph)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new empty graph object using `equal-hash` as the hash function and `equal?` as the equivalence function for nodes.

**(graph _hash equiv nodes edges_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates and returns a new graph object using _hash_ as the hash function and _equiv_ as the equivalence function for nodes. _nodes_ is a list of all the graph's initial nodes, and _edges_ is a list of all the initial edges of the graph. Each edge is represented as a list of the form (_from_ _to_) or (_from_ _to_ . _label_) where _from_ and _to_ are nodes, and _label_ is an arbitrary Lisp object used as a label annotation.

```scheme
(define g0
  (graph identity =
         '(1 2 3)
         '((1 2 . "one") (1 3 . "two"))))
(graph-neighbors+labels g0 1)
⇒ ((3 . "two") (2 . "one"))
```

**(eq-graph _nodes edges_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates and returns a new graph object using `eq-hash` as the hash function and `eq?` as the equivalence function for nodes. _nodes_ is a list of all the graph's initial nodes, and _edges_ is a list of all the initial edges of the graph. Each edge is represented as a list of the form (_from_ _to_) or (_from_ _to_ . _label_) where _from_ and _to_ are nodes, and _label_ is an arbitrary Lisp object used as a label annotation.

```scheme
(define g (eq-graph '(1 2 3) '((1 2) (1 3))))
(graph-neighbors g 1)
⇒ (3 2)
```

**(eqv-graph _nodes edges_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates and returns a new graph object using `eqv-hash` as the hash function and `eqv?` as the equivalence function for nodes. _nodes_ is a list of all the graph's initial nodes, and _edges_ is a list of all the initial edges of the graph. Each edge is represented as a list of the form (_from_ _to_) or (_from_ _to_ . _label_) where _from_ and _to_ are nodes, and _label_ is an arbitrary Lisp object used as a label annotation.

```scheme
(define g (eqv-graph '(1 2 3) '((1 2) (2 3) (1 3))))
(graph-edges g)
⇒ ((1 2) (1 3) (2 3))
```

**(equal-graph _nodes edges_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Creates and returns a new graph object using `equal-hash` as the hash function and `equal?` as the equivalence function for nodes. _nodes_ is a list of all the graph's initial nodes, and _edges_ is a list of all the initial edges of the graph. Each edge is represented as a list of the form (_from_ _to_) or (_from_ _to_ . _label_) where _from_ and _to_ are nodes, and _label_ is an arbitrary Lisp object used as a label annotation.

```scheme
(define g (equal-graph '(1 2 3) '((1 2) (1 3 . 9.8))))
(graph-neighbors+labels g 1)
⇒ ((3 . 9.8) (2 . #f))
```

**graph-type-tag** <span style="float:right;text-align:rigth;">[constant]</span>  

Symbol representing the `graph` type. The `type-for` procedure of library `(lispkit type)` returns this symbol for all graph objects.


## Predicates

**(graph? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a graph.

**(eq-graph? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a graph using `eq-hash` as hash function and `eq?` as equivalence function for nodes.

**(eqv-graph? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a graph using `eqv-hash` as hash function and `eqv?` as equivalence function for nodes.

**(equal-graph? _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _obj_ is a graph using `eq-hash` as hash function and `eq?` as equivalence function for nodes.

**(graph-empty? _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _graph_ is an empty graph, i.e. a graph without nodes and edges.

**(graph-cyclic? _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _graph_ is a cyclic graph, i.e. a graph which has at least one node with a path to itself.

```scheme
(define g
  (eq-graph '(1 2 3 4) '((1 2)(2 3)(3 4))))
(graph-cyclic? g)       ⇒ #f
(graph-add-edge! g 4 2)
(graph-cyclic? g)       ⇒ #t
```


## Introspection

**(node-equivalence-function _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the node equivalence function used by _graph_.

**(node-hash-function _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the node hash function used by _graph_.

**(graph-has-node? _graph node_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if _graph_ contains _node_; otherwise `#f` is returned.

**(graph-nodes _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all nodes of _graph_.

**(graph-has-edge? _graph edge_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(graph-has-edge? _graph from to_)**  

Returns `#t` if _edge_ is contained in _graph_, `#f` otherwise. _edge_ is a list with at least two elements: the first element is the starting node, the second element is the end node. Alternatively, it is possible to provide the start and end node explicitly as parameters _from_ and _to_.

**(graph-edges _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all edges of _graph_. Each edge is represented as a list of the form (_from_ _to_) or (_from_ _to_ . _label_) where _from_ and _to_ are nodes, and _label_ is an arbitrary Lisp object representing the edge label.

```scheme
(define g
  (eq-graph '(1 2 3) '((1 2 . "a") (1 3))))
(graph-edges g)
⇒  ((1 2 . "a") (1 3))
```

**(graph-edge-label _graph from to_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the label for the edge from node _from_ to node _to_. If there is no label associated with the edge, `#f` is returned. It is an error when `graph-edge-label` gets invoked for an edge that does not exist.

**(graph-in-degree _graph node_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of edges that lead into _node_ in _graph_. It is an error if _node_ is not contained in _graph_.

**(graph-in-degree _graph node_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of edges that lead into _node_ in _graph_. It is an error if _node_ is not contained in _graph_.

**(graph-out-degree _graph node_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of edges that originate in _node_ within _graph_. It is an error if _node_ is not contained in _graph_.

**(graph-neighbors _graph from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns the number of edges that originate in _node_ within _graph_. It is an error if _node_ is not contained in _graph_.

**(graph-neighbors _graph from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of neighbors of node _from_ in _graph_. A neighbor `n` is a node for which there is an edge originating in _from_ and leading to `n`. `graph-neighbors` returns `#f` if _from_ is not a node of _graph_.

**(graph-neighbors+labels _graph from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of pairs, consisting of neighbors with associated labels of node _from_ in _graph_. A neighbor `n` is a node for which there is an edge originating in _from_ and leading to `n`. The associated label is the label of this edge of `#f` if it does not exist. `graph-neighbors+labels` returns `#f` if _from_ is not a node of _graph_.

```scheme
(define g
  (eq-graph '(1 2 3) '((1 2 . "a") (1 3))))
(graph-neighbors+labels g 1)
⇒  ((3 . #f) (2 . "a"))
```


## Mutation

**(graph-add-node! _graph node_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Adds _node_ to _graph_. It is an error if the comparison and hash functions associated with _graph_ are incompatible with _node_.

**(graph-remove-node! _graph node_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Removes _node_ from _graph_ if it contains _node_, and does nothing otherwise. It is an error if the comparison and hash functions associated with _graph_ are incompatible with _node_.

**(graph-add-edge! _graph edge_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(graph-add-edge! _graph edge_)**  
**(graph-add-edge! _graph from to label_)**  

Adds _edge_ to _graph_. _edge_ is represented as a list of the form (_from_ _to_) or (_from_ _to_ . _label_) where _from_ and _to_ are nodes, and _label_ is an arbitrary Lisp object used as a label annotation.

**(graph-remove-edge! _graph edge_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(graph-remove-edge! _graph from to_)**  
**(graph-remove-edge! _graph from to label_)**  

Removes _edge_ from _graph_. _edge_ is represented as a list of the form (_from_ _to_) or (_from_ _to_ . _label_) where _from_ and _to_ are nodes, and _label_ is an arbitrary Lisp object used as a label annotation. The label given in _edge_ does not need to match the label of that edge in _graph_ for the edge to be removed.


## Transformation

**(graph-copy _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(graph-copy _graph rnodes_)**  

Returns a copy of _graph_ that only includes nodes from list _rnodes_; i.e. _rnodes_ acts as a node filter. Edges that either originate or lead into nodes that are not contained in _rnodes_ will not be copied over.

**(graph-transpose _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a copy of _graph_ with all edges reversed, i.e. start and end nodes swapped.

```scheme
(define g
  (eq-graph '(1 2 3 4) '((1 2 . "a") (1 3) (4 1))))
(graph-edges (graph-transpose g))
⇒  ((3 1) (1 4) (2 1 . "a"))
```

**(graph-complement _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a new graph containing all possible edges that have not been contained in _graph_. The new graph does not have any edge labels.

```scheme
(define g
  (eq-graph '(1 2 3) '((1 2 . "a") (1 3))))
(graph-edges (graph-complement g))
⇒  ((3 2) (3 1) (3 3) (1 1) (2 2) (2 1) (2 3))
```


## Processing graphs

**(graph-fold-nodes _f z graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This is the fundamental iterator for graph nodes. Applies the procedure _f_ across all nodes of _graph_ using initial state value _z_. That is, if _graph_ is empty, the procedure returns _z_. Otherwise, some node _n_ of _graph_ is chosen; let _g'_ be the remaining graph without node _n_. `graph-fold-nodes` returns `(graph-fold-nodes f (f n z) g')`.

**(graph-fold-edges _f z graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This is the fundamental iterator for graph edges. Applies the procedure _f_ across all edges of _graph_ using initial state value _z_. That is, if _graph_ is empty, the procedure returns _z_. Otherwise, some edge of _graph_ is chosen with start node _s_, end node _e_ and label _l_; let _g'_ be the remaining graph without this edge. `graph-fold-edges` returns `(graph-fold-edges f (f s e l z) g')`.

**(graph-reachable? _graph from to_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns `#t` if node `to` is reachable from node _from_, i.e. there is a path/sequence of edges for getting from node _from_ to node _to_. Otherwise, `#f` is returned.

**(graph-reachable-nodes _graph from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(graph-reachable-nodes _graph from limit_)**  

Returns a list of all nodes that are reachable from node _from_ within _graph_. These are nodes for which there is a path/sequence of edges starting from node _from_.  _limit_ specifies the maximum number of edges in the paths to explore. 

**(graph-topological-sort _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of nodes of _graph_ that are topologically sorted. A topological sort of a directed graph is a linear ordering of its nodes such that for every directed edge from node _u_ to node _v_, _u_ comes before _v_ in the ordering. `graph-topological-sort` returns `#f` if _graph_ is cyclic. In this case, it is not possible to sort all nodes topologically.

**(graph-weakly-connected-components _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all weakly connected components of _graph_. Each component is represented as a list of nodes. A weakly connected component is a subgraph of _graph_ where all nodes are connected to each other by some path, ignoring the direction of edges.

**(graph-strongly-connected-components _graph_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a list of all strongly connected components of _graph_. Each component is represented as a list of nodes. A strongly connected component is a subgraph of _graph_ where all nodes are connected to each other by some path.

**(graph-shortest-path _graph from to_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(graph-shortest-path _graph from to distance_)**  

Returns a shortest path from node _from_ to node _to_ in _graph_. _distance_ is a distance function taking a starting and ending node as arguments. By default _distance_ returns 1.0 for all edges. A path is represented as a list of nodes.

**(graph-shortest-paths _graph from_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(graph-shortest-paths _graph from distance_)**  

Returns all the shortest paths from node _from_ to node _to_ in _graph_. _distance_ is a distance function taking a starting and ending node as arguments. By default _distance_ returns 1.0 for all edges. Paths are represented as lists of nodes.
