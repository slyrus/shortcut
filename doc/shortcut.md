# Shortcut

## Introduction

Shortcut is a library for working with graphs, that is data structures
referring to nodes and edges. Currently shortcut only handles
undirected graphs. The clojure.contrib.graph package only works with
directed graphs, and I needed to represent undirected graphs, hence
shortcut.

Shortcut is a functional library for graphs in which the data is
represented by immutable data structures. The consequence of this is
that when you "add a node" to a graph, you don't, strictly speaking,
modify the existing graph such that it has an additional node, but
rather you create a new graph which contains all of the original nodes
and a new node. This functional style may take some getting used to at
first, but it is consistent with the general design principles
embodied in clojure. Ok, you might be thinking: "isn't this horribly
inefficient?" Well, it turns out that clojure was designed for exactly
this sort of thing and has a number of efficient primitive operations
to support this design. Ok, and now you might be thinking "but that's
insane; how do I add a whole bunch of nodes and edges to a graph?"
Well, the general idea is to either use recursive functions or, better
yet, use the reduce function which allows one to pass an argument
between successive iterations of calling a given function. This allows
one to do things like:

    (reduce add-node (make-graph)
            ["these" "strings" "will" "all" "each" "be" "nodes"])

# Using Shortcut

To use shortcut from your clojure
application/repl/programming-environment of choice, make sure that
either the shortcut jar or source directory is on your classpath and
execute the following:

    (use 'shortcut.graph)

To create a graph, one can use the make-graph function:

    user=> (make-graph)
    {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{}}

Of course a graph with no nodes and no edges, isn't particulary
interesting, so let's create a graph with some nodes:

    user=> (make-graph #{1 2 3 4} {})
    {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{1 2 3 4}}

Now we see that we've created a graph, represented by a map with two
entries, one for the nodes and one for the edges. Note that the nodes
can be arbitrary objects, just as long as one can apply the = function
to them and get the same result. They can be integers, strings,
objects, functions, etc...:

Unlike nodes, edges are represented by explicit edge objects that
contain information about which nodes are connected by the edge. More
on this later.

To create a graph with nodes and edges, pass a set of nodes and a set
of two-element vectors for the edges, each containing the two nodes to
be connected by an edge:

    user> (make-graph #{1 2 3 4} #{[1 2] [1 3] [3 4]})
    {:shortcut.graph/edge-map {1 {3 [1 3], 2 [1 2]}, 2 {1 [1 2]}, 3 {1 [1 3], 4 [3 4]}, 4 {3 [3 4]}}, :shortcut.graph/node-set #{1 2 3 4}}


## Immutable Graphs

In keeping with the general design of clojure, graphs in shortcut are
immutable. If one wants to add a node to a graph, one makes an
entirely new graph, with the nodes of the original graph and the new
node added. To see an example of this, first lets make a graph and store it in a var:

    user> (def q (make-graph #{1 2 3 4} {}))
    #'user/q

To make sure that we did this right, let's check the nodes of q:

    user> (nodes q)
    #{1 2 3 4}

So far so good. Now let's an edge between nodes 1 and 2:

    user> (add-edge q 1 2)
    {:shortcut.graph/edge-map {1 {2 [1 2]}, 2 {1 [1 2]}}, :shortcut.graph/node-set #{1 2 3 4}}

Now we see that we have the ndoe set #{1 2 3 4} as before, but we also
have the edge [1 2], stored twice in the map of edges of this graph,
once coming from the first node, 1, and once from the second node, 2.

However, notice that we didn't change q:

    user> q
    {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{1 2 3 4}}

Similarly, we can add a node to q:

    user> (add-node q 5)
    {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{1 2 3 4 5}}

But again, note that we didn't change q:

    user> q
    {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{1 2 3 4}}

If we want to add multiple nodes and edges, we can just call add-node or add-edge as appropriate to the results of the previous add-node or add-edge call:

    user> (add-edge (add-edge q 3 1) 1 4)
    {:shortcut.graph/edge-map {4 {1 [1 4]}, 3 {1 [3 1]}, 1 {4 [1 4], 3 [3 1]}}, :shortcut.graph/node-set #{1 2 3 4}}

While this may seem like more work than creating a mutable graph and
adding nodes or edges to it, the clojure internals are designed for
handling just this sort of immutable operation such that the
operations for creating a new graph with additional nodes or edges (or
fewer nodes or edges) are relatively inexpensive.

So instead of adding nodes or edges to an existing graph, we're
creating new graphs with more or fewer nodes and edges. Of course
we're probably going to want to use the resulting new graph, either by
calling a function with it, bind it locally to a symbol via let, or
assign it to a var with def:

    user> (def q2 (add-edge (add-edge q 3 1) 1 4))
    #'user/q2

To remove a node from a graph, or, more precisely, to create a new
graph containing the nodes and edges of the original graph, but for
the node to be removed and any edges containing that node, call
remove-node:

    user> (remove-node q 2) 
    {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{1 3 4}}

Note that edges containing the node are removed as well:

    user> (remove-node (add-edge q 1 2) 2) 
    {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{1 3 4}}

To remove an edge, use the remove-edge function:

    user> (remove-edge q2 1 3)
    {:shortcut.graph/edge-map {4 {1 [1 4]}, 1 {4 [1 4]}}, :shortcut.graph/node-set #{1 2 3 4}}

To make a graph with 1000 nodes where the nodes are the integers from 0 to 999, do:

    user> (def q6 (reduce #(add-node %1 %2) (make-graph) (range 1000)))
    #'user/q6
    user> (count (nodes q6))
    1000

To create a graph 10,000 randomly chosen edges and the nodes 0-999 as above:

    user> (def q7 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
                    q6
                    (take 10000 (repeatedly #(vector (rand-int 1000)
                                                   (rand-int 1000))))))
    #'user/q7


## Neighbors

We can use the neighbors function to find the nodes in a graph that are connected by a single edge from a given node:

    user> (neighbors q7 1)
    (416 544 868 681 76 13 79 591 815 81 945 563 188 700 796 317 639 895)

Recall that in the graph q7, defined above, we had 1000 nodes, with
10000 randomly created edges. Since the edges are undirected, that
means we should expect about 20 edges (and, therefore, 20 neighbors),
on average, connecting each node. In this case we see 18 neighbors:
pretty close. We can see how many neighbors the first 50 nodes of q7
have as follows:

    user> (map #(count (neighbors q7 %)) (range 50))
    (17 18 17 19 20 25 25 27 19 22 14 20 24 13 17 23 14 13 23 24 19 19 18 12 16 18 17 20 27 18 21 23 17 26 24 21 20 23 18 25 21 21 29 10 23 23 17 13 23 17)
    
    user> (/ (apply + (map #(count (neighbors q7 %)) (range 50))) 50.0)
    19.86

And we see that, on average, the first 50 nodes of q7 have 19.86
neighbors, as we would expect. In fact, the way this example is
designed, we would expect slightly less than 20 nodes as the 10,000
randomly chosen edges may include some duplicates and duplicate edges
ignored, leaving a single edge between the given nodes. More on
handling of duplicate edges later.


## Traversing Graphs

Now that we've got a routine for identifying the neighbors of a given node in a graph, we can start to think about how we might traverse the group. Two logical ways are breadth-first traversal and depth-first-traversal. But before getting into the details of these, let's start with a more managable, but still large enough to be interesting, randomly chosen graph:

    user> (def q8 (reduce #(add-node %1 %2) (make-graph) (range 100)))
    
    #'user/q8
    
    user> (def q9 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
                    q8
                    (take 1000 (repeatedly #(vector (rand-int 100)
                                                     (rand-int 100))))))
    #'user/q9

Now let's do a breadth-first traversal from node 0:

    user> (breadth-first-traversal q9 0)
    (0 97 98 39 73 44 76 77 79 24 56 91 92 62 65 99 6 38 7 71 40 41 10 42 75 12 45 47 49 58 61 68 19 51 52 54 86 55 87 25 29 93 31 32 3 35 8 48 80 82 22 90 94 33 67 13 16 50 83 26 60 5 9 78 81 84 85 28 63 69 14 23 2 36 43 53 88 57 89 27 30 4 74 46 96 66 72 17 18 95 64 15 34 21 1 20 37 70 11 59)

And we'll see that we get different results from doing a depth-first-search:

    user> (depth-first-traversal q9 0)
    (0 62 95 91 92 63 94 31 61 89 88 29 28 58 56 60 25 90 57 17 30 27 55 84 59 32 24 93 81 85 54 23 48 21 46 86 53 22 79 51 50 18 87 52 83 20 19 16 26 10 47 80 49 15 78 45 77 14 76 38 13 12 75 74 70 72 41 40 43 1 8 44 82 42 73 71 97 7 6 69 9 35 39 3 67 11 5 37 36 99 4 68 98 66 34 65 2 96 33 64)

To see what's going on here, let's try a simpler example:

    user> (def q3 (make-graph #{1 2 3 4 5} #{[1 2] [1 3] [3 4] [2 5]}))
    #'user/q3
    
    user> (breadth-first-traversal q3 1)
    (1 3 2 4 5)
    
    user> (depth-first-traversal q3 1)
    (1 2 5 3 4)

The order in which sibling nodes is unspecified, but notice that in
the breadth-first example, nodes 3 and 2 are both searched before 4
and 5, while in the depth-first example node 5 is searched before 3
and 4. Of course it would have been just as valid for 3 and 4 to have
been searched before 2 and 5 in this example.

Often one not only wants to traverse a graph, but also to keep track
of the paths during the traversal. This can be accomplished with
breadth-first-traversal-with-path and depth-first-traversal-with-path:

    user> (breadth-first-traversal-with-path q3 1)
    ([1] [1 3] [1 2] [1 3 4] [1 2 5])
    
    user> (depth-first-traversal-with-path q3 1)
    ([1] [1 2] [1 2 5] [1 3] [1 3 4])

## Finding Nodes

To find a given node in a graph, use the find-node function:

    user> (def q8 (reduce #(add-node %1 (str "node" %2)) (make-graph) (range 1000)))
    #'user/q8
    user> (def q9 (reduce (fn [g [n1 n2]]
                      (add-edge g (str "node" n1) (str "node" n2)))
                    q8
                    (take 10000 (repeatedly #(vector (rand-int 1000)
                                                     (rand-int 1000))))))
    #'user/q9
    user> (find-node q9 "node512")
    ["node42" "node383" "node512"]
    user> (find-node q9 "node512" "node12")
    ["node12" "node347" "node512"]

Find does a breadth-first-search on graph and returns the path to the
given node, if found. In contrast, breadth-first-traversal returns all
the nodes reachable from the starting node in breadth-first
order. Note that find-node takes an optional start argument so one can
find the path from a given node to another.

## Connected Components

To find the connected components of a graph, use the connected-components function. Connected components returns a sequence of graphs, each containing a connected component of the original graph.

    user> (def q10 (add-edges (make-graph (set (range 1 10)))
                        [[1 2] [1 3] [3 4] [4 5] [2 6] [7 8] [8 9] [7 9]]))
    #'user/q10
    user> (map clojure.pprint/pprint
         (map #(vector %
                       (graph-distance-matrix %))
              (connected-components q10)))
    ([{:shortcut.graph/edge-map
      {8 {9 [8 9], 7 [7 8]}, 7 {8 [7 8], 9 [7 9]}, 9 {8 [8 9], 7 [7 9]}},
      :shortcut.graph/node-set #{7 8 9}}
     [[0, 1, 1], [1, 0, 1], [1, 1, 0]]]
    [{:shortcut.graph/edge-map
      {6 {2 [2 6]},
       5 {4 [4 5]},
       4 {5 [4 5], 3 [3 4]},
       2 {6 [2 6], 1 [1 2]},
       1 {2 [1 2], 3 [1 3]},
       3 {4 [3 4], 1 [1 3]}},
      :shortcut.graph/node-set #{1 2 3 4 5 6}}
     [[0, 1, 1, 2, 3, 2], [1, 0, 2, 3, 4, 1], [1, 2, 0, 1, 2, 3],
      [2, 3, 1, 0, 1, 4], [3, 4, 2, 1, 0, 5], [2, 1, 3, 4, 5, 0]]]
    nil nil)

To retrieve a graph with a single connected component from a give graph, use the connected-component function:

    user> (connected-component q10 9)
    {:shortcut.graph/edge-map {8 {7 [7 8], 9 [8 9]}, 7 {8 [7 8], 9 [7 9]}, 9 {8 [8 9], 7 [7 9]}}, :shortcut.graph/node-set #{7 8 9}}

To retrieve a graph with a single connected component removed, use the remove-connected-component function:

    user> (remove-connected-component q10 9)
    {:shortcut.graph/edge-map {6 {2 [2 6]}, 5 {4 [4 5]}, 4 {5 [4 5], 3 [3 4]}, 3 {4 [3 4], 1 [1 3]}, 1 {3 [1 3], 2 [1 2]}, 2 {6 [2 6], 1 [1 2]}}, :shortcut.graph/node-set #{1 2 3 4 5 6}}

To get both a single connected from a graph and the rest of the graph with the given component removed, use the partition-graph function:

    user> (clojure.pprint/pprint (partition-graph q10 9))
    [{:shortcut.graph/edge-map
      {8 {7 [7 8], 9 [8 9]}, 7 {8 [7 8], 9 [7 9]}, 9 {8 [8 9], 7 [7 9]}},
      :shortcut.graph/node-set #{7 8 9}}
     {:shortcut.graph/edge-map
      {6 {2 [2 6]},
       5 {4 [4 5]},
       4 {5 [4 5], 3 [3 4]},
       3 {4 [3 4], 1 [1 3]},
       1 {3 [1 3], 2 [1 2]},
       2 {6 [2 6], 1 [1 2]}},
      :shortcut.graph/node-set #{1 2 3 4 5 6}}]
    nil

## Distance Matrix

To compute the distance, that is the lenght of the shortest path,
between any two nodes, use the graph-distance-matrix function, which
returns a Java 2D array:

    user> (graph-distance-matrix q10)
    #<int[][] [[I@4a5ecdd2>

    user> (clojure.pprint/pprint (graph-distance-matrix q10))
    [[0, 1, 1, 2, 3, 2, -1, -1, -1], [1, 0, 2, 3, 4, 1, -1, -1, -1],
     [1, 2, 0, 1, 2, 3, -1, -1, -1], [2, 3, 1, 0, 1, 4, -1, -1, -1],
     [3, 4, 2, 1, 0, 5, -1, -1, -1], [2, 1, 3, 4, 5, 0, -1, -1, -1],
     [-1, -1, -1, -1, -1, -1, 0, 1, 1], [-1, -1, -1, -1, -1, -1, 1, 0, 1],
     [-1, -1, -1, -1, -1, -1, 1, 1, 0]]
    nil

Note that the distance between two unreachable nodes is arbitrarily
set to -1 and the distance between a node and itself is 0, whether or
not there is an explicit self-edge. (Note that self edges aren't
particularly well thought out at this point.)

## Shortcut Internals

Currently, two protocols are used to define the behavior supported by
objects in graphs: NodeSet and EdgeSet. More documentation to follow.

