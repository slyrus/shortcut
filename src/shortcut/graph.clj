;;; file: shortcut/graph.clj
;;;
;;; Copyright (c) 2010 Cyrus Harmon (ch-lisp@bobobeach.com) All rights
;;; reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns shortcut.graph)

(defprotocol NodeSet
  (nodes [graph])
  (node? [graph node])
  (add-node [graph node])
  (remove-node [graph node])
  (neighbors [graph node]))

(defprotocol EdgeSet
  (edges [graph]
         [graph node])
  (edge? [graph node1 node2])
  (add-edge [graph edge]
            [graph node1 node2]
            [graph node1 node2 m])
  (remove-edge [graph edge]
               [graph node1 node2]))

(extend-protocol NodeSet
  clojure.lang.PersistentVector
  (nodes [v] v)
  (node? [v node] (some #{node} v))
  (neighbors [v node] (remove #{node} v)))

(defprotocol Edge
  (left [edge])
  (right [edge]))

(extend-protocol Edge
  clojure.lang.IPersistentVector
  (left [v] (first v))
  (right [v] (second v)))

;;; Arcs aren't supported yet. keep the protocol and some methods on it
;;; here for the moment though.
(defprotocol Arc
  (start [edge])
  (end [edge]))

(extend-protocol Arc
  clojure.lang.IPersistentVector
  (start [v] (first v))
  (end [v] (second v)))

(extend-protocol NodeSet
  clojure.lang.IPersistentMap
  (nodes [v] (::node-set v))
  (node? [v node] (get (nodes v) node))
  (add-node [g n]
            (assoc g ::node-set (conj (::node-set g) n)))
  (remove-node [v node]
               (let [edges-removed (reduce (fn [v e]
                                             (remove-edge v (left e) (right e)))
                                           v
                                    (edges v node))]
                 (assoc v
                   ::node-set (disj (::node-set edges-removed) node)
                   ::edge-map (::edge-map edges-removed))))
  (neighbors [v node]
             (map #(first (neighbors % node)) (vals (get (::edge-map v) node)))))

(extend-protocol EdgeSet
  clojure.lang.IPersistentMap
  (edges ([g]
            (distinct (apply concat (map vals (vals (::edge-map g))))))
         ([g node]
            (vals (get (::edge-map g) node))))
  (edge? [g n1 n2]
         (some #(when (node? % n2) %)
               (vals (get (::edge-map g) n1))))
  (add-edge ([g edge]
               (let [n1 (left edge) n2 (right edge)]
                 (letfn [(add-1-edge [e n1 n2]
                                     (assoc e n1 (assoc (or (get e n1) {}) n2 edge)))]
                   (if (some #(node? % n2) (edges g n1))
                     g
                     (assoc g ::edge-map
                            (add-1-edge
                             (add-1-edge (::edge-map g) n2 n1)
                             n1 n2))))))
            ([g n1 n2]
                (add-edge g n1 n2 nil))
            ([g n1 n2 meta-data-map]
               (let [obj (if meta-data-map
                           (with-meta [n1 n2] meta-data-map)
                           [n1 n2])]
                 (letfn [(add-1-edge [e n1 n2 obj]
                                     (assoc e n1 (assoc (or (get e n1) {}) n2 obj)))]
                   (if (some #(node? % n2) (edges g n1))
                     g
                     (assoc g ::edge-map
                            (add-1-edge
                             (add-1-edge (::edge-map g) n2 n1 obj)
                             n1 n2 obj)))))))
  (remove-edge ([g edge]
                  (remove-edge g (left edge) (right edge)))
               ([g n1 n2]
                  (letfn [(remove-1-edge [e n1 n2]
                                         (let [inner (dissoc (or (get e n1) {}) n2)]
                                           (if (seq inner)
                                             (assoc e n1 inner)
                                             (dissoc e n1))))]
                    (assoc g ::edge-map
                           (remove-1-edge
                            (remove-1-edge (::edge-map g) n2 n1)
                            n1 n2))))))

(defn add-edges [g edge-vec]
  (reduce (fn [g [n1 n2]] (add-edge g n1 n2)) g edge-vec))

(defn add-nodes [g & node-vec]
  (reduce (fn [g node] (add-node g node)) g node-vec))

(defn get-node [g node]
  (get (nodes g) node))

(defn make-graph
  ([] (assoc {} ::node-set #{} ::edge-map {}))
  ([nodes] (if nodes
             (assoc {} ::node-set nodes ::edge-map {})
             (make-graph)))
  ([nodes edge-vec] (add-edges (make-graph nodes) edge-vec)))

(defn breadth-first-traversal
  ([g start]
     (when (node? g start)
       (breadth-first-traversal g (conj (clojure.lang.PersistentQueue/EMPTY) start) #{})))
  ([g queue visited]
     (lazy-seq
      (when (peek queue)
        (let [node (peek queue)
              next (remove visited (neighbors g node))]
          (cons node
                (breadth-first-traversal g (into (pop queue) next)
                                         (into (conj visited node) next))))))))

;;; often it's nice to not just do a search, but keep a trail of the
;;; path of how one got to a particular node. we're also going to want
;;; to know the distance from the start, so we can just take the
;;; length of the path to get that.
(defn breadth-first-traversal-with-path
  ([g start]
     (when (node? g start)
       (breadth-first-traversal-with-path
         g (conj (clojure.lang.PersistentQueue/EMPTY) [start]) #{})))
  ([g queue visited]
     (lazy-seq
      (when (peek queue)
        (let [path (peek queue)
              node (last path)
              next (remove visited (neighbors g node))]
          (cons path (breadth-first-traversal-with-path
                       g
                       (into (pop queue)
                             (map #(conj path %) next))
                       (into (conj visited node) next))))))))

;;; note: In contrast to breadth-first-traversal above, in this case,
;;; since we're not returning a lazy-seq, it's trivial (?) to use
;;; recur instead of calling find-node recursively.
(defn find-node
  "finds the target node in g, either starting from a given node, or
from an (arbitrarily chosen) first node. If target is unreachable from
the starting node, returns nil."
  ([g target]
     (find-node g target (first (nodes g))))
  ([g target start]
     (when (node? g start)
       (find-node
        g target (conj (clojure.lang.PersistentQueue/EMPTY) [start]) #{})))
  ([g target queue visited]
     (when (peek queue)
       (let [path (peek queue)
             node (last path)
             next (remove visited (neighbors g node))]
         (if (= node target)
           path
           (recur g target (into (pop queue)
                                 (map #(conj path %) next))
                  (into (conj visited node) next)))))))

(defn depth-first-traversal
  ([g start]
     (when (node? g start)
       (depth-first-traversal g (list start) #{})))
  ([g queue visited]
     (lazy-seq
      (when (seq queue)
        (let [node (first queue)
              next (remove visited (neighbors g node))]
          (if-not (visited node)
            (cons node
                  (depth-first-traversal g (into (rest queue) next)
                                         (conj visited node)))
            (depth-first-traversal g (into (rest queue) next) visited)))))))

(defn depth-first-traversal-with-path
  ([g start]
     (when (node? g start)
       (depth-first-traversal-with-path g (list [start]) #{})))
  ([g queue visited]
     (lazy-seq
      (when (seq queue)
        (let [node (last (first queue))
              next (remove visited (neighbors g node))]
          (if-not (visited node)
            (cons (first queue)
                  (depth-first-traversal-with-path g
                    (into (rest queue)
                          (map #(conj (first queue) %) next))
                    (conj visited node)))
            (depth-first-traversal-with-path g
                        (into (rest queue)
                              (map #(conj (first queue) %) next))
                        visited)))))))

(defn remove-connected-component [g start]
  "returns a graph from which the connected component containing start
is removed."
  (reduce remove-connected-component
          (remove-node g start)
          (neighbors g start)))

(defn partition-graph [graph start]
  "partition graph returns a 2-element vector. the first element is a
graph of the connected component of graph contating start and the
second element is the a graph containing the rest of graph, that is
graph after removing the connected component containing start."
  (letfn [(connected-component*
           [[new old] node]
           (if node
             (reduce connected-component*
                     (let [[new2 old2]
                           (reduce (fn [[new old] edge]
                                     [(add-edge new edge)
                                      (remove-edge old edge)])
                                   [new old]
                                   (edges old node))]
                       [(add-node new2 node)
                        (remove-node old2 node)])
                     (neighbors old node))
             [new old]))]
    (connected-component* [(make-graph) graph] start)))

(defn connected-component [graph start]
  "Returns a graph corresponding the connected component of graph that
contains start."
  (first (partition-graph graph start)))

(defn connected-components [graph]
  "Returns a sequence of the connected components of g."
  (letfn [(connected-components*
           [graph acc]
           (if (empty? (nodes graph))
             acc
             (let [[part rest] (partition-graph graph (first (nodes graph)))]
               (when (nodes rest)
                 (recur rest (conj acc part))))))]
    (connected-components* graph nil)))

(defn find-longest-paths [g]
  "Finds a longest path acecssible from each node in the
graph. Returns a sequence of vectors containing the longest shortest paths for each
node. Each vector represents the path to the farthest node from the
first node of the vector to the last node of the vector."
  (map (fn [start]
         (first
          (reverse
           (sort-by count
                    (breadth-first-traversal-with-path g start)))))
       (nodes g)))


(defn graph-distance-hash [g]
  "Return a hashmap with entries for each node containing 2-element
arrays of the nodes in g and the distances (shortest path) from the
first node to the second node"
  (reduce (fn [m node]
            (conj m
                  {node
                   (map (fn [path]
                          [(last path) (dec (count path))])
                        (breadth-first-traversal-with-path g node))}))
          {}
          (nodes g)))

(defn position [coll x]
  (some (fn [[a b]]
          (when (= b x) a))
        (map vector (iterate inc 0) coll)))

(defn graph-distance-matrix [g]
  "Returns a 2-d array of the distance (shortest path) between two
nodes. The distance between a node and itself is 0. If a node is
unreachable from another node, the distance between the nodes is -1."
  (let [hash (graph-distance-hash g)
        size (count (nodes g))]
    (let [a (make-array (. Integer TYPE) size size)
          nodes (nodes g)]
      (dotimes [i size]
        (dotimes [j size]
          (aset a i j -1)))
      (doseq [outer nodes]
        (doseq [[inner distance] (get hash outer)]
          (let [outindex (position nodes outer)
                inindex (position nodes inner)]
            (aset a outindex inindex distance))))
      a)))

(defn find-cycle 
  ([g]
     (reduce #(or %1 %2)
             (map #(find-cycle % (first (nodes %)))
                  (connected-components g))))
  ([g start]
     (reduce #(or %1 %2)
             (map (fn [neighbor]
                    (find-node (remove-edge g start neighbor) neighbor start))
                  (neighbors g start)))))

(defn graph-empty? [g]
  "returns true if there are no nodes in g."
  (empty? (nodes g)))

