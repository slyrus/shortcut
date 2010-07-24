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

;;; Arcs aren't supported yet. keep the protocol and some methods on it
;;; here for the moment though.
(defprotocol Arc
  (start [edge])
  (end [edge]))

(extend-protocol Arc
  clojure.lang.PersistentVector
  (start [v] (first v))
  (end [v] (second v)))

(extend-protocol NodeSet
  clojure.lang.IPersistentMap
  (nodes [v] (::node-set v))
  (node? [v node] (get (nodes v) node))
  (add-node [g n]
            (assoc g ::node-set (conj (::node-set g) n)))
  (remove-node [v node]
               (let [edges-removed (reduce (fn [v [n1 n2]]
                                             (remove-edge v n1 n2))
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
               (let [[n1 n2] edge]
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
                  (apply remove-edge g edge))
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

(defn make-graph
  ([] (assoc {} ::node-set #{} ::edge-map {}))
  ([nodes] (assoc {} ::node-set nodes ::edge-map{}))
  ([nodes edge-vec] (add-edges (make-graph nodes) edge-vec)))

(defn breadth-first-traversal
  ([g start]
     (when (node? g start)
       (breadth-first-traversal g (conj (clojure.lang.PersistentQueue/EMPTY) start) #{})))
  ([g queue visited]
     (lazy-seq
      (if (peek queue)
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
      (if (peek queue)
        (let [path (peek queue)
              node (last path)
              next (remove visited (neighbors g node))]
          (cons path (breadth-first-traversal-with-path
                       g
                       (into (pop queue)
                             (vec (map #(conj path %) (vec next))))
                       (into (conj visited node) next))))))))

(defn depth-first-traversal
  ([g start]
     (when (node? g start)
       (depth-first-traversal g (list start) #{})))
  ([g queue visited]
     (lazy-seq
      (if (seq queue)
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
      (if (seq queue)
        (let [node (last (first queue))
              next (remove visited (neighbors g node))]
          (if-not (visited node)
            (cons (first queue)
                  (depth-first-traversal-with-path g
                    (into (rest queue)
                          (vec (map #(conj (first queue) %) (vec next))))
                    (conj visited node)))
            (depth-first-traversal-with-path g
                        (into (rest queue)
                              (vec (map #(conj (first queue) %) (vec next))))
                        visited)))))))

(defn remove-connected-component [g start]
  (reduce remove-connected-component
          (remove-node g start)
          (neighbors g start)))

(defn connected-component [old start]
  (letfn [(connected-component-2
           [[old new] node]
           (reduce connected-component-2
                   (let [[old2 new2]
                         (reduce (fn [[old new] edge]
                                   [(remove-edge old edge)
                                    (add-edge new edge)])
                                 [old new]
                                 (edges old node))]
                     [(remove-node old2 node)
                      (add-node new2 node)])
                   (neighbors old node)))]
    (second (connected-component-2 [old (make-graph)] start))))

