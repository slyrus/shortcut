
(ns shortcut-scratch
  (:use [shortcut.graph
         :only (make-graph
                nodes add-node add-nodes
                edges add-edge add-edges
                neighbors
                breadth-first-traversal graph-distance-matrix
                connected-components connected-component partition-graph)
         :as graph]))

(def q (make-graph #{1 2 3 4} {}))
(def q2 (add-edge (add-edge q 3 1) 1 4))
(def q3 (add-edges q [[1 2] [1 3] [3 4]]))
(def q4 (add-edges (add-nodes q3 5 6) [[4 5] [1 5] [4 6]]))
(def q5 (add-edges (make-graph #{1 2 3 4 5 6})
                   [[1 2] [2 3] [3 4] [4 5] [5 6] [6 1]]))

(def q6 (reduce #(add-node %1 %2) (make-graph) (range 1000)))
(def q7 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
                q6
                (take 10000 (repeatedly #(vector (rand-int 1000)
                                                 (rand-int 1000))))))
(neighbors q7 1)
(take-while (complement #{2}) (breadth-first-traversal q7 1))

(def q8 (reduce #(add-node %1 (str "node" %2)) (make-graph) (range 1000)))


(def q9 (reduce (fn [g [n1 n2]] (add-edge g
                                          (str "node" n1)
                                          (str "node" n2)))
                q8
                (take 10000 (repeatedly #(vector (rand-int 1000)
                                                 (rand-int 1000))))))

(def q10 (add-edges (make-graph (set (range 1 10)))
                    [[1 2] [1 3] [3 4] [4 5] [2 6] [7 8] [8 9] [7 9]]))

(map clojure.pprint/pprint
     (map #(vector %
                   (graph-distance-matrix %))
          (connected-components q10)))

(defn neighborfn [g]
  (fn [n] (neighbors g n)))


(def q11 (reduce (fn [g [n1 n2]] (add-edge g n1 n2))
                 (reduce #(add-node %1 %2) (make-graph) (range 50))
                 (take 100 (repeatedly #(vector (rand-int 50)
                                                (rand-int 50))))))

;;; this breaks connected components!!! puts a nil in node-set. should not do that...
(def q12 (add-edges (make-graph (set (range 1 3)))
                    [[1 1] [1 2] ]))
(connected-components q12)

;;; well, it's actually partition-graph that's broken:
(partition-graph q12 1)
;;; [{:shortcut.graph/edge-map {1 {2 [1 2]}, 2 {1 [1 2]}}, :shortcut.graph/node-set #{nil 1 2}} {:shortcut.graph/edge-map {}, :shortcut.graph/node-set #{}}]
;;; should be:
;;; [{:shortcut.graph/edge-map {1 {2 [1 2]}, 2 {1 [1 2]}}, :shortcut.graph/node-set #{1 2}}]
;;;
