
(ns shortcut_scratch
  (:use shortcut.graph))  

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


(defn neighborfn [g]
  (fn [n] (neighbors g n)))

