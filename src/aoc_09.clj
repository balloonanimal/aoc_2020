(ns aoc-09
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")
(def input (slurp (io/resource "input_09.txt")))

(defn input->vec [input]
  (mapv #(Long/parseLong %) (str/split-lines input)))

;; (defn subtract-maps [a b]
;;   (reduce
;;    (fn [coll [k v]]
;;      (let [new-v (- (get coll k) v)]
;;        (assert (>= new-v 0) "Tried to subtract more than was present in the map")
;;        (cond
;;          (= new-v 0) (dissoc coll k)
;;          (> new-v 0) (assoc coll k new-v))))
;;    a b))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll] (reduce conj (queue) coll)))

(let [n 5
      coll (input->vec sample-input)
      pop-sum-queue
      (fn [sum-q]
        [(into [] (map (fn [[n v]] [n (subvec v 1)])) (rest sum-q))
         (into [] (map first) (rest sum-q))])
      conj-sum-queue
      (fn [q x]
        (let [[new-q to-remove] (if (>= (count sum-q) n)
                                     (pop-sum-queue sum-q)
                                     [sum-q []])
              sums (into [] (map (fn [[y _]] (+ x y))) popped-q)
              new-q (conj queue [x sums])]
          [new-q to-remove]))
      ;; queue (queue)
      reducing-fn
      (fn [{:keys [queue invalids]} x]
        {:queue (push-to-queue x)
         :invalids (if (some (fn [[_ sums]] (sums x)) queue)
                     invalids
                     (conj invalids x))})]
  (reduce reducing-fn starting-queue ))

((fn [q] (into [] (map (fn [[n v]] [n (subvec v 1)])) (rest q)))
 [[1 []] [2 [3]] [3 [4 5]] [4 [5 6 7]] [5 [6 7 8 9]]])
