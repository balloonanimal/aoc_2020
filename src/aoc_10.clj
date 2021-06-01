(ns aoc-10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(defn parse-input [input]
  (mapv #(Integer/parseInt %) (str/split-lines input)))

(defn fast-sort [v]
  (let [a (into-array v)]
    (java.util.Arrays/sort a)
    (vec a)))

(defn solution1 [input]
  (->> (parse-input input)
       fast-sort
       (#(concat [0] % [(+ 3 (last %))]))
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       frequencies
       (#(* (% 3) (% 1)))))

(solution1 sample-input)
; => 220
; sample 1

(def input (slurp (io/resource "input_10.txt")))
(solution1 input)
; => 1690
; part 1

;; (-> (parse-input input)
;;     (#(= (count (set %)) (count %))))
