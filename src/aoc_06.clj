(ns aoc-06
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def sample-input
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn split-input [input]
  (map str/split-lines (str/split input #"\R\R")))

(defn yes-count [party]
  (count (reduce into #{} party)))

(->> (split-input sample-input)
     (map yes-count)
     (reduce +))
; => 11
; sample 1

(def input (slurp (io/resource "input_06.txt")))
(->> (split-input input)
     (map yes-count)
     (reduce +))
; => 6590
; part 1

(defn all-yes-count [party]
  (->> party
       (map set)
       (reduce set/intersection)
       count))

(->> (split-input sample-input)
     (map all-yes-count)
     (reduce +))
; => 6
; sample 2
(->> (split-input input)
     (map all-yes-count)
     (reduce +))
; => 3288
; part 2
