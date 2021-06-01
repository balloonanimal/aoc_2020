(ns aoc-05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-input
  "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL")

(defn parse-line [s]
  (let [[_ row-seq col-seq] (re-find #"^([BF]{7})([LR]{3})$" s)]
    [(map #(= % \B) row-seq)
     (map #(= % \R) col-seq)]))

(defn binsearch-seq [splits]
  (loop [sum 0
         stepsize (Math/round (Math/pow 2 (dec (count splits))))
         splits splits]
    (if (seq splits)
      (recur (if (first splits) (+ sum stepsize) sum)
             (/ stepsize 2)
             (rest splits))
      sum)))

(defn row-and-col [[row-splits col-splits]]
  [(binsearch-seq row-splits) (binsearch-seq col-splits)])

(defn seat-id [[row col]]
  (+ (* 8 row) col))

(defn seat-id-for-line [line]
  (-> line
      parse-line
      row-and-col
      seat-id))

(def input (slurp (io/resource "input_05.txt")))

(->> (str/split-lines input)
     (map seat-id-for-line)
     (reduce max))
; => 816
; part 1

(->> (str/split-lines input)
     (map seat-id-for-line)
     (sort <)
     (partition 3 1)
     (filter (fn [[a b c]]
               (or (not= (inc a) b)
                   (not= (inc b) c))))
     ((fn [[[_ b _] _]] (inc b))))
; => 539
; part 2
