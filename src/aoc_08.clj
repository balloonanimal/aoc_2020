(ns aoc-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-instruction [s]
  (let [[instruction n] (str/split s #"\s")]
    [(keyword instruction) (Integer/parseInt n)]))

(defn input->instructions [input]
  (->> (str/split-lines input)
       (map parse-instruction)))

(defn run-instructions [instructions]
  (loop [ptr 0 acc 0 visited #{}]
    (if (visited ptr)
      acc
      (let [[instruction val] (nth instructions ptr)]
        (condp = instruction
          :acc (recur (inc ptr) (+ acc val) (conj visited ptr))
          :jmp (recur (+ ptr val) acc (conj visited ptr))
          :nop (recur (inc ptr) acc (conj visited ptr))
          :else (throw (Exception. "unknown instruction type")))))))

(run-instructions (input->instructions sample-input))
                                        ; => 5
                                        ; sample 1

(def input (slurp (io/resource "input_08.txt")))
(run-instructions (input->instructions input))
                                        ; => 2003
                                        ; part 1


;; uggo eww (refactor)
(defn search
  ([instructions]
   (search instructions 0 0 #{} false))
  ([instructions ptr acc visited altered]
   (println ptr acc visited altered)
   (cond
     (visited ptr) nil
     (= ptr (count instructions)) acc
     :else
     (let [[instruction val] (nth instructions ptr)]
       (condp = instruction
         :acc (search instructions (inc ptr) (+ acc val) (conj visited ptr) altered)
         :jmp (or (search instructions (+ ptr val) acc (conj visited ptr) altered)
                  (when-not altered
                    (search instructions (inc ptr) acc (conj visited ptr) true)))
         :nop (or (search instructions (inc ptr) acc (conj visited ptr) altered)
                  (when-not altered
                    (search instructions (+ ptr val) acc (conj visited ptr) true)))
         :else (throw (Exception. "unknown instruction type")))))))

(search (input->instructions sample-input))
; => 8
; sample 2

(search (input->instructions input))
; => 1984
; input 2
