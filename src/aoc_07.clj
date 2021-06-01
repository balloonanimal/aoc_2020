(ns aoc-07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample-input
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn name->kw [name]
  (keyword (str/join "-" (str/split name #"\s"))))

(defn parse-constraint [constraint]
  (if (= constraint "no other bags")
    {}
    (let [[_ num name] (re-matches #"(\d+) (\w+ \w+) bags?" constraint)]
      [(name->kw name) (Integer/parseInt num)])))

(defn parse-line [line]
  (let [[bag & rest] (str/split line #"( contain |, |\.)")
        [_ bag-name] (re-matches #"(\w+ \w+) bags" bag)]
    {(name->kw bag-name) (into {} (map parse-constraint) rest)}))

(defn input->map [input]
  (into {} (map parse-line) (str/split-lines input)))

;; (defn add-maps [maps]
;;   (reduce #(merge-with + %1 %2) {} maps))

;; (defn map-vals [f coll]
;;   (into {} (map (fn [[k v]] [k (f v)])) coll))

;; (defn reducing-fn
;;   ([] {})
;;   ([m [k v]] (update m k (fn [x] (if x (+ x v) v)))))

;; (defn process-children [[k v]]
;;   )


(defn flatten-bags [constraints]
  (let [chase-constraints
        (memoize
         (fn chase-constraints [k]
           "returns the number of bags each bag contains (including itself)"
           (reduce (partial merge-with +)
                   {k 1}
                   (map (fn [[k v]] (map-vals #(* v %) (chase-constraints k)))
                        (constraints k)))))]
    (into {} (map (fn [[k _]] [k (chase-constraints k)])) constraints)))

(->> (input->map sample-input)
     flatten-bags
     (filter (fn [[_ v]] (:shiny-gold v)))
     count
     dec)
; => 4
; sample 1

(def input (slurp (io/resource "input_07.txt")))

(->> (input->map input)
     flatten-bags
     (filter (fn [[_ v]] (:shiny-gold v)))
     count
     dec)
; => 252
; part 1

(defn total-bags [constraints]
  (let [chase-constraints
        (memoize
         (fn chase-constraints [k]
           (let [contents (constraints k)]
             (if (seq contents)
               (reduce + (map (fn [[k v]] (* v (inc (chase-constraints k)))) (constraints k)))
               0))))]
    (into {} (map (fn [[k _]] [k (chase-constraints k)])) constraints)))

(->> (input->map sample-input)
     total-bags
     :shiny-gold)
; => 32
; sample 2

(->> (input->map input)
     total-bags
     :shiny-gold)
; => 35487
; part 2
