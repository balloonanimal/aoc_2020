(ns sodoku
  (:require [clojure.set :as set]
            [clojure.test :as test]))

(defn cross [a b] (into #{} (for [a-elem a b-elem b] [a-elem b-elem])))

(def digits "123456789")
(def rows (into [] (comp (map str) (map keyword)) "abcdefghi"))
(def cols (into [] (comp (map str) (map keyword)) digits))
(def squares (into [](for [row rows col cols] [row col])))
(def units
  (let [row-units (into #{} (map (fn [row] (cross [row] cols)) rows))
        col-units (into #{} (map (fn [col] (cross rows [col])) cols))
        block-units (into #{}
                          (for [row-triple [[:a :b :c] [:d :e :f] [:g :h :i]]
                                col-triple [[:1 :2 :3] [:4 :5 :6] [:7 :8 :9]]]
                            (cross row-triple col-triple)))]
    (set/union row-units col-units block-units)))
(def units-of
  (let [units-of-square
        (fn [square] (into #{}(filter #(% square)) units))]
    (into {} (map (fn [square] [square (units-of-square square)]) squares))))
(def peers-of
  (let [remove-square (fn [square set] (disj set square))
        peers-of-square
        (fn [square]
          (->> square
               units-of
               (reduce set/union)
               (remove-square square)))]
    (into {} (map (fn [square] [square (peers-of-square square)]) squares))))

(test/deftest test-init
  (do
    (test/is (= 81 (count squares)))
    (test/is (= 27 (count units)))
    (test/is (every? #(= 3 (count (units-of %))) squares))
    (test/is (every? #(= 20 (count (peers-of %))) squares))
    (test/is (= #{#{[:a :2] [:b :2] [:c :2] [:d :2] [:e :2] [:f :2] [:g :2] [:h :2] [:i :2]}
                  #{[:c :1] [:c :2] [:c :3] [:c :4] [:c :5] [:c :6] [:c :7] [:c :8] [:c :9]}
                  #{[:a :1] [:a :2] [:a :3] [:b :1] [:b :2] [:b :3] [:c :1] [:c :2] [:c :3]}}
                (units-of [:c :2])))
    (test/is (= #{[:a :2] [:b :2] [:d :2] [:e :2] [:f :2] [:g :2] [:h :2] [:i :2]
                  [:c :1] [:c :3] [:c :4] [:c :5] [:c :6] [:c :7] [:c :8] [:c :9]
                  [:a :1] [:a :3] [:b :1] [:b :3]}
                (peers-of [:c :2])))))

(defn parse-grid [grid]
  (let [naive-assign (fn [char]
                       (if (#{\. \0} char)
                         (set digits)
                         #{char}))
        values (->> grid
                    (filter (set/union (set digits) #{\. \0}))
                    (map naive-assign)
                    (zipmap squares))]
    (assert (= (count squares) (count values)) "invalid grid passed")
    values))

(test/deftest test-parse-grid
  (let [sample-grids
        {:flat "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
         :zeros "400000805
030000000
000700000
020000060
000080400
000010000
000603070
500200000
104000000"
         :full "4 . . |. . . |8 . 5
. 3 . |. . . |. . .
. . . |7 . . |. . .
------+------+------
. 2 . |. . . |. 6 .
. . . |. 8 . |4 . .
. . . |. 1 . |. . .
------+------+------
. . . |6 . 3 |. 7 .
5 . . |2 . . |. . .
1 . 4 |. . . |. . ."}])
  (test/is (= (parse-grid (sample-grids :flat))
              (parse-grid (sample-grids :zeros))
              (parse-grid (sample-grids :full)))))


(defn fixed
  ([values]
   (fixed values squares))
  ([values targets]
   (into
    {}
    (comp
     (filter #(= 1 (count (values %))))
     (map #(vector % (values %))))
    targets)))

(let [values (parse-grid (sample-grids :flat))]
  (fixed values))

(test/run-tests)
