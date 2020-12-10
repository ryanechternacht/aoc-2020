(ns day06
  (:require [clojure.string :as s]))

(def sample "resources/day06-sample.txt")
(def input "resources/day06.txt")

(defn split-on-blank-lines
  "takes a seq of strings in which some lines are blank. breaks the seq into sublists
   on blank lines. the sublists are concatted into a single line (separated by an additional space)"
  [coll]
  (let [lines
        (reduce (fn [{:keys [current] :as acc} line]
                  (if (s/blank? line)
                    (-> acc
                        (update :items conj (s/trim current))
                        (assoc :current ""))
                    (update acc :current str " " line)))
                {:items [] :current ""}
                coll)]
    (if (s/blank? (:current lines))
      (:items lines)
      (conj (:items lines) (s/trim (:current lines))))))

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       split-on-blank-lines))

(defn line-to-set [line]
  (->> line
       (#(s/split % #" "))
       (reduce into #{})))

(defn count-answers [line]
  (-> line
      frequencies
      (update \space #(if (= % nil) 1 (inc %)))))

(->> input
     parse-input
     (map line-to-set)
     (map count)
     (reduce +))

(defn count-totals [m]
  (let [total (m \space)]
    (->> m
         (filter (fn [[_ v]] (= total v)))
         count
         dec)))

(->> input
     parse-input
     (map count-answers)
     (map count-totals)
     (reduce +))
