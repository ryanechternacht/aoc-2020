(ns day6
  (:require [clojure.set :as s]))

(def sample "resources/day6-sample.txt")
(def input "resources/day6.txt")

(defn split-on-blank-lines
  "takes a seq of strings in which some lines are blank. breaks the seq into sublists
   on blank lines. the sublists are concatted into a single line (separated by an additional space)"
  [coll]
  (let [lines
        (reduce (fn [{:keys [current] :as acc} line]
                  (if (clojure.string/blank? line)
                    (-> acc
                        (update :items conj (clojure.string/trim current))
                        (assoc :current ""))
                    (update acc :current str " " line)))
                {:items [] :current ""}
                coll)]
    (if (clojure.string/blank? (:current lines))
      (:items lines)
      (conj (:items lines) (clojure.string/trim (:current lines))))))

(defn parse-input [file]
  (->> file
       slurp
       (#(clojure.string/split % #"\n"))
       split-on-blank-lines))

(defn line-to-set [line]
  (->> line
       (#(clojure.string/split % #" "))
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
