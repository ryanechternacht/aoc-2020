(ns day01
  (:require [clojure.string :as s]))

(def file "resources/day01.txt")

(defn read-lines [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       (map read-string)))

(let [nums (read-lines file)
      pairs (for [i nums
                  j nums]
              [i j])]
  (->> pairs
       (filter (fn [[i j]] (> i j))) ;; de-dup list and remove when we match a number with itself
       (filter (fn [[i j]] (= 2020 (+ i j))))
       first
       (apply *)))

(let [nums (read-lines file)
      trips (for [i nums
                  j nums
                  k nums]
      [i j k])]
      (->> trips
           (filter (fn [[i j k]] (> i j k)))
           (filter (fn [[i j k]] (= 2020 (+ i j k))))
           first
           (apply *)))
