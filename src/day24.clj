(ns day24
  (:require [clojure.string :as s]))

(def sample "resources/day24-sample.txt")
(def input "resources/day24.txt")

(defn parse-line [line]
  (re-seq #"e|ne|se|sw|nw|w" line))

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       (map parse-line)))

(defn generate-position [directions]
  (let [dir-fn {"ne" (fn [[x y]] [(inc x) (inc y)])
                "e" (fn [[x y]] [(+ x 2) y])
                "se" (fn [[x y]] [(inc x) (dec y)])
                "sw" (fn [[x y]] [(dec x) (dec y)])
                "w" (fn [[x y]] [(- x 2) y])
                "nw" (fn [[x y]] [(dec x) (inc y)])}]
    (reduce (fn [acc dir]
              ((dir-fn dir) acc))
            [0 0]
            directions)))

(defn flip-tiles [directions]
  (reduce (fn [acc dirs]
            (let [pos (generate-position dirs)]
              (if (acc pos)
                (disj acc pos)
                (conj acc pos))))
          #{}
          directions))

(->> input
     parse-input
     flip-tiles
     count)
