(ns day22
  (:require [clojure.string :as s]))

(def sample "resources/day22-sample.txt")
(def input "resources/day22.txt")

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n\n"))
       (map (fn [s]
              (->> s
                   (#(s/split % #"\n"))
                   rest
                   (map read-string)
                   vec)))
       ((fn [[v1 v2]]
         {:p1 v1
          :p2 v2}))))

(defn play-round [{[p1 & p1-rest] :p1 [p2 & p2-rest] :p2}]
  (if (> p1 p2)
    {:p1 (conj (vec p1-rest) p1 p2) :p2 p2-rest}
    {:p1 p1-rest :p2 (conj (vec p2-rest) p2 p1)}))

(defn play-game [m]
  (loop [{:keys [p1 p2] :as game} m]
    (cond
      (empty? p1) {:winner :p2 :deck p2}
      (empty? p2) {:winner :p1 :deck p1}
      :else (recur (play-round game)))))

(->> input
     parse-input
     play-game
     :deck
     reverse
     (map-indexed (fn [idx item] (* item (inc idx))))
     (reduce +))
