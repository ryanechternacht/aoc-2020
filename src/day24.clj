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

(defn get-neighbors [[x y]]
  [[(inc x) (inc y)]
   [(+ x 2) y]
   [(inc x) (dec y)]
   [(dec x) (dec y)]
   [(- x 2) y]
   [(dec x) (inc y)]])

(defn get-neighbor-counts [positions]
  (reduce (fn [acc pos]
            (reduce (fn [acc2 pos2]
                      (assoc acc2 pos2 (inc (get acc2 pos2 0))))
                    acc
                    (get-neighbors pos)))
          {}
          positions))

(defn step-tiles [tiles]
  (let [neighbor-counts (get-neighbor-counts tiles)
        all-tiles (reduce conj tiles (map first neighbor-counts))]
    (reduce (fn [acc pos]
              (let [c (get neighbor-counts pos 0)]
                (if (contains? tiles pos)
                ;; currently black
                  (if (<= 1 c 2)
                    (conj acc pos)
                    acc)
                ;; currently white
                  (if (= 2 c)
                    (conj acc pos)
                    acc))))
            #{}
            all-tiles)))

(->> input
     parse-input
     flip-tiles
     (#(reduce (fn [acc _]
                 (step-tiles acc))
               %
               (range 100)))
     count)

(step-tiles (flip-tiles (parse-input sample)))
