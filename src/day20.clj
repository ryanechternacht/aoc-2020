(ns day20
  (:require [clojure.string :as s]))

(def sample "resources/day20-sample.txt")
(def input "resources/day20.txt")

(defn parse-tile [tile-lines]
  (let [num (first (re-seq #"\d+" (first tile-lines)))]
    {:num (read-string num) :data (rest tile-lines)}))

(parse-tile ["Tile 2311:"
             "..##.#..#."
             "##..#....."
             "#...##..#."
             "####.#...#"
             "##.##.###."
             "##...#.###"
             ".#.#.#..##"
             "..#....#.."
             "###...#.#."
             "..###..###"])

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n\n"))
       (map #(s/split % #"\n"))))

(parse-input sample)

(defn grab-edges [m {:keys [num data]}]
  (let [edges [(map identity (first data))
               (vec (reverse (first data)))
               (map identity (last data))
               (vec (reverse (last data)))
               (map first data)
               (vec (reverse (map first data)))
               (map last data)
               (vec (reverse (map last data)))]]
    (reduce (fn [acc e]
              (let [v (get acc e #{})]
                (assoc acc e (conj v num)))) m edges)))

(->> sample
     parse-input
     (map parse-tile)
     (reduce #(grab-edges %1 %2) {})
     (filter (fn [[_ v]] (= 1 (count v))))
     (reduce (fn [acc [_ v-set]]
               (let [v (first v-set)
                     c (get acc v 0)]
                 (assoc acc v (inc c)))) {})
     (filter (fn [[_ v]] (= v 4)))
     (map (fn [[k _]] k))
     (reduce *))
