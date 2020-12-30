(ns day21
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def sample "resources/day21-sample.txt")
(def input "resources/day21.txt")

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       (map (fn [line]
              (let [[ings algs] (s/split line #" \(|\)")]
                [(into #{} (s/split ings #" "))
                 (into #{} (s/split (subs algs 9) #", "))])))))

(defn remove-ingredient [coll ing alg]
  (map (fn [[ings algs]]
         [(disj ings ing) (disj algs alg)])
       coll))

(defn remove-allergens [full-coll]
  (let [vec-algs (vec (reduce (fn [acc [_ a]] (into acc a))
                              #{}
                              full-coll))]
    (loop [coll full-coll
           [alg & others] vec-algs
           bad-ingredients []]
      (if (nil? alg)
        bad-ingredients
        (let [ing
              (->> coll
                   (filter (fn [[_ a]] (a alg)))
                   (map first)
                   (reduce set/intersection))]
          (if (= 1 (count ing))
            (recur (remove-ingredient coll (first ing) alg) others (conj bad-ingredients [ing alg]))
            (recur coll (conj (vec others) alg) bad-ingredients)))))))

(remove-allergens (parse-input sample))

(->> input
     parse-input
     remove-allergens
     (sort-by second)
     (map first)
     (map first)
     (s/join ","))
