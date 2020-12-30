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
           [alg & others] vec-algs]
      (Thread/sleep 1000)
      (println coll)
      (println alg others)
      (if (nil? alg)
        coll
        (let [ing
              (->> coll
                   (filter (fn [[_ a]] (a alg)))
                   (map first)
                   (reduce set/intersection))]
          (println "ing" ing)
          (if (= 1 (count ing))
            (recur (remove-ingredient coll (first ing) alg) others)
            (recur coll (conj (vec others) alg))))))))

(remove-allergens (parse-input sample))

(->> input
     parse-input
     remove-allergens
     (map first)
     (map count)
     (reduce +))

(isolate-ingredients (parse-input input))

(isolate-ingredients (par))