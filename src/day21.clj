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

(defn isolate-ingredients [coll]
  (loop [[[ings algs] & rest] coll]
    (println "start" ings algs)
    (if (nil? ings)
      :failure
      (if (and (seq ings)
               (= (count ings) (count algs)))
        [[ings algs]]
        (let [isolated
              (->> rest
                   (map (fn [[i a]]
                          [(set/intersection ings i)
                           (set/intersection algs a)]))
                   (filter (fn [[i a]]
                             (= (count i) (count a))))
                   (remove (fn [[i _]]
                             (empty? i))))]
          (println "isolated" isolated)
          (if (seq isolated)
            isolated
            (recur rest)))))))

(defn remove-allergens [full-coll]
  (loop [coll full-coll]
    (if (every? (fn [[_ algs]] (empty? algs)) coll)
      coll
      (recur (reduce (fn [c [ing alg]]
                       (println "reduce" c ing alg)
                       (remove-ingredient c (first ing) (first alg)))
                     coll
                     (isolate-ingredients coll))))))


(->> sample
     parse-input
     remove-allergens
     (map first)
     (map count)
     (reduce +))

(isolate-ingredients (parse-input input))

(isolate-ingredients (par))