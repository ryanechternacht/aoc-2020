(ns day7
  (:require [clojure.string :as s]))

(def sample "resources/day7-sample.txt")
(def input "resources/day7.txt")

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       (map (fn [line]
              (->> (s/split line #"contain|,|\.")
                   (map clojure.string/trim))))))

(defn build-map [lines]
  (let [get-bag-name (fn [b] (s/join " " (take 2 (s/split b #" "))))]
    (reduce (fn [m [k & others]]
              (let [new-map
                    (reduce
                     (fn [acc to-bag]
                       (if (= to-bag "no other bags")
                         acc
                         (let [first-space (s/index-of to-bag " ")
                               n (read-string (subs to-bag 0 first-space))
                               bag (subs to-bag (inc first-space))]
                           (assoc acc (get-bag-name bag) n))))
                     {}
                     others)]
                (assoc m (get-bag-name k) new-map)))
            {}
            lines)))

(defn check-bag-recurse [master target-bag bag]
  (let [bags (master bag)]
    (cond
      (zero? (count bags)) nil
      (some (fn [[k _]] (= k target-bag)) bags) true
      :else (some (fn [[k _]] (check-bag-recurse master target-bag k)) bags))))

(comment
  (check-bag-recurse {"dark orange bags" {"bright white bags" 3, "muted yellow bags" 4}
                      "bright white bags" {"shiny gold bag" 1}
                      "vibrant plum bags" {"faded blue bags" 5, "dotted black bags" 6}
                      "shiny gold bags" {"dark olive bag" 1, "vibrant plum bags" 2}
                      "faded blue bags" {}
                      "dotted black bags" {}
                      "light red bags" {"bright white bag" 1, "muted yellow bags" 2}
                      "dark olive bags" {"faded blue bags" 3, "dotted black bags" 4}
                      "muted yellow bags" {"shiny gold bags" 2, "faded blue bags" 9}}
                     "shiny gold bags"
                     "bright white bags"))


(defn bags-which-contain-bag [master bag]
  (->> master 
       (map (fn [[k _]] [k (check-bag-recurse master bag k)]))
       (reduce conj {})))

(-> input
    parse-input
    build-map)

(->> (-> input
         parse-input
         build-map
         (bags-which-contain-bag "shiny gold"))
     (filter (fn [[_ v]] v))
     count)

(defn count-bags [master bag]
  (println "start" bag)
  (let [bags (master bag)]
    (println "bags" bags)
    (if (empty? bags)
      1
      (reduce (fn [acc [k v]] (println "inner" k v) (+ acc  (* v (count-bags master k)))) 1 bags))))

(-> input
    parse-input
    build-map
    (count-bags "shiny gold"))

(s/index-of "1 brigth white bag" " ")

(subs "1 bright white bag" 0 1)
(subs "1 bright white bag" 2)
