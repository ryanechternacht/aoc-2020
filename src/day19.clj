(ns day19
  (:require [clojure.string :as s]))

(def sample-rules "resources/day19-sample-rules.txt")
(def sample-tests "resources/day19-sample-tests.txt")
(def input-rules "resources/day19-rules.txt")
(def input-tests "resources/day19-tests.txt")

(defn parse-rule [rule]
  (if (= (first rule) \")
    {:value (str (second rule))}
    {:opts (->> (s/split rule #"\s\|\s")
                (map #(s/split % #" "))
                (map #(map read-string %)))}))

(defn parse-rules [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       (map (fn [s] 
              (let [[k v] (s/split s #":\s")]
                [(read-string k) (parse-rule v)])))
       (into {})))

(def rules (parse-rules input-rules))

(def check-rule
  (memoize (fn [rule string]
             (let [{:keys [value opts]} (get rules rule)]
               (if value
                 (= value string)
                 (some (fn [[opt-1 opt-2]]
                         (if (nil? opt-2)
                           (check-rule opt-1 string)
                           (loop [i 1]
                             (if (= i (count string))
                               false ;; final failure
                               (let [left (subs string 0 i)
                                     right (subs string i)]
                                 (if (and (check-rule opt-1 left)
                                          (check-rule opt-2 right))
                                   true ;; success
                                   (recur (inc i))))))))
                       opts))))))

(defn parse-tests [file]
  (->> file
       slurp
       (re-seq #"[ab]+")))

(->> (parse-test input-tests)
     (map #(check-rule)))
