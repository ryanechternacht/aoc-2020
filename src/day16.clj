(ns day16
  (:require [clojure.string :as s]))

(def sample "resources/day16-sample.txt")
(def input "resources/day16.txt")
(def sample-2 "resources/day16-sample-2.txt")

(defn parse-fields [text]
  (->> text
       (#(s/split % #"\n"))
       (map (fn [s]
              (let [[k s2] (s/split s #":")
                    [_ low _ high] (s/split s2 #" ")]
                [k low high])))
      ;;  ((fn [x] (println x) x))
       (map (fn [[k low high]]
              (let [[l1 l2] (map read-string (s/split low #"-"))
                    [h1 h2] (map read-string (s/split high #"-"))]
                ;; (println l1 l2 h1 h2)
                {(keyword (s/replace k #" " "_"))
                 (fn [n]
                   (or (<= l1 n l2) (<= h1 n h2)))})))
       (reduce conj)))

(parse-fields "departure location: 32-69 or 86-968")

(defn parse-other-tickets [text]
  (->> text
       (#(s/split % #"\n"))
       rest
       (map #(re-seq #"\d+" %))
       (map (fn [l] (map read-string l)))))

(defn parse-my-ticket [text]
  (let [[_ line] (s/split text #"\n")]
    (map read-string (re-seq #"\d+" line))))

(defn parse-input [file]
  (-> file
      slurp
      (s/split #"\n\n")
      (#(zipmap [:fields :my-ticket :other-tickets] %))
      (update :fields parse-fields)
      (update :my-ticket parse-my-ticket)
      (update :other-tickets parse-other-tickets)))

(let [{:keys [other-tickets fields]} (parse-input input)
      test-fns (fn [n] (some #(% n) (vals fields)))]
  (->> other-tickets
       flatten
       (remove test-fns)
       (reduce +)))


;; (let [{:keys [other-tickets my-tickets fields]} (parse-input input)
;;       _ (println my-tickets)
;;       _ (println fields)
;;       cols (for [i (range (count fields))]
;;              [i (map #(nth % i) other-tickets)])
;;       result {}
;;       ;; _ (println (:wagon fields))
;;       ;; filtered (for [f [:wagon (:wagon fields)]]
;;       ;;            (assoc result :wagon
;;       ;;                   (let [results
;;       ;;                         (for [[i l] cols]
;;       ;;                           [i (every? f l)])]
;;       ;;                     (->> results
;;       ;;                          (filter second)
;;       ;;                          (map first)))))]
;;       wagon (:wagon fields)
;;       wagon-results (for [[i l] cols]
;;                       [i (every? wagon l)])
;;       dp (:departure_platform fields)
;;       dp-results (for [[i l] cols]
;;                    [i (every? dp l)])]
;;   dp-results)

(let [{:keys [other-tickets fields]} (parse-input input)
      passes-any-fn (fn [n] (some #(% n) (vals fields)))
      passing-tickets (filter #(every? passes-any-fn %) other-tickets)
      cols (for [i (range (count fields))]
             [i (map #(nth % i) passing-tickets)])
      result {}
      filtered (for [[k f] fields]
                 (assoc result k
                        (let [results
                              (for [[i l] cols]
                                [i (every? f l)])]
                          (->> results
                               (filter second)
                               (map first)))))]
  (println (count passing-tickets))
  (println (count other-tickets))
  filtered)

;; determined manually. there is always a column that has only 1 hit, or a fn that has
;; only 1 column. so you could just loop, look for it, and drop that number from all 
;; others until you get to the end
;; 
;; class 15
;; price 1
;; seat 19
;; arrival platform 1
;; arrival station 7
;; duration 16
;; type 1
;; row 0
;; arrival location 4
;; arrival track 2
;; depature track 13
;; departure station 8
;; departure location 12
;; departur platform 18
;; departure date 3
;; departure time 10
;; zone 9
;; train 11
;; route 6
;; wagon 5

;; 0   1  2   3  4   5   6   7   8  9   10  11 12  13  14 15  16 17  18  19
;; 157,89,103,59,101,181,109,127,67,173,151,97,107,167,61,131,53,163,179,113
(* 167 67 107 179 59 151)
