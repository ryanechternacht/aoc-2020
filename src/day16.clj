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

(let [{:keys [other-tickets fields]} (parse-input sample-2)
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
