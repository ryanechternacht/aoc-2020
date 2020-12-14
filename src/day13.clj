(ns day13
  (:require [clojure.string :as s]))

(def sample "resources/day13-sample.txt")
(def input "resources/day13.txt")
(def easy "resources/day13-easy.txt")

(defn parse-input [file]
  (->> file
       slurp
       (re-seq #".+")
       ((fn [[t b]]
          {:time (read-string t)
           :buses (->> (s/split b #",")
                       (map (fn [s]
                              (if (= s "x")
                                s
                                (read-string s)))))}))))

(let [{:keys [time buses]} (parse-input input)]
  (->> buses
       (map (fn [b]
              {:bus b
               :wait-time (if (= b "x")
                            (inc time)
                            (- b (mod time b)))}))
       (sort-by :wait-time)
       first
       ((fn [{:keys [bus wait-time]}] (* bus wait-time)))))

(defn parse-input-2 [file]
  (->> file
       slurp
       (re-seq #".+")
       second
       (#(s/split % #","))
       (map-indexed (fn [i bus] {:bus (if (= bus "x") bus (bigint (read-string bus))) :offset (bigint i)}))
       (filter (fn [{bus :bus}] (not= bus "x")))))

;; (let [buses (parse-input-2 input)
;;       {largest-bus :bus largest-offset :offset} (last (sort-by :bus buses))
;;       every-10000 (bigint (* 10000 largest-bus))]
;;   (println largest-bus)
;;   (loop [t 99999999999551N]
;;     (when (= 0N (mod t every-10000)) (println t))
;;     (if (every? (fn [{:keys [bus offset]}]
;;                   (= 0N (mod (- t (- largest-offset offset)) bus)))
;;                 buses)
;;       (- t largest-offset)
;;       (recur (+ t largest-bus)))))

(let [buses (parse-input-2 input)]
  (:offset (reduce (fn [{acc-offset :offset acc-bus :bus :as acc} {:keys [bus offset] :as b}]
            (println acc b)
            (loop [t acc-offset]
              (println t)
              (if (= 0N (mod (+ t offset) bus))
                {:offset t :bus (* acc-bus bus)}
                (recur (+ t acc-bus)))))
          buses)))

(mod (- 3196 (- 102 3)) 19)
(mod (+ 3417 3) 19)

;; (quot 100000000000000 457)
;; (* 218818380743 457)
;; 

(mod (+ (* 221 15) 102 3) 19)

(mod 102 19)

(- 4199 2873)