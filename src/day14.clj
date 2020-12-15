(ns day14
  (:require [clojure.string :as s]))

(def sample "resources/day14-sample.txt")
(def sample-2 "resources/day14-sample-2.txt")
(def input "resources/day14.txt")

(defn build-mask-fn [mask-string]
  (->> mask-string
       reverse
       (map-indexed (fn [i l] [i l]))
       (reduce (fn [acc [i l]]
                 (conj acc (condp = l
                             \X identity
                             \1 (fn [x] (bit-or x (bit-flip 0 i)))
                             \0 (fn [x] (bit-and x (bit-flip (bit-not 0) i))))))
               [])
       (apply comp)))

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       (map #(s/split % #" = "))))

(->> input
     parse-input
     (reduce (fn [{:keys [mask-fn] :as state} [instr value]]
               (cond
                 (= instr "mask") (assoc state :mask-fn (build-mask-fn value))
                 (re-seq #"\d+" instr) (let [[i] (re-seq #"\d+" instr)]
                                         (assoc-in state [:mem i] (mask-fn (read-string value))))
                 :else "why are we here!"))
             {:mask-fn nil :mem {}})
     :mem
     vals
     (reduce +))

(parse-input sample)

(defn build-mask-fn-2 [mask-string]
  (fn [value]
    (->> mask-string
         reverse
         (map-indexed (fn [i l] [i l]))
         (reduce (fn [acc [i l]]
                   (condp = l
                     \0 (map #(+ % (bit-and (bit-flip 0 i) value)) acc)
                     \1 (map #(+ % (bit-flip 0 i)) acc)
                     \X (mapcat (fn [x]
                                  [x (+ x (bit-flip 0 i))]) acc)
                     "panic!"))
                 [0]))))

(->> input
     parse-input
     (reduce (fn [{mask-fn :mask-fn :as state} [instr value]]
              ;;  (println state instr value)
               (cond
                 (= instr "mask") (assoc state :mask-fn (build-mask-fn-2 value))
                 (re-seq #"\d+" instr) (let [base-addr (read-string (first (re-seq #"\d+" instr)))]
                                        ;;  (println "inner" base-addr (mask-fn base-addr))
                                         (assoc state :mem
                                                (reduce (fn [mem addr]
                                                          ;; (println "inner inner" mem addr)
                                                          (assoc mem addr (read-string value)))
                                                        (:mem state)
                                                        (mask-fn base-addr))))
                 :else "panic!"))
             {:mask-fn nil :mem {}})
     :mem
     vals
     (reduce +))
