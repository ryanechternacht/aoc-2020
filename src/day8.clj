(ns day8
  (:require [clojure.string :as s]))

(def sample "resources/day8-sample.txt")
(def input "resources/day8.txt")

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n"))
       (map #(s/split % #" " ))
       (map (fn [[k v]] {:instruction (keyword k) :value (read-string v)}))))

(defn find-acc-before-repeat [steps]
  (loop [line-num 0
         seen #{}
         acc 0]
    ;; (println "func" line-num seen acc)
    (if (seen line-num)
      [:fails acc]
      (let [line (nth steps line-num :end)]
        (if (= line :end)
          [:ends acc]
          (let [{:keys [instruction value]} line
                seen-next (conj seen line-num)]
        ;; (println "step" instruction value)
            (condp = instruction
              :nop (recur (inc line-num) seen-next acc)
              :acc (recur (inc line-num) seen-next (+ acc value))
              :jmp (recur (+ line-num value) seen-next acc))))))))

(-> sample
    parse-input
    find-acc-before-repeat)

(defn build-modified-instructions [original]
  (for [i (range (count original))]
    (let [{:keys [instruction value]} (get original i)
          new-instr (condp = instruction
                      :nop :jmp
                      :jmp :nop
                      :skip)
          new-line {:instruction new-instr :value value}]
      (if (= new-instr :skip)
        :skip
        (assoc original i new-line)))))

(->> input
     parse-input
     vec
     build-modified-instructions
     (filter #(not= % :skip))
     (map find-acc-before-repeat)
     (filter (fn [[k _]] (= k :ends))))
