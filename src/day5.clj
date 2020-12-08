(ns day5)

(def sample "resources/day5-sample.txt")
(def input "resources/day5.txt")

(defn parse-input [file]
  (->> file
       slurp
       (#(clojure.string/split % #"\n"))))

(defn convert-to-binary [line]
  (let [m {\B "1" \F "0" \L "0" \R "1"}]
    (apply str (map m line))))

(defn determine-seat [binary]
  {:row (Integer/parseInt (subs binary 0 7) 2)
   :col (Integer/parseInt (subs binary 7 10) 2)})

(->> input
     parse-input
     (map convert-to-binary)
     (map determine-seat)
     (map (fn [{:keys [row col]}] (+ (* row 8) col)))
     (reduce max))

(->> input
     parse-input
     (map convert-to-binary)
     (map determine-seat)
     (map (fn [{:keys [row col]}] (+ (* row 8) col)))
     sort
     (reduce (fn [last next]
               (if (= (inc last) next)
                 next
                 (reduced last)))))
