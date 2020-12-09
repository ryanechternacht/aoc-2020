(ns day9)

(def sample "resources/day9-sample.txt")
(def input "resources/day9.txt")

(defn parse-input [file]
  (->> file
       slurp
       (re-seq #"\d+")
       (map read-string)))

(defn sum-exists? [numbers sum]
  (let [pairs (for [i numbers
                    j numbers]
                [i j])]
    (->> pairs
         (remove (fn [[j i]] (= j i)))
         (map #(apply + %))
         (some #(= sum %)))))

(defn line-passes? [master-list lookback line]
  (let [target (nth master-list line)
        numbers (->> (range (- line lookback) line)
                     (map #(nth master-list %)))]
    (sum-exists? numbers target)))

(defn run-tests [lookback master-list]
    (for [line (range lookback (count master-list))]
      {:line line
       :value (nth master-list line)
       :outcome (line-passes? master-list lookback line)}))

(->> input
     parse-input
     (run-tests 25)
     (remove (fn [{:keys [outcome]}] outcome)))

(defn try-sum? [start-line target master-list]
  (loop [line start-line
         last-sum 0]
    (let [sum (+ last-sum (nth master-list line))]
      (cond
        (= sum target) {:start start-line
                        :end line}
        (> sum target) false
        (< sum target) (recur (inc line) sum)))))

(defn run-tests-2 [target master-list]
  (map (fn [line] {:line line
                   :outcome (try-sum? line target master-list)})
       (range (count master-list))))

(->> sample
     parse-input
     (run-tests-2 127)
     (filter (fn [{:keys [outcome]}] outcome)))
