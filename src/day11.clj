(ns day11)

(def sample "resources/day11-sample.txt")
(def input "resources/day11.txt")

(defn parse-input [file]
  (->> file
       slurp
       (re-seq #".+")
       (map vec)
       vec))

(defn next-state [state x y]
  (let [current (get-in state [x y])
        adjacencies [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        neighbor-occupied (map (fn [[dx dy]]
                                 (= (get-in state [(+ x dx) (+ y dy)]) \#))
                               adjacencies)
        neighbor-count (->> neighbor-occupied (filter identity) count)]
    (cond
      (and (= current \L) (zero? neighbor-count)) \#
      (and (= current \#) (>= neighbor-count 4) \L) \L
      :else current)))

(defn run-generation [state]
  (let [rows (count state)
        cols (count (first state))]
    (->> (for [i (range rows)]
           (for [j (range cols)]
             (next-state state i j)))
         (map vec)
         vec)))

(defn count-occupied [state]
  (->> state
       (map (fn [row]
              (->> row
                   (map #(= \# %))
                   (filter identity)
                   count)))
       (reduce +)))

(count-occupied (loop [state (parse-input input)
                       i 0]
                  (let [next-state (run-generation state)]
                    (println i)
                    (if (or (>= i 1000) (= state next-state))
                      state
                      (recur next-state (inc i))))))

(defn next-state-2 [state x y]
  (let [current (get-in state [x y])
        adjacencies [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        neighbor-occupied (map (fn [[dx dy]]
                                 (loop [x2 (+ x dx)
                                        y2 (+ y dy)]
                                   (condp = (get-in state [x2 y2])
                                     nil false
                                     \# true
                                     \L false
                                     \. (recur (+ x2 dx) (+ y2 dy)))))
                               adjacencies)
        neighbor-count (->> neighbor-occupied (filter identity) count)]
    (cond
      (and (= current \L) (zero? neighbor-count)) \#
      (and (= current \#) (>= neighbor-count 5) \L) \L
      :else current)))

(defn run-generation-2 [state]
  (let [rows (count state)
        cols (count (first state))]
    (->> (for [i (range rows)]
           (for [j (range cols)]
             (next-state-2 state i j)))
         (map vec)
         vec)))

(count-occupied (loop [state (parse-input input)
                       i 0]
                  (let [next-state (run-generation-2 state)]
                    (println i)
                    (if (or (>= i 1000) (= state next-state))
                      state
                      (recur next-state (inc i))))))
