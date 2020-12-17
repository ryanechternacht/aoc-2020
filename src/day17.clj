(ns day17)

(def sample "resources/day17-sample.txt")
(def input "resources/day17.txt")

(defn get-board 
  ([board x y] (get-board [board] 0 x y))
  ([board z x y]
  (-> board
      (nth z)
      (nth x)
      (nth y))))

(defn parse-input [file]
  (->> file
       slurp
       (re-seq #".+")
       (map #(vec (map identity %)))
       vec))

(defn make-board [input padding]
  (let [len (count input)
        wdt (count (first input))
        hgt 1
        board (vec (repeat (+ padding padding hgt)
                           (vec (repeat (+ padding padding wdt)
                                        (vec (repeat (+ padding padding len) \.))))))]
    (reduce (fn [b [l w]]
              (assoc-in b [padding (+ padding l) (+ padding w)] (get-board input l w)))
            board
            (for [l (range len)
                  w (range wdt)]
              [l w]))))

(defn get-neighbors [z x y]
  (let [fns [dec identity inc]] 
    (for [z-fn fns
          x-fn fns
          y-fn fns]
    [(z-fn z) (x-fn x) (y-fn y)])))

(defn count-adjacent-on [board z x y]
  (count (filter (fn [[z x y]] (= \# (get-board board z x y))) (get-neighbors z x y))))

(defn step [board]
  (let [hgt (count board)
        len (count (first board))
        wdt (count (first (first board)))
        spaces (for [z (range 1 (dec hgt))
                     x (range 1 (dec len))
                     y (range 1 (dec wdt))]
                 [z x y])]
    (reduce (fn [b [z x y]]
              (let [on (= \# (get-board board z x y))
                    adjacent-on (count-adjacent-on board z x y)]
                (cond
                  (and on (not (<= 3 adjacent-on 4))) (assoc-in b [z x y] \.)
                  (and (not on) (= 3 adjacent-on)) (assoc-in b [z x y] \#)
                  :else b)))
            board spaces)))


(-> 6
    #(reduce (fn [b _] (step b)) (make-board (parse-input sample) (inc 6)) (range 6)))

(->> (reduce (fn [b _] (step b)) (make-board (parse-input input) 7) (range 6))
     flatten
     (filter #(= \# %))
     count)