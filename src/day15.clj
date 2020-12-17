(ns day15)

(def sample {:last-turn 3
             :last-spoken 6
             0 (list 1)
             3 (list 2)
             6 (list 3)})

(def input {:last-turn 6
            :last-spoken 3
            20 (list 1)
            0 (list 2)
            1 (list 3)
            11 (list 4)
            6 (list 5)
            3 (list 6)})

(defn take-turn [{:keys [last-turn last-spoken] :as state}]
  (let [new? (= 1 (count (state last-spoken)))
        will-speak (if new? 0
                       (let [[prior prior-2] (state last-spoken)]
                         (- prior prior-2)))
        this-turn (inc last-turn)]
    (-> state
        (assoc :last-turn this-turn)
        (update will-speak conj this-turn)
        (assoc :last-spoken will-speak))))

(defn thread-print [x] (println x) x)

(println (new java.util.Date))
(loop [{turn :last-turn spoken :last-spoken :as state} input]
  ;; (when (= 0 (mod turn 100000)) (println turn))
  (if (= turn 30000000)
    spoken
    (recur (take-turn state))))
(println (new java.util.Date))
