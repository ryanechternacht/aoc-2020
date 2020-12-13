(ns day12)

(def sample "resources/day12-sample.txt")
(def input "resources/day12.txt")

(defn parse-input [file]
  (->> file
       slurp
       (re-seq #".+")
       (map (fn [s]
              {:cmd (keyword (subs s 0 1))
               :value (read-string (subs s 1))}))))

(def dir-to-heading
  {:N {:x 0 :y 1}
   :S {:x 0 :y -1}
   :E {:x 1 :y 0}
   :W {:x -1 :y 0}})

;; move right increment through list, move list decrement
(def dirs [:N :E :S :W])

(defn update-heading [heading degrees]
  (let [steps (/ degrees 90)
        i (.indexOf dirs heading)]
    (nth dirs (mod (+ i steps) 4))))

(defn move [{boat-x :x boat-y :y boat-dir :dir} dir value]
  (let [{:keys [x y]} (dir-to-heading dir)]
    {:x (+ boat-x (* x value)) :y (+ boat-y (* y value)) :dir boat-dir}))

(defn step [{:keys [dir] :as boat} {:keys [cmd value]}]
  (cond
    (#{:N :S :W :E} cmd) (move boat cmd value)
    (= cmd :F) (move boat (:dir boat) value)
    (= cmd :R) (assoc boat :dir (update-heading dir value))
    (= cmd :L) (assoc boat :dir (update-heading dir (- value)))))

(->> input
     parse-input
     (reduce step {:x 0 :y 0 :dir :E})
     ((fn [{:keys [x y]}]
        (+ (max x (- x)) (max y (- y))))))

(defn rotate-heading-right [thing times]
  (loop [i 0
         {:keys [x y dir] :as t} thing]
    (if (= i times)
      t
      (recur (inc i) {:x y :y (- x) :dir dir}))))

(defn rotate-heading-left [thing times]
  (loop [i 0
         {:keys [x y dir] :as t} thing]
    (if (= i times)
      t
      (recur (inc i) {:x (- y) :y x :dir dir}))))

(defn step-2 [{{b-dir :dir b-x :x b-y :y :as boat} :boat {w-x :x w-y :y :as waypoint} :waypoint :as m}
              {:keys [cmd value]}]
  (cond
    (#{:N :S :W :E} cmd) (assoc m :waypoint (move waypoint cmd value))
    (= cmd :F) (assoc m :boat {:x (+ b-x (* value w-x)) :y (+ b-y (* value w-y)) :dir b-dir})
    (= cmd :R) (assoc m :waypoint (rotate-heading-right waypoint (/ value 90)))
    (= cmd :L) (assoc m :waypoint (rotate-heading-left waypoint (/ value 90)))))

(->> input
     parse-input
     (reduce step-2 {:boat {:x 0 :y 0 :dir :E} :waypoint {:x 10 :y 1}})
     ((fn [{{:keys [x y]} :boat}]
        (+ (max x (- x)) (max y (- y))))))
