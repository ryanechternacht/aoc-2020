(ns day3.code)

(def sample "resources/day3-sample.txt")
(def input "resources/day3.txt")

(defn parse-input [file]
  (->> file
       slurp
       (#(clojure.string/split % #"\n"))
       (map vec)
       vec))

(defn is-tree [hill row col]
  (let [width (count (get hill 0))
        y (mod col width)]
    (-> hill
        (get row)
        (get y)
        (= \#))))

(defn count-hits [hill {:keys [x y]}]
  (let [trees? (for [i (range 1 (/ (count hill) y))]
                 (is-tree hill (* i y) (* i x)))]
    (->> trees?
         (filter identity)
         count)))

(count-hits (parse-input input) {:x 3 :y 1})

(def slopes [{:x 1 :y 1}
             {:x 3 :y 1}
             {:x 5 :y 1}
             {:x 7 :y 1}
             {:x 1 :y 2}])

(let [hill (parse-input input)
      hits (for [s slopes]
             (count-hits hill s))]
  (reduce * hits))
