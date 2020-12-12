(ns day10)

(def sample "resources/day10-sample.txt")
(def sample-2 "resources/day10-sample-2.txt")
(def input "resources/day10.txt")

(defn parse-input [file]
  (->> file
       slurp
       (re-seq #"\d+")
       (map read-string)))

(->> input
     parse-input
     (#(conj % 0))
     (#(conj % (+ 3 (apply max %))))
     sort
     (partition 2 1)
     (map (fn [[n m]] (- m n)))
     (frequencies)
     ((juxt #(get % 1) #(get % 3)))
     (apply *))

(defn find-combinations [nums]
  (let [end (first nums)]
    (loop [[x & xs] (rest nums)
           combs {end 1}]
      (if (nil? x)
        (get combs 0)
        (let [plus-1 (get combs (inc x) 0)
              plus-2 (get combs (+ x 2) 0)
              plus-3 (get combs (+ x 3) 0)
              ways (+ plus-1 plus-2 plus-3)]
          (recur xs (assoc combs x ways)))))))

(find-combinations (->> input
                        parse-input
                        (#(conj % 0))
                        (#(conj % (+ 3 (apply max %))))
                        sort
                        reverse))

