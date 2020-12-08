(ns day2.code)

(def file "resources/day2.txt")

(defn parse-input [file]
  (->> file
       slurp
       (#(clojure.string/split % #"\n"))
       (map #(clojure.string/split % #" "))
       (map (fn [[range-raw letter-raw test]]
              (let [[range-min range-max] (clojure.string/split range-raw #"-")]
                {:min (read-string range-min)
                 :max (read-string range-max)
                 :letter (.charAt letter-raw 0)
                 :test test})))))

(defn test-password [{:keys [min max letter test] :as pw}]
  (let [c (get (frequencies test) letter 0)]
    (assoc pw :res (<= min c max))))

(->> file
     parse-input
     (map test-password)
     (filter :res)
     count)

(defn test-password-2 [{:keys [min max letter test] :as pw}]
  (let [c1 (get test (dec min))
        c2 (get test (dec max))
        match-1 (if (= letter c1) 1 0)
        match-2 (if (= letter c2) 1 0)]
    (assoc pw :res (= 1 (+ match-1 match-2)))))

(->> file
     parse-input
     (map test-password-2)
     (filter :res)
     count)