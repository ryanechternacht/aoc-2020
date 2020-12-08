(ns day4.code)

(def sample "resources/day4-sample.txt")
(def input "resources/day4.txt")

(defn parse-input [file]
  (-> file
      slurp
      (clojure.string/split #"\n")
      split-on-blank-lines))

(defn split-on-blank-lines
  "takes a seq of strings in which some lines are blank. breaks the seq into sublists
   on blank lines. the sublists are concatted into a single line (separated by an additional space)"
  [coll] (:items
          (reduce (fn [{:keys [current] :as acc} line]
                    (if (clojure.string/blank? line)
                      (-> acc
                          (update :items conj (clojure.string/trim current))
                          (assoc :current ""))
                      (update acc :current str " " line)))
                  {:items [] :current ""}
                  coll)))

(defn line-to-map [line]
  (->> line
       (#(clojure.string/split % #" "))
       (map #(clojure.string/split % #":"))
       (reduce (fn [m [k v]] (assoc m (keyword k) v)) {})))

(defn valid-passport? [passport]
  (let [keys [:byr :iyr :eyr :hgt :hcl :ecl :pid]
        valid-keys (reduce (fn [acc k]
                             (+ acc (if (k passport) 1 0)))
                           0 keys)]
    (= valid-keys (count keys))))

(defn valid-passport-2? [passport]
  (let [keys [{:key :byr
               :valid-fn (fn [byr]
                           (<= 1920 (read-string byr) 2002))}
              {:key :iyr
               :valid-fn (fn [iyr]
                           (<= 2010 (read-string iyr) 2020))}
              {:key :eyr 
               :valid-fn (fn [eyr]
                           (<= 2020 (read-string eyr) 2030))}
              {:key :hgt
               :valid-fn (fn [hgt]
                           (cond
                             (re-matches #".*in$" hgt) (try (<= 59 (read-string (subs hgt 0 2)) 76)
                                                            (catch Exception _ false))
                             (re-matches #".*cm$" hgt) (try (<= 150 (read-string (subs hgt 0 3)) 193)
                                                            (catch Exception _ false))
                             :else false))}
              {:key :hcl 
               :valid-fn (fn [hcl]
                           (re-matches #"#[a-f0-9]{6}" hcl))}
              {:key :ecl :valid-fn #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}}
              {:key :pid
               :valid-fn (fn [hcl]
                           (re-matches #"\d{9}" hcl))}]
        valid-keys (reduce (fn [acc {:keys [key valid-fn]}]
                             (let [value (key passport)]
                             (+ acc (if (and value (valid-fn value)) 1 0))))
                           0 keys)]
    (= valid-keys (count keys))))

(->> input
     parse-input
     (map line-to-map)
     (map valid-passport?)
     (filter identity)
     count)

(->> input
     parse-input
     (map line-to-map)
     (map valid-passport-2?)
     (filter identity)
     count)
