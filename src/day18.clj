(ns day18
  (:require [clojure.string :as s]))

(def sample "resources/day18-sample.txt")
(def input "resources/day18.txt")

(defn tokenize [s]
  (re-seq #"[+*()]|\d+" s))

(defn make-group [tokens]
  (loop [depth 1
         group []
         remaining tokens]
    (if (= depth 0)
      [(butlast group) remaining] ;; Exit condition
      (let [[t & new-remaining] remaining
            new-group (conj group t)]
        (case t
          "(" (recur (inc depth) new-group new-remaining)
          ")" (recur (dec depth) new-group new-remaining)
          (recur depth new-group new-remaining))))))

(make-group (tokenize "(2 + 3) + 4) + 5"))

(let [[t & res] (tokenize "(3 + 3)")]
  [t res])


(defn do-math [tokens]
  (let [token-1-raw (first tokens)
        [token-1 remaining-tokens] (if (= token-1-raw "(")
                                     (let [[l t-rest] (make-group (rest tokens))]
                                       [(do-math l) t-rest])
                                     [(read-string token-1-raw) (rest tokens)])]
    (loop [left token-1
           operator nil
           [t & t-rest] remaining-tokens]
      (if (nil? t)
        left
        (case t
          "+" (recur left + t-rest)
          "*" (recur left * t-rest)
          "(" (let [[right t-rest-2] (make-group t-rest)]
                (recur (operator left (do-math right)) nil t-rest-2))
          (recur (operator left (read-string t)) nil t-rest) ;; operand
          )))))

(defn parse-input [file]
  (->> file
       slurp
       (#(s/split % #"\n"))))

(->> input
     parse-input
     (map tokenize)
     (map do-math)
     (reduce +))

(defn do-math-2 [tokens]
  (let [token-1-raw (first tokens)
        [token-1 remaining-tokens] (if (= token-1-raw "(")
                                     (let [[l t-rest] (make-group (rest tokens))]
                                       [(do-math-2 l) t-rest])
                                     [(read-string token-1-raw) (rest tokens)])]
    (loop [ops [token-1]
           add nil
           [t & t-rest] remaining-tokens]
      (println ops add t t-rest)
      (if (nil? t)
        (apply * ops)
        (let [[operand operator new-remaining]
              (case t
                "+" [nil + t-rest]
                "*" [nil * t-rest]
                "(" (let [[o r] (make-group t-rest)]
                      [(do-math-2 o) nil r])
                [(read-string t) nil t-rest])]
          (cond
            (= + operator) (recur ops true new-remaining)
            (= * operator) (recur ops false new-remaining)
            add (let [o (last ops)
                        new-ops (pop ops)]
                    (recur (conj new-ops (+ o operand)) nil new-remaining))
            :else (recur (conj ops operand) nil new-remaining)))))))

(->> input
     parse-input
     (map tokenize)
     (map do-math-2)
     (reduce +))
