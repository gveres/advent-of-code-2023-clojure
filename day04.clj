(ns day04
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(def lines (with-open [rdr (io/reader "day04-input.txt")]
             (doall (line-seq rdr))))

(def one-line (nth lines 3))

(map read-string (str/split (subs one-line 42) #" "))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn calculate-points [m]
  (if (= m 0) 0
      (exp 2 (dec m))))

(defn line->line-data [s]
  (let [;;_ (do (println s) (flush))
        winning (into #{} (map read-string (filter seq (str/split (subs s 10 39) #" +"))))
        mine (into #{} (map read-string (filter seq (str/split (subs s 42) #" +"))))
        matches (set/intersection winning mine)]
    {:winning winning
     :mine mine
     :matches matches
     :num-matches (count matches)
     :points (calculate-points (count matches))}))


(def all-games (map  line->line-data lines))

(def result-1 (reduce + (map :points all-games)))

result-1;; => 27845
