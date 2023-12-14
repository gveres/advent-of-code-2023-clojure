(ns day04
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(def lines (with-open [rdr (io/reader "inputs/day04-input.txt")]
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
        card-no (read-string (subs s 5 8))
        winning (into #{} (map read-string (filter seq (str/split (subs s 10 39) #" +"))))
        mine (into #{} (map read-string (filter seq (str/split (subs s 42) #" +"))))
        matches (set/intersection winning mine)]
    {:card-no card-no
     :winning winning
     :mine mine
     :matches matches
     :num-matches (count matches)
     :points (calculate-points (count matches))}))


(def all-games (map  line->line-data lines))

(def result-1 (reduce + (map :points all-games)))

result-1;; => 27845

(def one-game (nth all-games 6))

(defn add-won-copies [g]
  (let [games-to-add
        (if-not (:claimed g)
          (range 0 (min (count all-games) (:num-matches g)))
          '())
        won-cards (map #(nth all-games (+ % (:card-no g)) nil) games-to-add)]
    (concat [(assoc g :claimed true)] (filter some? won-cards))))

(map :card-no (add-won-copies one-game))

(count (mapcat add-won-copies all-games))

(count (remove :claimed all-games))


(defn recursive-claim [cards]
  (loop [cards cards]
    (let [_ (do (println "Now I have " (count cards) "cards") (flush))
          unclaimed (count (remove :claimed cards))]
      (if (= 0 unclaimed)
        (count cards)

        (recur (mapcat add-won-copies cards))))))

(comment
  (recursive-claim all-games))

(def result-2 (recursive-claim all-games))

;;result-2 is 9496801
