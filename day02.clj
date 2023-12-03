(ns day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def lines (with-open [rdr (io/reader "day02-input.txt")]
             (doall (line-seq rdr))))

(defn game-string-to-data [s]
  (into {} (->> (str/split s #",")
                (map str/trim)
                (map #(str/split % #" "))
                (mapcat (fn [[n color]] {(keyword color) (read-string n) })))))

(defn line-to-data [l]
  (let [games-string (map str/trim (str/split (subs l (inc (str/index-of l ":"))) #";"))
        games (mapv game-string-to-data games-string)
        maxes (reduce #(merge-with max %1 %2) games)]
    {:game-id (read-string (subs l 5 (str/index-of l ":")))
     :games games
     :maxes maxes
     :power (* (:red maxes) (:green maxes) (:blue maxes))}))

(def line-data (map line-to-data lines))

(def one-game (first (:games (first (map line-to-data lines)))))

(def rounds [ { :blue 7, :green 6, :red 3 } { :red 3, :green 5, :blue 1 } { :red 1, :green 5, :blue 8 } { :red 3, :green 1, :blue 5 } ])

(def possibles
  (filter
   (fn [{:keys [maxes game-id]}]
     (let [{:keys [red green blue]} maxes]
       (and
        (<= red 12)
        (<= green 13)
        (<= blue 14))))
   line-data))

(def result-1
  (reduce
   (fn [acc p]
     (+ acc (:game-id p)))
   0
   possibles))

(def result-2 (reduce + (map :power line-data)))
