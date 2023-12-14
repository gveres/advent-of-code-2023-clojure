(ns day14
  (:require
   clojure.core.reducers
   [clojure.set :as set]
   [utils]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "inputs/day14-input.txt")]
             (doall (line-seq rdr))))

(def sample-lines
  [
   "O....#...."
   "O.OO#....#"
   ".....##..."
   "OO.#O....O"
   ".O.....O#."
   "O.#..O.#.#"
   "..O..#O..O"
   ".......O.."
   "#....###.."
   "#OO..#...."
   ])

(defn transpose-lines [lines]
  (map (partial apply str) (utils/transpose lines)))



(defn tilt-left [line]
  (->> line
       (partition-by #(= % \#))
       (mapcat (comp reverse sort))
       (apply str)))

(defn tilt-north [lines]
  (transpose-lines (map tilt-left (transpose-lines lines))))

;; The amount of load caused by a single rounded rock (O) is equal to the number of rows from the rock to the south edge of the platform, including the row the rock is on. (Cube-shaped rocks (#) don't contribute to load.) So, the amount of load caused by each rock in each row is as follows:

(defn calculate-line-loads-north [lines]
  (let [max-load (count lines)]
    (map-indexed
     (fn [i l] (* (- max-load i) (utils/count-if #{\O} l))) lines)))

(def result-sample
  (reduce + (-> sample-lines
                tilt-north
                calculate-line-loads-north)))

(def result-part1
  (reduce + (-> lines
                tilt-north
                calculate-line-loads-north)))
