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

(defn transpose-lines* [lines]
  (map (partial apply str) (utils/transpose lines)))

(def transpose-lines (memoize transpose-lines*))

(defn tilt-left [line]
  (->> line
       (partition-by #(= % \#))
       (mapcat (comp reverse sort))
       (apply str)))

(defn tilt-right [line]
  (->> line
       (partition-by #(= % \#))
       (mapcat sort)
       (apply str)))

(defn tilt-north [lines]
  (transpose-lines (map tilt-left (transpose-lines lines))))

(defn tilt-south [lines]
  (transpose-lines (map tilt-right (transpose-lines lines))))

(defn tilt-west [lines]
  (map tilt-left lines))

(defn tilt-east [lines]
  (map tilt-right lines))

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

;;part 2

(defn spin-cycle* [lines]
  (-> lines
      tilt-north
      tilt-west
      tilt-south
      tilt-east))

(def spin-cycle (memoize spin-cycle*))

(def spinned-seq (iterate spin-cycle lines))

(defn lead-in-and-cycle-size [seq]
  (loop [knowns #{}
         knowns-to-index {}
         index 0]
    (let [current (nth seq index)]
      (if (knowns current)
        [(get knowns-to-index current) (- index (get knowns-to-index current))]
        (recur (conj knowns current)
               (assoc knowns-to-index current index)
               (inc index))))))

(lead-in-and-cycle-size spinned-seq)
;; lead-in is 124 , cycle size is 26 for the real one

(lead-in-and-cycle-size (iterate spin-cycle sample-lines))
;; 3 and 7 for the sample

(defn shrink-cycle [n lead-in cycle-size]
  (+ lead-in (rem (- n lead-in) cycle-size)))

(defn calculate-result-2 [input]
  (let [spin-seq (iterate spin-cycle input)
        [lead-in cycle-size] (lead-in-and-cycle-size spin-seq)
        result-index (shrink-cycle 1000000000 lead-in cycle-size)]
    (reduce + (calculate-line-loads-north (nth spin-seq result-index)))))


(calculate-result-2 sample-lines)
;; => 64

(time (calculate-result-2 lines))
;; => 105008
