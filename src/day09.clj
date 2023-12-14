(ns day09
  (:require
   clojure.core.reducers
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "inputs/day09-input.txt")]
             (doall (line-seq rdr))))

(def sample-lines
  ["0 3 6 9 12 15"
   "1 3 6 10 15 21"
   "10 13 16 21 30 45"])

(def histories (map #(read-string (str "[" % "]")) lines))

;; this is creating the breakdown thorugh iteration, step-by-step
(defn step [start]
  (loop [acc []
         left start]
    (let [[a b] (take 2 left)]
      (if (nil? b)
        acc
        (recur (conj acc (- b a)) (drop 1 left))))))


(def steps-seq (partial iterate step))

(defn breakdown [start]
  (reverse (take-while #(not-every? zero? %) (steps-seq start))))

;; I'm pretty certain that there is a formula to calculate the next item in the top row,
;; but I decided to just implement this the way it was described

(defn extend-breakdown [breakdown]
  (loop [acc []
         left breakdown
         last-added 0]
    (if-not (seq left)
      acc
      (let [
            new-left (rest left)
            workline (first left)
            new-value (+ (last workline) last-added)
            extended-line (concat workline [new-value])]
        (recur (concat acc [extended-line])
               new-left
               new-value)))))

(def extended-breakdowns (map (comp extend-breakdown breakdown) histories))

(def result-1 (reduce + (map (comp last last) extended-breakdowns)))

result-1
;; => 1702218515


;; part2 do it on reveresed
;; we reverse the numbers in every line and instead of adding we substract, otherwise the exact same shape.
                                        ; could have factored this into a reusable more generic breakdown-extension, but I did not


(defn reversed-breakdown [start]
  (mapv reverse (reverse (take-while #(not-every? zero? %) (steps-seq start)))))

;; (reversed-breakdown [10 13 16 21 30 45])

(defn extend-breakdown-2 [breakdown]
  (loop [acc []
         left breakdown
         last-added 0]
    (if-not (seq left)
      acc
      (let [
            new-left (rest left)
            workline (first left)
            new-value (- (last workline) last-added)
            extended-line (concat workline [new-value])]
        (recur (concat acc [extended-line])
               new-left
               new-value)))))

(def extended-breakdowns-2 (map (comp extend-breakdown-2 reversed-breakdown) histories))

(def result-2 (reduce + (map (comp last last) extended-breakdowns-2)))

result-2
;; => 925
