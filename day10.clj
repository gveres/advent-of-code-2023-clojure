(ns day10
  (:require
   clojure.core.reducers
   [clojure.set :as set]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "day10-input.txt")]
             (doall (line-seq rdr))))

(def sample-lines
  ["7-F7-"
   ".FJ|7"
   "SJLL7"
   "|F--J"
   "LJ.LJ"])

;; parse into
(defn sketch [input]
  (apply merge (flatten (map-indexed (fn [y row]
                                       (map-indexed (fn [x char] {[x y] char} ) row)) input))))

;; 95 74 :e

(def sample-sketch (sketch lines))

(defn start [sketch]
  (first (filter #(= (second %) \S) sketch)))

(defn direction [sketch x y]
  (get sketch [x y]))

(def sample-start (key (start sample-sketch)))

;; dir to offsets
(def pipe-types
  {\| {:n :n :s :s}
   \- {:e :e :w :w}
   \L {:s :e :w :n}
   \J {:e :n :s :w}
   \7 {:e :s :n :w}
   \F {:n :e :w :s}
   \. {}})

(def dir->offsets
  {:n [0 -1]
   :e [1 0]
   :w [-1 0]
   :s [0 1]})

(defn dest [[x y] arrival-dir]
  (let [pipe-type (get pipe-types (get sample-sketch [x y]))]
    (get pipe-type arrival-dir)))

(defn next-location [[x y] arrival-dir]
  (let [leave-dir (dest [x y] arrival-dir)
        [xoff yoff] (dir->offsets leave-dir)]
    (println [x y] (get sample-sketch [x y]) leave-dir)
    (if (and xoff yoff)
      [[(+ x xoff) (+ y yoff)] leave-dir]
      nil)))

(def step-seq (iterate (partial apply next-location) [[74 94] :n]))

(def result-1
  (/ (count (take-while some? step-seq)) 2))

(def full-loop
  (take-while some? step-seq))

(def bricks (into #{} (conj (map first full-loop)
                            [74 95])))


(def loop-str (str/split  (apply str (for [
                                           y (range 140)
                                           x (range 140)
                                           ]
                                       (cond
                                         (= x 139) "\n"
                                         (contains? bricks [x y]) (get sample-sketch [x y])
                                         :else " "))) #"\n"))

(second loop-str)

(defn count-walls [s]
  (count (filter #{\F \| \7} s)))

(defn in-fields [line]
  (let [processed-line  (map-indexed (fn [i c]
                                       (let [walls-before (count-walls (subs line 0 i))]
                                         (cond
                                           (and (= c \space) (even? walls-before)) "."
                                           (and (= c \space) (odd? walls-before)) "I"
                                           :else c
                                           ))) line)]
    ;; (println processed-line)
    (get (frequencies processed-line) "I")))

;; (in-fields (nth loop-str 70))

(def result-2 (apply + (filter some? (map in-fields loop-str))))

result-2
;; => 383
