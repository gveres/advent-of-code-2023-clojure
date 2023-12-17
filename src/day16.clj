(ns day16
  (:require
   clojure.core.reducers
   [clojure.set :as set]
   [utils]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "inputs/day16-input.txt")]
             (doall (line-seq rdr))))

(def sample-lines
  (with-open [rdr (io/reader "inputs/day16-sample.txt")]
    (doall (line-seq rdr))))

(def sample-grid (vec (map vec sample-lines))) ;; row -> column

(def grid (vec (map vec lines)))

(def ray-1 [[0 0] :r])

(defn move [[r c] dir]
  (condp = dir
    :d [(inc r) c]
    :u [(dec r) c]
    :r [r (inc c)]
    :l [r (dec c)]))

(defn mirror [m dir]
  (condp = m
    \/ (condp = dir :r :u, :l :d, :d :l, :u :r)
    \\ (condp = dir :r :d, :l :u, :d :r, :u :l)))

(defn split [s dir]
  (condp = s
    \| (condp = dir
         :r [:u :d]
         :l [:u :d]
         :d [:d]
         :u [:u])
    \- (condp = dir
         :r [:r]
         :l [:l]
         :d [:l :r]
         :u [:l :r])))

(defn step [grid [[r c] dir]]
  (let [e (get-in grid [r c])
        step-candidates (condp = e
                          \. [ [(move [r c] dir) dir] ]
                          \\ [ [(move [r c] (mirror \\ dir)) (mirror \\ dir)] ]
                          \/ [ [(move [r c] (mirror \/ dir)) (mirror \/ dir)] ]
                          \| (let [dirs (split e dir)]
                               (map (fn [d] [(move [r c] d) d]) dirs))
                          \- (let [dirs (split e dir)]
                               (map (fn [d] [(move [r c] d) d]) dirs))
                          (println [r c] dir e)
                          )]
    (filter (fn [[[r c] _dir]]
              (and (>= (dec (count grid)) r 0)
                   (>= (dec (count (first grid))) c 0)))
            step-candidates)))

(defn step-multiray [{:keys [grid rays seen]}]
  (let [new-ray-candidates (mapcat #(step grid %) rays)
        new-rays (remove seen new-ray-candidates)]
    {:rays new-rays
     :grid grid
     :seen (into seen new-rays)}))

(defn state-seq [grid starting-ray]
  (iterate step-multiray {:rays [starting-ray]
                          :grid grid
                          :seen #{starting-ray}}))

(def ran-out (first (drop-while #(not-empty (:rays %))
                                (state-seq grid ray-1))))

(def result-1
  (count (distinct (map first (:seen ran-out)))))
;;=> 7623

(defn energizeds [grid first-ray]
  (let [sseq (state-seq grid first-ray)
        last-step (first (drop-while #(not-empty (:rays %))
                                     sseq))]
    (count (distinct (map first (:seen last-step))))))

;; part2 - try all incoming beams
(defn all-inbeams [grid]
  (let [max-r (dec (count grid))
        max-c (dec (count (first grid)))]
    (println max-r max-c)
    (mapcat identity
            (for [r (range (inc max-r))
                  c (range (inc max-c))
                  :when (or (zero? c)
                            (zero? r)
                            (= max-r r)
                            (= max-c c))]
              (let [dirs (cond
                           (= 0 r c) [:r :d]
                           (and (= 0 r) (= max-c c)) [:l :d]
                           (and (= max-r r) (= 0 c)) [:r :u]
                           (and (= max-r r) (= max-c c)) [:l :u]
                           (= 0 r) [:d]
                           (= 0 c) [:r]
                           (= max-r r) [:u]
                           (= max-c c) [:l]
                           )]
                (map (fn [d] [[r c] d]) dirs)
                )))))

(def result-2
  (reduce max
          (map #(energizeds grid %) (all-inbeams grid))))

result-2



(comment
  (def format
    {:rays []
     :grid []
     :seen []}))
;; => nil
