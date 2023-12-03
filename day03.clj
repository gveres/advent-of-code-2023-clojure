(ns day03
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]))

(def lines (with-open [rdr (io/reader "day03-input.txt")]
             (doall (line-seq rdr))))

(def one-line (nth lines 3))

(def digit? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def space? #{\.})

(def gear? #{\*})

(defn pure-nils? [s]
  (empty? (filter some? s)))

(defn value-of-block [num-block]
  (->> num-block
       (map :char)
       (concat)
       (apply str)
       read-string))

(defn x-coords-of-block [num-block]
  (map :x num-block))

(defn num-block-to-data [num-block]
  (let [num-value (value-of-block num-block)
        x-coords (x-coords-of-block num-block)
        y-coord (:y (first num-block))]
    {:num-value num-value
     ;; :x-coords x-coords
     ;; :y-coord y-coord
     :coords (into #{} (for [x x-coords y [y-coord]]
                         {:x x :y y}))}))

(defn blocks-of-line [y l]
  (->> l
       (map-indexed (fn [i c] (when (digit? c) {:x i :char c :y y} )))
       (partition-by nil?)
       (filter (complement pure-nils?))
       (map num-block-to-data)))

(def number-blocks (flatten (map-indexed blocks-of-line lines)))

(defn markers-of-line [y l]
  (->> l
       (map-indexed (fn [i c] (when (not
                                     (or (digit? c)
                                         (space? c)))
                                {:x i :y y})))
       (partition-by nil?)
       (filter (complement pure-nils?))
       flatten))

(defn  marked-by-marker [{:keys [x y]}]
  (for [x-offset [-1 0 1]
        y-offset [-1 0 1]]
    {:x (+ x x-offset)
     :y (+ y y-offset)}))

(def all-marked-coords
  (into #{} (mapcat  marked-by-marker (flatten (map-indexed markers-of-line lines)))))


(defn is-marked? [{:keys [coords]}]
  (seq (set/intersection coords all-marked-coords )))

(def marked-blocks (filter is-marked? number-blocks))

(count marked-blocks)

;; now the sum of marked blocks

(def result-1 (reduce + (map :num-value marked-blocks)))


(defn gears-of-line [y l]
  (->> l
       (map-indexed (fn [i c] (when (gear? c)
                                {:x i :y y})))
       (partition-by nil?)
       (filter (complement pure-nils?))
       flatten))


(def all-gears (flatten (map-indexed gears-of-line lines)))

(def all-gear-marked-coord-sets (map #(into #{} (marked-by-marker %)) all-gears))

(defn is-marked-by? [{:keys [coords]} marked-coords]
  (seq (set/intersection coords marked-coords )))

(def geared-togethers
  (map
   #(filter (fn [x] (is-marked-by? x %)) number-blocks )
   all-gear-marked-coord-sets))

(def valid-gearings (filter #(= 2 (count %)) geared-togethers))

(count geared-togethers)
(count valid-gearings)

(def geared-numbers (map #(map :num-value %) valid-gearings))

(def result-2
  (reduce
   (fn [acc [first second]]
     (+ acc (* first second)))
   0
   geared-numbers))

result-2;; => 87605697
