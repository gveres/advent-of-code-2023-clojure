(ns day11
  (:require
   clojure.core.reducers
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "day11-input.txt")]
             (doall (line-seq rdr))))

(def sample-lines
  ["...#......"
   ".......#.."
   "#........."
   ".........."
   "......#..."
   ".#........"
   ".........#"
   ".........."
   ".......#.."
   "#...#....."])

(defn input-to-mx [input]
  (mapv #(mapv identity %) input))

(def sample-image (input-to-mx sample-lines))

(defn transpose [image]
  (println "T")
  (apply mapv vector image))

(defn num-rows [image]
  (count image))

(defn num-cols [image]
  (count (first image)))

(defn expand-empty-rows
  ([image] (expand-empty-rows 2 image))
  ([times image]
   (reduce
    (fn [acc row]
      (if (some #(= % \#) row)
        (concat acc [row])
        (concat acc (vec (take times (cycle [row]))))))
    []
    image)))

(defn expand
  ([image] (expand 2 image))
  ([times image]
   (->> image
        (expand-empty-rows times)
        transpose
        (expand-empty-rows times)
        transpose)))

(def expanded-sample (expand sample-image))

(defn galaxy-coords [image]
  (filter seq
          (for [x (range (num-rows image))
                y (range (num-cols image))]
            (let [current-char (get-in image [y x])]
              (if (= \# current-char)
                [x y]
                nil)))))

(defn coord-combinations [coords]
  (combo/combinations coords 2))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn sum-distances [image]
  (reduce + (->> image
                 expand
                 galaxy-coords
                 coord-combinations
                 (map (partial apply distance)))))


(def result-1 (sum-distances (input-to-mx lines)))

result-1
;; => 9214785


;; part 2 huge expansion
;; naive brute-force won't work, need to adjust distances with the number of expanded rows/cols between the galaxies


(defn expanding-rows [image]
  (reduce
   (fn [acc row]
     (if (some #(= % \#) row)
       nil
       (concat acc [row])))
   []
   image))

(def image (input-to-mx lines))

(defn expanding-rows [image]
  (let [indexed-image (map-indexed (fn [i v] [i v]) image)]
    (map first (filter (fn [[i row]] (not-any? #(= % \#) row)) indexed-image))))

(defn expanding-rows-in-range [expanding-rows rstart rend]
  (count (set/intersection
          (into #{} expanding-rows)
          (into #{} (range (min rstart rend)
                           (max rstart rend))))))

(defn expanding-cols-in-range [expanding-columns cstart cend]
  (count (set/intersection (into #{} expanding-columns)
                           (into #{} (range (min cstart cend)
                                            (max cstart cend))))))



(defn adjusted-distance [expanding-rows expanding-columns [x1 y1] [x2 y2]]
  (let [expanding-cols (expanding-cols-in-range expanding-columns x1 x2)
        expanding-rows (expanding-rows-in-range expanding-rows y1 y2)
        distance (+ (abs (- x1 x2))
                    (abs (- y1 y2))
                    (* 999999 expanding-rows)
                    (* 999999 expanding-cols))]
    distance
    ))


(defn adjusted-distances [expanding-rows expanding-columns]
  (->> image
       galaxy-coords
       coord-combinations
       (map (partial apply adjusted-distance expanding-rows expanding-columns))
       ))


(def result-2 (reduce + (adjusted-distances (expanding-rows image) (expanding-rows (transpose image)))))

(expanding-rows image)
(expanding-rows (transpose image))

(galaxy-coords sample-image)

result-2
;; => 613686987427
