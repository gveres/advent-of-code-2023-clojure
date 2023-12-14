(ns day13
  (:require
   clojure.core.reducers
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "inputs/day13-input.txt")]
             (doall (line-seq rdr))))

(def sample-lines
  ["#.##..##."
   "..#.##.#."
   "##......#"
   "##......#"
   "..#.##.#."
   "..##..##."
   "#.#.##.#."
   ""
   "#...##..#"
   "#....#..#"
   "..##..###"
   "#####.##."
   "#####.##."
   "..##..###"
   "#....#..#"])

(def samples (take-nth 2 (partition-by empty? sample-lines)))

(def input (take-nth 2 (partition-by empty? lines)))

(def sample-1 (vec (first samples)))
(def sample-2 (vec (second samples)))

(defn check-if-mirror-end [pattern i mirror-type]
  (condp = mirror-type
    :left-mirror (and (not= 0 i)
                      (odd? i)
                      (= (take (inc i) pattern)
                         (reverse (take (inc i) pattern))))

    :right-mirror (and (not= (inc i) (count pattern))
                       (even? (- (count pattern) i))
                       (= (drop i pattern) (reverse (drop i pattern))))
    ))

(defn vertical-mirror-ends [pattern]
  (let [first-row (first pattern)
        last-row (last pattern)]
    (filter some? (map-indexed (fn [i other-row]
                                 (let [mirrors-first (= first-row other-row)
                                       mirrors-last (= last-row other-row)]
                                   ;; (println "Row " i " mirrors first " mirrors-first " mirrors last " mirrors-last)
                                   (when (or mirrors-first mirrors-last)
                                     (+ (if (check-if-mirror-end pattern i :left-mirror)
                                          (/ (inc i) 2)
                                          0)
                                        (if (check-if-mirror-end pattern i :right-mirror)
                                          (- (/ (+ (inc i) (count pattern)) 2) 0.5)
                                          0)))))
                               pattern))))

(defn transpose* [vs]
  (map (partial apply str) (apply mapv vector vs)))

(def transpose (memoize transpose*))

(defn all-mirror-lines [pattern]
  {:vertical (reduce + (vertical-mirror-ends pattern))
   :horizontal (reduce + (vertical-mirror-ends (transpose pattern)))})

(defn valuate
  [{:keys [horizontal vertical]}]
  (+ horizontal (* 100 vertical)))

(valuate (all-mirror-lines sample-2))

(def solution-1  (reduce + (map (comp valuate all-mirror-lines) input)))

(comment (map all-mirror-lines input)

         (vertical-mirror-ends (first input))

         (check-if-mirror-end (first input) 1 :left-mirror )

;; 56971 too high
;; 52415 too high
;; 31887 too low
;; 37561 OKOKOK YAY



         (count sample-1)

         (defn row-to-num [row]
           (read-string (str "2r" (str/replace (str/replace row "." "0") "#" "1"))))

         (def sample-2-n (map row-to-num sample-2)))



;; (time (rest (butlast (potential-mirror-ends sample-2-n))))

;; smudges

(all-mirror-lines sample-1)

(defn all-smudges [pattern]
  (for [y (range 0 (count pattern))
        x (range 0 (count (first pattern)))]
    (update-in pattern [y] (fn [row] (apply str (update-in (vec row) [x] (fn [c] (if (= c \.) \# \.))))))))

(defn unsmudged-mirror-lines [pattern]
  (let [ml (all-mirror-lines pattern)
        unsmudged-mls (map all-mirror-lines (all-smudges pattern))
        new-ml (first (filter #(and (not= ml %) (not (and (= 0 (:horizontal %))
                                                          (= 0 (:vertical %))))) unsmudged-mls))
        diff-ml {:vertical (if (= (:vertical new-ml) (:vertical ml)) 0 (:vertical new-ml))
                 :horizontal (if (= (:horizontal new-ml) (:horizontal ml)) 0 (:horizontal new-ml))}]
    (println ml "\n" unsmudged-mls "\n" new-ml "\n" diff-ml)
    diff-ml
    )
  )

(all-mirror-lines sample-1)
(unsmudged-mirror-lines sample-1)
(unsmudged-mirror-lines sample-2)

(def solution-2-sample
  (reduce + (map (comp valuate unsmudged-mirror-lines vec) samples)))



(map (comp unsmudged-mirror-lines vec) input)

(def m4 (nth input 10))

(unsmudged-mirror-lines (vec m4))

(def solution-2
  (reduce + (map (comp valuate unsmudged-mirror-lines vec) input)))

;; 28180 too low
;; 45235 too high

;; 31108 but why?
