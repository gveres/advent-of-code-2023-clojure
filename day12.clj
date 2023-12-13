(ns day12
  (:require
   clojure.core.reducers
   [clojure.core.match :refer [match]]
   [clojure.set :as set]
   [clojure.math.combinatorics :as combo]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "day12-input.txt")]
             (doall (line-seq rdr))))

(def sample-lines
  [
   "???.### 1,1,3"
   ".??..??...?##. 1,1,3"
   "?#?#?#?#?#?#?#? 1,3,1,6"
   "????.#...#... 4,1,1"
   "????.######..#####. 1,6,5"
   "?###???????? 3,2,1"
   ]
  )

(defn trim-goods [springs-str]
  (apply str (as-> springs-str s
               (apply str (drop-while #(= % \.) s))
               (drop-last (count (re-find #"\.+$" s)) s))))

(defn parse-line [statuses-f line]
  (let [[spring-statuses-str damaged-groups-str] (str/split line #" " 2)]
    {:springs (statuses-f spring-statuses-str)
     :damaged-group-sizes (mapv read-string (str/split damaged-groups-str #","))}))

(defn parse-lines [statuses-f lines]
  (mapv (partial parse-line statuses-f) lines))

;; brute force?

(defn poss [s]
  (when (str/includes? s "?")
    [(str/replace-first s #"\?" "#")
     (str/replace-first s #"\?" ".")]))

(defn posses [str]
  (let [num-?s (count (re-seq #"\?" str))]
    (nth (iterate (partial mapcat poss) [str]) num-?s)))

(defn group-counts [str]
  (mapv count (re-seq #"#+" str)))

(defn count-ways-of-fit [str group-sizes]
  (get (frequencies (map group-counts (posses str))) group-sizes))

(def count-ways-of-fit-memo (memoize count-ways-of-fit))


(def sample-input (parse-lines trim-goods sample-lines))

(def real-input (parse-lines trim-goods lines))

(def result-1 (time (reduce + (map (fn [{:keys [springs damaged-group-sizes]}]
                                     (count-ways-of-fit-memo springs damaged-group-sizes))
                                   real-input))))
;; 7017
;; part 2


;; transform input lines

;; five times

(defn transform-single-input [{:keys [springs damaged-group-sizes]}]
  [(str springs "?" springs "?" springs "?" springs "?" springs) (vec (flatten (repeat 5 damaged-group-sizes))) ])

(def part2-input (map transform-single-input real-input))

;; stolen from https://github.com/emlyn/advent-of-clerk-2023/blob/main/src/advent_of_clerk/day_12.clj

;; to learn more:
;; should try the matrix multiplication guy's solution
;; or something with core.match

(declare arrangements)

(defn arrangements*
  "Calculate the number of possible arrangements of the groups given a row."
  [[row [grp & more :as groups]]]
  (cond
    ;; No more groups left, this is possible if there are no more # in the row:
    (nil? grp)
    (if (re-find #"#" row) 0 1)

    ;; Size of groups and gaps is bigger than remaining row, so impossible:
    (> (apply + (dec (count groups)) groups)
       (count row))
    0

    :else  ;; Find first possible position of first group:
    (if-let [[total prefix match _suffix]
             (re-find (re-pattern (format "^([^#]*?)([#?]{%d})([.?]|$)" grp)) row)]
      ;; Add number of arrangements of remaining groups in remaining row...
      (+ (arrangements [(subs row (count total))
                        more])
         ;; ...to number of arrangements with first group later in the row...
         (if (not= \# (first match))
           (arrangements [(subs row (inc (count prefix)))
                          groups])
           ;; ...unless first group cannot go later in the row.
           0))
      0)))

(def arrangements (memoize arrangements*))

(arrangements ["???.###." [1 1 3]]) ;; expect 1

(arrangements [".??..??...?##." [1 1 3]]) ;; expect 4

(arrangements ["?###????????.." [3 2 1]]) ;; expect 10

(def solution-2
  (reduce + (map arrangements part2-input)))

solution-2
;; => 520603948020
