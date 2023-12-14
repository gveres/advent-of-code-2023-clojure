(ns day06
  (:require
   clojure.core.reducers
   [clojure.java.io :as io]
   [clojure.string :as str]))

#_(def lines (with-open [rdr (io/reader "day05-input.txt")]
               (doall (line-seq rdr))))


(def races "[Time distance]" [[61 430]
                              [67 1036]
                              [75 1307]
                              [71 1150]])


(defn hold-to-distance-until
  [hold limit]
  (let [speed hold
        distance (* speed (- limit hold))]
    distance))

(defn ways-to-win [[limit record]]
  (->> (map #(hold-to-distance-until % limit) (range 0 limit))
       (filter #(> % record))
       count))

(apply * (map ways-to-win [[7 9]
                           [15 40]
                           [30 200]]))

(def result-1
  (apply * (map ways-to-win races)))

(def result-2
  (ways-to-win [61677571 430103613071150]));; => #'day06/result-2
