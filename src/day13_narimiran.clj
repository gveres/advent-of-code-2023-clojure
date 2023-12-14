(ns day13-narimiran
  (:require utils))


(defn differences
  "Counts the number off differences between two sequences. Sequences should be of the same lenght"
  [a b]
  (utils/count-if false? (map = a b)))


(defn is-mirror? [part pattern nrettap line]
  (let [before (take-last line nrettap)
        after (drop line pattern)
        diffs (map differences before after)]
    (case part
      1 (every? zero? diffs)
      2 (= 1 (reduce + diffs)))))


(defn mirror-line [part pattern]
  (utils/find-first
   (partial is-mirror? part pattern (rseq pattern))
   (range 1 (count pattern))))


(defn find-mirror [part pattern]
  (if-let [horizontal-mirror (mirror-line part pattern)]
    (* 100 horizontal-mirror)
    (mirror-line part (utils/transpose pattern))))


(defn notes-sum [patterns part]
  (reduce + (pmap (partial find-mirror part) patterns)))


(defn solve [input-file]
  (let [patterns (utils/read-input-paragraphs input-file)]
    [(notes-sum patterns 1)
     (notes-sum patterns 2)]))


(solve "day13-input")

(def  patterns (utils/read-input-paragraphs "day13-input"))
