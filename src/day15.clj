(ns day15
  (:require
   clojure.core.reducers
   [utils]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def line (first (with-open [rdr (io/reader "inputs/day15-input.txt")]
                   (doall (line-seq rdr)))))

(def sample-line "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn hassh [s]
  (reduce

   (fn [acc c]
     (if (some? c)

       (let [new-acc
             (-> c
                 int
                 (+ acc)
                 (* 17)
                 (mod 256))]
         new-acc)

       acc))
   0

   s))

(defn steps [s]
  (str/split s #","))

(defn hassh-sum-steps [s]
  (let [steps (steps s)
        hasshes (map hassh steps)]
    (reduce + hasshes)))


(def result-1-sample (hassh-sum-steps sample-line))

result-1-sample
;; => 1320

(def result-1 (hassh-sum-steps line))

result-1
;; => 510273

;; ---------------- part 2

(defn parse-step
  "Returns [box label op arg]"
  [step-str]
  (let [[label op arg-c] (partition-by #{\- \=} step-str)
        arg (when arg-c (read-string (apply str arg-c)))
        box (hassh (apply str label))]
    [box (apply str label) (keyword (apply str op)) arg]))

(defn drop-lens [lens label]
  (vec (remove #(= label (first %)) lens)))

(defn replace-or-insert-lens [lens [label strength]]
  (if (some #(= (first %) label) lens) ;; has the label already
    (mapv
     #(if (= (first %) label) [label strength]
          %)
     lens)

    (conj lens [label strength]) ;;or just add
    ))

(defn update-boxes "the dsl interpreter"
  [boxes step]
  (let [[box label op arg] (parse-step step)]
    (if (contains? boxes box)
      (condp = op
        :- (update-in boxes [box] drop-lens label)
        := (update-in boxes [box] replace-or-insert-lens [label arg]))
      boxes)))

(defn process-steps-from-empty [steps]
  (let [empty-boxes (vec (take 256 (cycle [[]])))]
    (reduce update-boxes empty-boxes steps)))

(defn focusing-powers [box-config]
  (flatten (map-indexed
            (fn [box lenses]
              (let [used-slots
                    (map-indexed (fn [slot [_ focal]]
                                   (* (inc box) (inc slot) focal))
                                 lenses)]
                used-slots))
            box-config)))

(def sample-box-config
  (process-steps-from-empty (steps sample-line)))
(def result-2-sample (reduce + (focusing-powers sample-box-config)))

result-2-sample
;; => 145

(def result-2
  (reduce + (-> line
                steps
                process-steps-from-empty
                focusing-powers)))
result-2
;; => 212449
