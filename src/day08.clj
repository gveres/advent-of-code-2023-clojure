(ns day08
  (:require
   clojure.core.reducers
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :as math]))

(def lines (with-open [rdr (io/reader "inputs/day08-input.txt")]
             (doall (line-seq rdr))))



(def directions (first lines))

(def node-lines (drop 2 lines))

(subs (first node-lines) 12 15)

(def nodes (into {} (map (fn [l]
                           (let [node (subs l 0 3)
                                 left (subs l 7 10)
                                 right (subs l 12 15)]
                             {node  {:node node
                                     :left left
                                     :right right} })) node-lines)))

(def directions-inf (cycle directions))

(take 1000 directions-inf)

(nth directions-inf 0)

(defn next-location [nodes location direction]
  (:node (condp = direction
           \L (get nodes (get-in nodes [location :left] ))
           \R (get nodes (get-in nodes [location :right])))))

(next-location nodes "AAA" \L)

(def result-1
  (inc (loop [current-location "AAA"
              steps 0]
         (let [dir (nth directions-inf steps)
               new-location (next-location nodes current-location dir)]
           (if (= new-location "ZZZ")
             steps
             (recur new-location (inc steps)))))))

;; 20093

;; part 2

(def part2-starts (filter #(str/ends-with? % "A") (keys nodes)))

(defn steps-til-z-from [start]
  (loop [current-location start
         steps 0]
    (let [dir (nth directions-inf steps)
          new-location (next-location nodes current-location dir)]
      (if (str/ends-with? new-location "Z")
        [(inc steps) new-location]
        (recur new-location (inc steps))))))

(def cycle-sizes (map first (map steps-til-z-from part2-starts)))

;; the input data was crafted in a way that the cycle sizes are constant (once we know the first A->Z cycle size, doing a cycle of the same size also does A->Z. No idea how to figure this out without guesswork and dumb luck, but this makes it possible to use lcm. Otherwise this would be waaay harder.)

(def result-2 (reduce math/lcm cycle-sizes))
