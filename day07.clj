(ns day07
  (:require
   clojure.core.reducers
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def lines (with-open [rdr (io/reader "day07-input.txt")]
             (doall (line-seq rdr))))

(def real-input (into {}
                      (map (fn [line]
                             (let [[hand bid-str] (str/split line #" +")]
                               {hand (read-string bid-str)})) lines)))


(def sample-input
  {"32T3K" 765
   "T55J5" 684
   "KK677" 28
   "KTJJT" 220
   "QQQJA" 483}
  )

(defn five-o-k? [n-tuples] (contains? (into #{} (keys n-tuples)) 5))
(defn four-o-k? [n-tuples] (contains? (into #{} (keys n-tuples)) 4))
(defn full-house? [n-tuples] (and (contains? (into #{} (keys n-tuples)) 2)
                                  (contains? (into #{} (keys n-tuples)) 3)))
(defn three-o-k? [n-tuples] (contains? (into #{} (keys n-tuples)) 3))
(defn two-pair? [n-tuples] (= 2 (get n-tuples 2)))
(defn one-pair? [n-tuples] (= 1 (get n-tuples 2)))
(defn high-card? [n-tuples] (= 5 (get n-tuples 1)))


(defn hand-to-type
  ([h] (hand-to-type h frequencies))
  ([h freq-fn]
   (let [freqs (freq-fn h)
         n-tuples (frequencies (map val freqs))]

     (cond
       (five-o-k? n-tuples) :five-ok
       (four-o-k? n-tuples) :four-ok
       (full-house? n-tuples) :full-house
       (three-o-k? n-tuples) :three-ok
       (two-pair? n-tuples) :two-pair
       (one-pair? n-tuples) :one-pair
       (high-card? n-tuples) :high-card
       :else n-tuples))))

(def card->val {\A "14" \K "13" \Q "12" \J "11" \T "10" \9 "09" \8 "08" \7 "07" \6 "06" \5 "05" \4 "04" \3 "03" \2 "02"})
(def type->val {:five-ok "07" :four-ok "06" :full-house "05" :three-ok "04" :two-pair "03" :one-pair "02" :high-card "01"})

(defn eval-input
  ([input] (eval-input input frequencies card->val))
  ([input freq-fn card->val]
   (map
    (fn [[k v]]
      (let [htype (hand-to-type k freq-fn)] {:hand k :bid v  :type htype :alpha (apply str (type->val htype) (map card->val k))}))
    input)))

(defn result [input]
  (map #(assoc % :winning (* (:bid %) (:rank %)))
       (map-indexed
        #(assoc %2 :rank (inc %1))
        (sort-by :alpha
                 (eval-input input)))))

(def result-1 (reduce + (map :winning (result real-input))))

result-1 ;; => 253910319


;; part 2 (jokers)

(eval-input sample-input)

(defn freqs-with-jokers [h]
  (println h)
  (let [freqs (frequencies h)
        jokers (get freqs \J 0)
        max-non-joker (or (last (sort-by val (frequencies (str/replace h #"J" ""))))
                          (clojure.lang.MapEntry/create \K 0))
        joker-adjusted-freqs (dissoc
                              (merge freqs {(key max-non-joker) (+ jokers (val max-non-joker))})
                              \J)]
    joker-adjusted-freqs))

(def card->val-part2
  {\A "14" \K "13" \Q "12" \J "01" \T "10" \9 "09" \8 "08" \7 "07" \6 "06" \5 "05" \4 "04" \3 "03" \2 "02"})

(def hands-2 (eval-input real-input freqs-with-jokers card->val-part2))

(defn value-all [input]
  (map #(assoc % :winning (* (:bid %) (:rank %)))
       (map-indexed
        #(assoc %2 :rank (inc %1))
        (sort-by :alpha
                 (eval-input input freqs-with-jokers card->val-part2)))))

(def result-2-sample (reduce + (map :winning (value-all sample-input))))

(value-all real-input)

(def result-2 (reduce + (map :winning (value-all real-input))))

result-2
;; => 254083736
