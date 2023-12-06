(ns day05
  (:require
   clojure.core.reducers
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def lines (with-open [rdr (io/reader "day05-input.txt")]
             (doall (line-seq rdr))))

(def seeds (map read-string (-> (first lines)
                                (subs 7)
                                (str/split #" "))))

(defn line-range [lines start len]
  (map #(map read-string (str/split % #" ")) (take len (drop start lines))))

(defn resolve-with-mapdefs [lines src-num]
  (let [applicable-mapdef (first (filter (fn [[dst-start src-start range-length]]
                                           (<= src-start src-num (+ src-start range-length)))
                                         lines))]
    (if applicable-mapdef
      (- src-num (- (second applicable-mapdef)
                    (first applicable-mapdef)))
      src-num)))

(defn resolver [lines start len]
  (partial resolve-with-mapdefs (line-range lines start len)))


(def seed-to-soil (resolver lines 3 25) )
(def soil-to-fertilizer (resolver lines 30 9))
(def fertilizer-to-water (resolver lines 41 33))
(def water-to-light (resolver lines 76 46))
(def light-to-temperature (resolver lines 124 35))
(def temperature-to-humidity (resolver lines 161 20))
(def humidity-to-location (resolver lines 183 43))

(def demo '((50 98 2)(52 50 48)))

(def seed-to-location (comp
                       humidity-to-location
                       temperature-to-humidity
                       light-to-temperature
                       water-to-light
                       fertilizer-to-water
                       soil-to-fertilizer
                       seed-to-soil))


(def result-1
  (apply min (map seed-to-location seeds)))

result-1;; => 31599214

seeds

(def seed-range-defs (into [] (apply hash-map seeds)))

(def seed-ranges (map (fn [[start len]] {:start start :end (+ start len)}) seed-range-defs))



(comment
  (def seeds-def2 (mapcat
                   (fn [[start len]] (range start (+ start len)))
                   (apply hash-map seeds)))

  (def seed-info-def2 (map
                       (fn [[start len]] {:start start :end (+ start len)})
                       (apply hash-map seeds)))

  (count seeds-def2)

  (def locations-2 (pmap seed-to-location seeds-def2))

  ((first locations-2))

  (def result-2
    (time (clojure.core.reducers/reduce min (first locations-2) locations-2)))


  )

(defn line->xf [[dst-start src-start range-length]]
  {:start src-start
   :end (dec (+ src-start range-length))
   :offset (- dst-start src-start)})

(def lines-seed-to-soil (map line->xf (line-range lines 3 25)) )
(def lines-soil-to-fertilizer (map line->xf (line-range lines 30 9)))
(def lines-fertilizer-to-water (map line->xf (line-range lines 41 33)))
(def lines-water-to-light (map line->xf (line-range lines 76 46)))
(def lines-light-to-temperature (map line->xf (line-range lines 124 35)))
(def lines-temperature-to-humidity (map line->xf (line-range lines 161 20)))
(def lines-humidity-to-location (map line->xf (line-range lines 183 43)))


;; (fn [[dst-start src-start range-length]]
;; (<= src-start src-num (+ src-start range-length)))

(def seed-info-def2 (map
                     (fn [[start len]] {:start start :end (+ start len)})
                     (apply hash-map seeds)))

;; apply a transformation to a range

(def one-range (first seed-info-def2))

(def one-xform (first lines-seed-to-soil))




(defn swallow? [r x] (<= (:start x) (:start r) (:end r) (:end x)))
(defn bite-low? [r x] (<= (:start x) (:start r) (:end x) (:end r)))
(defn bite-high? [r x] (<= (:start r) (:start x) (:end r) (:end x)))
(defn bite-mid? [r x] (<= (:start r) (:start x) (:end x) (:end r)))
(defn miss? [r x] (or (<= (:start r) (:end r) (:start x) (:end x))
                      (<= (:start x) (:end x) (:start r) (:end r))))

(swallow? one-range one-xform)
(bite-low? one-range one-xform)
(bite-high? one-range one-xform)
(bite-mid? one-range one-xform)
(miss? one-range one-xform)


(defn apply-xf [range xf]
  (let [{:keys [start end]} range
        [xform-start xform-end] ((juxt :start :end) xf)
        offset (:offset xf)]
    (println "Will apply xf " xf " to range " range)
    (if-not (:touched range)
      (let [res (cond
                  (swallow? range xf) (do (println "This is a swallow!")
                                          [{:start (+ start offset) :end (+ end offset) :touched true}])

                  (bite-low? range xf) (do (println "This is a bite-low!")
                                           [{:start (+ start offset) :end (+ xform-end offset) :touched true}
                                            {:start (inc xform-end) :end end}])

                  (bite-high? range xf)  (do (println "This is a bite-high!")
                                             [{:start start :end (dec xform-start)}
                                              {:start (+ xform-start offset) :end (+ end offset) :touched true}])


                  (bite-mid? range xf)  (do (println "This is a bite-mid!")
                                            [{:start start :end (dec xform-start)}
                                             {:start (+ xform-start offset) :end (+ offset xform-end) :touched true}
                                             {:start (inc xform-end) :end end}])

                  (miss? range xf) (do (println "This is a miss!")
                                       [range])

                  :else (println "!!!" range xf)

                  )]
        (when-let [nullas (seq (filter #(= (:start %) 0) res))] (println "!0! " range xf nullas)
                  )
        res)
      [range])))

(defn xform-applies-to-range? [xform range]
  ;; (print "Checking if xform applies to range: " xform " / " range)
  (or (bite-high? range xform)
      (bite-low? range xform)
      (bite-mid? range xform)
      (swallow? range xform)))


#_(defn apply-xfs-to-ranges [xforms ranges]
    (println xforms ranges)
    (into #{} (flatten (if (seq xforms)
                         (for [range ranges
                               xform (filter #(xform-applies-to-range? % range) xforms)]
                           (apply-xf range xform))
                         [range]))))

(defn apply-xfs-to-ranges [the-xforms the-ranges]
  (let [result (reduce
                (fn [acc xf]
                  (prn "xf is now " xf)
                  (prn "acc is now " acc)
                  (into #{}
                        (mapcat #(apply-xf % xf) acc)))
                the-ranges
                the-xforms)]
    (println "Application round results: " result)
    (map #(dissoc % :touched) result)))


(apply-xfs-to-ranges lines-seed-to-soil seed-info-def2)

(def all-locations-2
  (->> seed-info-def2
       (apply-xfs-to-ranges lines-seed-to-soil)
       (apply-xfs-to-ranges lines-soil-to-fertilizer)
       (apply-xfs-to-ranges lines-fertilizer-to-water)
       (apply-xfs-to-ranges lines-water-to-light)
       (apply-xfs-to-ranges lines-light-to-temperature)
       (apply-xfs-to-ranges lines-temperature-to-humidity)
       (apply-xfs-to-ranges lines-humidity-to-location)
       ))

(first (sort-by :start all-locations-2))
;; => {:start 20358599, :end 25430813}  <- now that's the result... not proud of the solution, but still much better than brute forcing

(comment
  (count all-locations-2)

  (def min-location (second (sort-by :start all-locations-2)))

  (def demo-seed-ranges
    [{:start 79
      :end 92}
     {:start 55
      :end 67}])

  (def se-so [(line->xf '(50 98 2))
              (line->xf '(52 50 48))])

  (def so-fe [(line->xf '(0 15 37))
              (line->xf '(37 52 2))
              (line->xf '(39 0 15))])

  (def fe-wa [(line->xf '(49 53 8))
              (line->xf '(0 11 42))
              (line->xf '(42 0 7))
              (line->xf '(57 7 4))])

  (def wa-li [(line->xf '(88 18 7))
              (line->xf '(18 25 70))])

  (def li-te [(line->xf '(45 77 23))
              (line->xf '(81 45 19))
              (line->xf '(68 64 13))])

  (def te-hu [(line->xf '(0 69 1))
              (line->xf '(1 0 69))])

  (def hu-lo [(line->xf '(60 56 37))
              (line->xf '(56 93 4))])

  (def demo-locs
    (->> demo-seed-ranges
         (apply-xfs-to-ranges se-so)
         (apply-xfs-to-ranges so-fe)
         (apply-xfs-to-ranges fe-wa)
         (apply-xfs-to-ranges wa-li)
         (apply-xfs-to-ranges li-te)
         (apply-xfs-to-ranges te-hu)
         (apply-xfs-to-ranges hu-lo)
         ))

  (apply min (map :start demo-locs))


  (def the-xforms se-so)
  (def the-ranges demo-seed-ranges)

(apply-xfs-to-ranges [{:start 99 :end 100 :offset 5} {:start 100 :end 150 :offset -4}]
                     [{:start 6 :end 1200} {:start 1500 :end 1900}]))
