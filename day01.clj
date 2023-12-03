(ns day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def lines (with-open [rdr (io/reader "day01-input.txt")]
             (doall (line-seq rdr))))

(def matcher (re-matcher #"\d+" "abc12345def"))

(defn extract-number [s]
  (read-string (apply str (re-find #"\d" s)
                      (re-find #"\d" (str/reverse s)))))

(def day01-solution (reduce + (map extract-number lines)))

(def written-nubers
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def test1 "xtwone3four")



(str/index-of test1 "two")

(def day02-solution
  (reduce +
          (map
           (comp extract-number resolve-written-nubers)
           lines)))

(map resolve-written-nubers
     ["two1nine"
      "eightwothree"
      "abcone2threexyz"
      "xtwone3four"
      "4nineeightseven2"
      "zoneight234"
      "7pqrstsixteen" ])
