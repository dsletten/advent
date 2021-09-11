;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               2015.clj
;;;;
;;;;   Started:            Fri Sep 10 12:25:18 2021
;;;;
;;;;   Purpose:
;;;;   Purpose: Advent of Code 2015
;;;;   https://adventofcode.com/2015
;;;;
;;;;   Notes:
;;;;
;;;;

(ns two-thousand-fifteen
  (:use clojure.test
        [clojure.pprint :only (cl-format)])
  (:import))

;;;
;;;    Day 1
;;;
(defn deliver-presents [s]
  "Determine floor for Santa to deliver presents based on instructions `s`. `(` means up one floor. `)` means down one floor."
  (reduce (fn [floor ch]
            (case ch
              \( (inc floor)
              \) (dec floor)))
          0
          s))

(defn deliver-presents [s]
  "Determine floor for Santa to deliver presents based on instructions `s`. `(` means up one floor. `)` means down one floor."
  (loop [i 0
         floor 0]
    (if (== i (count s))
      floor
      (case (get s i)
        \( (recur (inc i) (inc floor))
        \) (recur (inc i) (dec floor)))) ))

(deftest test-deliver-presents ()
  (is (== (deliver-presents "(())") 0))
  (is (== (deliver-presents "()()") 0))
  (is (== (deliver-presents "(((") 3))
  (is (== (deliver-presents "(()(()(") 3))
  (is (== (deliver-presents "))(((((") 3))
  (is (== (deliver-presents "())") -1))
  (is (== (deliver-presents "))(") -1))
  (is (== (deliver-presents ")))") -3))
  (is (== (deliver-presents ")())())") -3)))

;;;
;;;    At first, it seems more natural to treat the string as a sequence,
;;;    but there is some awkwardness.
;;;    
(defn enter-basement [s]
  "Determine when (or whether) Santa enters the basement. 1-based index."
  (loop [ch (first s)
         s (rest s)
         floor (case ch \( 1 \) -1)
         index 1]
    (cond (== floor -1) index
          (empty? s) false
          :else (case (first s)
                  \( (recur (first s) (rest s) (inc floor) (inc index))
                  \) (recur (first s) (rest s) (dec floor) (inc index)))) ))

(defn enter-basement [s]
  "Determine when (or whether) Santa enters the basement. 1-based index."
  (loop [i 0
         floor 0]
    (cond (== floor -1) i
          (== i (count s)) false
          :else (case (get s i)
                  \( (recur (inc i) (inc floor))
                  \) (recur (inc i) (dec floor)))) ))

(deftest test-enter-basement ()
  (is (== (enter-basement ")") 1))
  (is (== (enter-basement "()())") 5))
  (is (not (enter-basement "()"))))
