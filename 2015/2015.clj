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
  (:require [clojure.string :as string])
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

;;;
;;;    Day 2
;;;    
(defn compute-wrap [length width height]
  (let [[a b c] (sort [length width height])
        ab (* a b)
        ac (* a c)
        bc (* b c)
        slack ab]
    (+ (* 2 ab) (* 2 ac) (* 2 bc) slack)))

(deftest test-compute-wrap ()
  (is (== (compute-wrap 2 3 4) 58))
  (is (== (compute-wrap 1 1 10) 43)))

(defn parse-dimensions [dimensions]
  (map read-string (string/split dimensions #"x")))

;;;
;;;    Don't `map` twice!
;;;    
;; (defn calculate-total-wrap [file]
;;   (reduce + (map #(apply compute-wrap %) (map parse-dimensions (string/split (slurp file) #"\n")))) )

(defn calculate-total-wrap [file]
  (reduce + (map #(apply compute-wrap (parse-dimensions %)) (string/split (slurp file) #"\n"))))

(defn calculate-total-quantity [file compute-quantity]
  (reduce + (map (comp (partial apply compute-quantity) parse-dimensions) (string/split (slurp file) #"\n"))))

(defn calculate-total-wrap [file]
  (calculate-total-quantity file compute-wrap))

(defn compute-ribbon [length width height]
  (let [[a b c] (sort [length width height])
        ribbon (+ (* 2 a) (* 2 b))
        bow (* a b c)]
    (+ ribbon bow)))

(deftest test-compute-ribbon ()
  (is (== (compute-ribbon 2 3 4) 34))
  (is (== (compute-ribbon 1 1 10) 14)))

;;;
;;;    Don't `map` twice!
;;;    
;; (defn calculate-total-ribbon [file]
;;   (reduce + (map #(apply compute-ribbon %) (map parse-dimensions (string/split (slurp file) #"\n")))) )

(defn calculate-total-ribbon [file]
  (reduce + (map #(apply compute-ribbon (parse-dimensions %)) (string/split (slurp file) #"\n"))))

(defn calculate-total-ribbon [file]
  (calculate-total-quantity file compute-ribbon))
