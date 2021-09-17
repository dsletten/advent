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
(defn deliver-presents
  "Determine floor for Santa to deliver presents based on instructions `s`. `(` means up one floor. `)` means down one floor."
  [s]
  (reduce (fn [floor ch]
            (case ch
              \( (inc floor)
              \) (dec floor)))
          0
          s))

(defn deliver-presents
  "Determine floor for Santa to deliver presents based on instructions `s`. `(` means up one floor. `)` means down one floor."
  [s]
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
(defn enter-basement
  "Determine when (or whether) Santa enters the basement. 1-based index."
  [s]
  (loop [ch (first s)
         s (rest s)
         floor (case ch \( 1 \) -1)
         index 1]
    (cond (== floor -1) index
          (empty? s) false
          :else (case (first s)
                  \( (recur (first s) (rest s) (inc floor) (inc index))
                  \) (recur (first s) (rest s) (dec floor) (inc index)))) ))

(defn enter-basement
  "Determine when (or whether) Santa enters the basement. 1-based index."
  [s]
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

;;;
;;;    Day 3
;;;    
(defn visit-houses [s]
  (loop [s s
         x 0
         y 0
         visited {[x y] 1}]
    (cond (empty? s) (count visited)
          :else (let [ch (first s)
                      x1 (case ch \< (dec x) \> (inc x) x)
                      y1 (case ch \^ (inc y) \v (dec y) y)]
                  (recur (rest s) x1 y1 (assoc visited [x1 y1] (inc (visited [x1 y1] 0)))) ))))

(deftest test-visit-houses ()
  (is (== (visit-houses ">") 2))
  (is (== (visit-houses "^>v<") 4))
  (is (== (visit-houses "^v^v^v^v^v") 2)))

;(visit-houses (slurp "/home/slytobias/lisp/books/Advent/advent/2015/day3.data")) => 2081

(defrecord Agent [x y])

(defn location [agent]
  [(:x agent) (:y agent)])

(defn leave-present
  "Leave a present at the agent's location"
  [agent visited]
  (let [loc (location agent)]
    (assoc visited loc (inc (visited loc 0)))) )

(defn update-location
  "Update the agent's location in the given direction."
  [agent direction]
  (let [x (:x agent)
        y (:y agent)
        x1 (case direction \< (dec x) \> (inc x) x)
        y1 (case direction  \^ (inc y) \v (dec y) y)]
    (Agent. x1 y1)))

(defn robo-visit [s]
  (let [santa (Agent. 0 0)
        robot (Agent. 0 0)
        visited (leave-present robot (leave-present santa {}))]
    (loop [q (conj (conj clojure.lang.PersistentQueue/EMPTY santa) robot)
           s s
           visited visited]
      (cond (empty? s) (count visited)
            :else (let [agent (update-location (peek q) (first s))
                        new-q (pop q)]
                    (recur (conj new-q agent) (rest s) (leave-present agent visited)))) )))

(deftest test-robo-visit ()
  (is (== (robo-visit "^v") 3))
  (is (== (robo-visit "^>v<") 3))
  (is (== (robo-visit "^v^v^v^v^v") 11)))
   
;; (robo-visit (slurp "/home/slytobias/lisp/books/Advent/advent/2015/day3.data")) => 2341
