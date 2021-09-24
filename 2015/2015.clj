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
  (:import [java.security MessageDigest]))

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

(defn visit-houses [s]
  (let [leave-present (fn [visited x y]
                        (assoc visited [x y] (inc (visited [x y] 0))))
        update-location (fn [x y ch]
                          [(case ch \< (dec x) \> (inc x) x)
                           (case ch \^ (inc y) \v (dec y) y)])]
    (loop [s s
           x 0
           y 0
           visited {}]
      (let [visited (leave-present visited x y)]
        (cond (empty? s) (count visited)
              :else (let [[x1 y1] (update-location x y (first s))]
                      (recur (rest s) x1 y1 visited)))) )))

(defn visit-houses [s]
  (letfn [(leave-present [visited x y]
            (let [current (visited [x y] 0)]
              (assoc visited [x y] (inc current))))
          (update-location [x y ch]
            [(case ch \< (dec x) \> (inc x) x)
             (case ch \^ (inc y) \v (dec y) y)])
          (visit [s x y visited]
            (let [new-visited (leave-present visited x y)]
              (cond (empty? s) (count new-visited)
                    :else (let [[x1 y1] (update-location x y (first s))]
                            (recur (rest s) x1 y1 new-visited)))) )]
    (visit s 0 0 {})))

(deftest test-visit-houses ()
  (is (== (visit-houses ">") 2))
  (is (== (visit-houses "^>v<") 4))
  (is (== (visit-houses "^v^v^v^v^v") 2)))

;(visit-houses (slurp "/home/slytobias/lisp/books/Advent/advent/2015/day3.data")) => 2081

(defrecord Agent [x y])

(defn make-agent
  ([] (make-agent 0 0))
  ([x y] (Agent. x y)))

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
    (make-agent x1 y1)))

(def update-map {\< (fn [[x y]] [(dec x) y])
                 \> (fn [[x y]] [(inc x) y])
                 \^ (fn [[x y]] [x (inc y)])
                 \v (fn [[x y]] [x (dec y)])})
(defn update-location
  "Update the agent's location in the given direction."
  [agent direction]
  (let [[x1 y1] ((update-map direction) (location agent))]
    (make-agent x1 y1)))

;; (defn robo-visit [s]
;;   (let [santa (make-agent)
;;         robot (make-agent)
;;         visited (leave-present robot (leave-present santa {}))]
;;     (loop [q (conj (conj clojure.lang.PersistentQueue/EMPTY santa) robot)
;;            s s
;;            visited visited]
;;       (cond (empty? s) (count visited)
;;             :else (let [agent (update-location (peek q) (first s))
;;                         new-q (pop q)]
;;                     (recur (conj new-q agent) (rest s) (leave-present agent visited)))) )))

(defn robo-visit [s]
  (let [santa (make-agent)
        robot (make-agent)
        visited (leave-present robot (leave-present santa {}))
        team (conj (conj clojure.lang.PersistentQueue/EMPTY santa) robot)]
    (letfn [(visit [s team visited]
              (cond (empty? s) (count visited)
                    :else (let [agent (update-location (peek team) (first s))
                                new-team (pop team)]
                            (recur (rest s) (conj new-team agent) (leave-present agent visited)))) )]
      (visit s team visited))))

(deftest test-robo-visit ()
  (is (== (robo-visit "^v") 3))
  (is (== (robo-visit "^>v<") 3))
  (is (== (robo-visit "^v^v^v^v^v") 11)))
   
;; (robo-visit (slurp "/home/slytobias/lisp/books/Advent/advent/2015/day3.data")) => 2341

;;;
;;;    Day 4
;;;    
;;;
;;;    Inspired by:
;;;    https://gist.github.com/jizhang/4325757
;;;    Aleksander Madland Stapnes
(defn coin-found? [^String seed n]
  (let [nybbles (mapcat #(vector (quot % 16) (rem % 16))
                        (map #(Byte/toUnsignedInt %) (vec (.digest (MessageDigest/getInstance "MD5") (.getBytes seed)))) )]
    (every? zero? (take n nybbles))))

(defn mine-advent-coin [^String prefix n]
  (loop [i 1]
    (let [seed (cl-format false "~A~D" prefix i)]
      (if (coin-found? seed n)
        i
        (recur (inc i)))) ))

;;;
;;;    This is very slow! Need to figure out how to speed up...
;;;    
;; (deftest test-mine-advent-coin ()
;;   (is (== (mine-advent-coin "abcdef" 5) 609043))
;;   (is (== (mine-advent-coin "pqrstuv" 5) 1048970)))

;; (mine-advent-coin "ckczppom" 5) => 117946
;; (mine-advent-coin "ckczppom" 6) => 3938038

;;;
;;;    Day 5
;;;    
(defn nice-string? [s]
  (letfn [(nice? [vowels double?]
            (and (>= vowels 3) double?))
          (vowel? [ch]
            (contains? #{\a \e \i \o \u} ch))
          (check-double [ch s vowels double?]
            (let [next-ch (first s)]
              (cond (nil? next-ch) (nice? vowels double?)
                    :else (check-nice next-ch (rest s) vowels (or (= ch next-ch) double?)))) )
          (check-nice [ch s vowels double?]
            (cond (nil? ch) (nice? vowels double?)
                  :else (case ch
                          \a (check-a (first s) (rest s) (inc vowels) double?)
                          \c (check-c (first s) (rest s) vowels double?)
                          \p (check-p (first s) (rest s) vowels double?)
                          \x (check-x (first s) (rest s) vowels double?)
                          (check-double ch s (if (vowel? ch) (inc vowels) vowels) double?))))
          (check-a [ch s vowels double?]
            (cond (nil? ch) (nice? vowels double?)
                  :else (case ch
                          \b false
                          \a (check-a (first s) (rest s) (inc vowels) true)
                          \c (check-c (first s) (rest s) vowels double?)
                          \p (check-p (first s) (rest s) vowels double?)
                          \x (check-x (first s) (rest s) vowels double?)
                          (check-double ch s (if (vowel? ch) (inc vowels) vowels) double?))))
          (check-c [ch s vowels double?]
            (cond (nil? ch) (nice? vowels double?)
                  :else (case ch
                          \d false
                          \a (check-a (first s) (rest s) (inc vowels) double?)
                          \c (check-c (first s) (rest s) vowels true)
                          \p (check-p (first s) (rest s) vowels double?)
                          \x (check-x (first s) (rest s) vowels double?)
                          (check-double ch s (if (vowel? ch) (inc vowels) vowels) double?))))
          (check-p [ch s vowels double?]
            (cond (nil? ch) (nice? vowels double?)
                  :else (case ch
                          \q false
                          \a (check-a (first s) (rest s) (inc vowels) double?)
                          \c (check-c (first s) (rest s) vowels double?)
                          \p (check-p (first s) (rest s) vowels true)
                          \x (check-x (first s) (rest s) vowels double?)
                          (check-double ch s (if (vowel? ch) (inc vowels) vowels) double?))))
          (check-x [ch s vowels double?]
            (cond (nil? ch) (nice? vowels double?)
                  :else (case ch
                          \y false
                          \a (check-a (first s) (rest s) (inc vowels) double?)
                          \c (check-c (first s) (rest s) vowels double?)
                          \p (check-p (first s) (rest s) vowels double?)
                          \x (check-x (first s) (rest s) vowels true)
                          (check-double ch s (if (vowel? ch) (inc vowels) vowels) double?)))) ]
    (check-nice (first s) (rest s) 0 false)))

(deftest test-nice-string? ()
  (is (nice-string? "ugknbfddgicrmopn"))
  (is (nice-string? "aaa"))
  (is (not (nice-string? "jchzalrnumimnmhp")))
  (is (not (nice-string? "haegwjzuvuyypxyu")))
  (is (not (nice-string? "dvszwmarrgswjxmb"))))

;; (count (remove false? (map nice-string? (string/split (slurp "day5.data") #"\n")))) => 236

(defn nice-string?* [s]
  (let [rule1 (re-find #"(.)(.).*\1\2" s)
        rule2 (re-find #"(.).\1" s)]
    (if (and rule1 rule2)
      true
      false)))

(deftest test-nice-string?* ()
  (is (nice-string?* "qjhvhtzxzqqjkmpb"))
  (is (nice-string?* "xxyxx"))
  (is (nice-string?* "qryjbohkprfazczc")) ; Check index for Rule 1!
  (is (not (nice-string?* "uurcxstgmygtbstg")))
  (is (not (nice-string?* "ieodomkazucvgmuy")))
  (is (not (nice-string?* "suerykeptdsutidb")))) ; Check index for Rule 2!

;; (count (remove false? (map nice-string?* (string/split (slurp "day5.data") #"\n")))) => 51
