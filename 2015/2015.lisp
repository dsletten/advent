;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               2015.lisp
;;;;
;;;;   Started:            Fri Sep 10 11:56:11 2021
;;;;
;;;;   Purpose: Advent of Code 2015
;;;;   https://adventofcode.com/2015
;;;;
;;;;
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :2015 (:use :common-lisp :lang :test))

(in-package :2015)

;;;
;;;    Day 1
;;;
(defun deliver-presents (s)
  "Determine floor for Santa to deliver presents based on instructions S. `(` means up one floor. `)` means down one floor."
  (loop with floor = 0
        for ch across s
        do (ecase ch
             (#\( (incf floor))
             (#\) (decf floor)))
        finally (return floor)))

(defun deliver-presents (s)
  "Determine floor for Santa to deliver presents based on instructions S. `(` means up one floor. `)` means down one floor."
  (labels ((deliver (i floor)
             (if (= i (length s))
                 floor
                 (ecase (char s i)
                   (#\( (deliver (1+ i) (1+ floor)))
                   (#\) (deliver (1+ i) (1- floor)))) )))
    (deliver 0 0)))

(defun deliver-presents (s)
  "Determine floor for Santa to deliver presents based on instructions S. `(` means up one floor. `)` means down one floor."
  (reduce #'(lambda (floor ch)
              (ecase ch
                (#\( (1+ floor))
                (#\) (1- floor))))
          s
          :initial-value 0))

(deftest test-deliver-presents ()
  (check
   (= (deliver-presents "(())") 0)
   (= (deliver-presents "()()") 0)
   (= (deliver-presents "(((") 3)
   (= (deliver-presents "(()(()(") 3)
   (= (deliver-presents "))(((((") 3)
   (= (deliver-presents "())") -1)
   (= (deliver-presents "))(") -1)
   (= (deliver-presents ")))") -3)
   (= (deliver-presents ")())())") -3)))

(defun enter-basement (s)
  "Determine when (or whether) Santa enters the basement. 1-based index."
  (loop with floor = 0
        for index from 1
        for ch across s
        do (ecase ch
             (#\( (incf floor))
             (#\) (decf floor)))
        when (= floor -1)
          do (return index)))

(defun enter-basement (s)
  "Determine when (or whether) Santa enters the basement. 1-based index."
  (labels ((basement (i floor)
             (cond ((= floor -1) i)
                   ((= i (length s)) nil)
                   (t (ecase (char s i)
                        (#\( (basement (1+ i) (1+ floor)))
                        (#\) (basement (1+ i) (1- floor)))) ))))
    (basement 0 0)))

(deftest test-enter-basement ()
  (check
   (= (enter-basement ")") 1)
   (= (enter-basement "()())") 5)
   (not (enter-basement "()"))))
