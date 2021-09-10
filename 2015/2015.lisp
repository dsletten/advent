;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               2015.lisp
;;;;
;;;;   Started:            Fri Sep 10 11:56:11 2021
;;;;   Modifications:
;;;;
;;;;   Purpose: Advent of Code 2015
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
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
  (loop with count = 0
        for ch across s
        do (case ch
             (#\( (incf count))
             (#\) (decf count))
             (otherwise (return nil)))
        finally (return count)))

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
  (loop with count = 0
        for index from 1
        for ch across s
        do (case ch
             (#\( (incf count))
             (#\) (decf count))
             (otherwise (return nil)))
        when (= count -1)
          do (return index)
        finally (return count)))

(deftest test-enter-basement ()
  (check
   (= (enter-basement ")") 1)
   (= (enter-basement "()())") 5)))
