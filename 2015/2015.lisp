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
(load "/home/slytobias/lisp/packages/strings.lisp")
(load "/home/slytobias/lisp/packages/io.lisp")
(load "/home/slytobias/lisp/packages/collections.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :2015 (:use :common-lisp :lang :strings :io :test))

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

;;;
;;;    Day 2
;;;    
(defun compute-wrap (length width height)
  (destructuring-bind (a b c) (sort (list length width height) #'<)
    (let* ((ab (* a b))
           (ac (* a c))
           (bc (* b c))
           (slack ab))
      (+ (* 2 ab) (* 2 ac) (* 2 bc) slack))))

(deftest test-compute-wrap ()
  (check
   (= (compute-wrap 2 3 4) 58)
   (= (compute-wrap 1 1 10) 43)))

(defun parse-dimensions (dimensions)
  "Convert serialized dimension into list of dimensions: '2x3x4' => (2 3 4)"
  (mapcar #'read-num (split dimensions #\x)))

(deftest test-parse-dimensions ()
  (check
   (equal (parse-dimensions "29x13x26") '(29 13 26))
   (equal (parse-dimensions "11x11x14") '(11 11 14))
   (equal (parse-dimensions "27x2x5") '(27 2 5))))

;; (defun calculate-total-wrap (file)
;;   (loop for package in (read-file file)
;;         summing (apply #'compute-wrap (parse-dimensions package))))

;; (defun calculate-total-quantity (file compute-quantity)
;;   (reduce #'+ (mapcar #'(lambda (package)
;;                           (apply compute-quantity (parse-dimensions package)))
;;                       (read-file file))))

(defun calculate-total-quantity (file compute-quantity)
  (reduce #'+ (mapcar (compose (partial #'apply compute-quantity) #'parse-dimensions) (read-file file))))

(defun calculate-total-wrap (file)
  (calculate-total-quantity file #'compute-wrap))

(defun compute-ribbon (length width height)
  (destructuring-bind (a b c) (sort (list length width height) #'<)
    (let ((ribbon (+ (* 2 a) (* 2 b)))
          (bow (* a b c)))
      (+ ribbon bow))))

(deftest test-compute-ribbon ()
  (check
   (= (compute-ribbon 2 3 4) 34)
   (= (compute-ribbon 1 1 10) 14)))

;; (defun calculate-total-ribbon (file)
;;   (loop for package in (read-file file)
;;         summing (apply #'compute-ribbon (parse-dimensions package))))

;; (defun calculate-total-ribbon (file)
;;   (reduce #'+ (mapcar #'(lambda (package)
;;                           (apply #'compute-ribbon (parse-dimensions package)))
;;                       (read-file file))))

(defun calculate-total-ribbon (file)
  (calculate-total-quantity file #'compute-ribbon))

;; (calculate-total-wrap "/home/slytobias/lisp/books/Advent/advent/2015/foo.data") => 101
;; (calculate-total-wrap "/home/slytobias/lisp/books/Advent/advent/2015/day2.data") => 1586300
;; (calculate-total-ribbon "/home/slytobias/lisp/books/Advent/advent/2015/foo.data") => 48
;; (calculate-total-ribbon "/home/slytobias/lisp/books/Advent/advent/2015/day2.data") => 3737498

;;;
;;;    Day 3
;;;    
(defun visit-houses (s)
  (let ((visited (make-hash-table :test #'equal))
        (x 0)
        (y 0))
    (loop for ch across s
          do (incf (gethash (cons x y) visited 0))
             (case ch
               (#\^ (incf y))
               (#\v (decf y))
               (#\> (incf x))
               (#\< (decf x))))
    (incf (gethash (list x y) visited 0))
    (hash-table-count visited)))

(defun visit-houses (s)
  (let ((visited (make-hash-table :test #'equal))
        (stream (make-string-input-stream s)))
    (labels ((visit (x y)
               (incf (gethash (cons x y) visited 0))
               (let ((ch (read-char stream nil nil)))
                 (case ch
                   (#\^ (visit x (1+ y)))
                   (#\v (visit x (1- y)))
                   (#\> (visit (1+ x) y))
                   (#\< (visit (1- x) y))
                   ((nil) (hash-table-count visited)))) ))
      (visit 0 0))))

(deftest test-visit-houses ()
  (check
   (= (visit-houses ">") 2)
   (= (visit-houses "^>v<") 4)
   (= (visit-houses "^v^v^v^v^v") 2)))

;; (visit-houses (read-file-as-string "/home/slytobias/lisp/books/Advent/advent/2015/day3.data")) => 2081

(defclass agent ()
  ((x :initform 0)
   (y :initform 0)))

(defgeneric location (agent)
  (:documentation "Return the location of AGENT"))
(defmethod location ((a agent))
  (with-slots (x y) a
    (list x y)))
(defmethod (setf location) (location (a agent))
  (with-slots (x y) a
    (destructuring-bind (new-x new-y) location
      (setf x new-x
            y new-y))))

(defgeneric leave-present (agent visited)
  (:documentation "Leave a present at the agent's location"))
(defmethod leave-present ((a agent) visited)
  (incf (gethash (location a) visited 0)))

(defgeneric update-location (agent direction)
  (:documentation "Update the agent's location in the given direction."))
;; (defmethod update-location ((a agent) (direction character))
;;   (with-slots (x y) a
;;     (setf x (update-x x direction)
;;           y (update-y y direction))))
(defmethod update-location ((a agent) (direction character))
  (with-slots (x y) a
    (ecase direction
      (#\^ (incf y))
      (#\v (decf y))
      (#\> (incf x))
      (#\< (decf x)))) )

(defun print-visits (visited)
  (print ">>>")
  (loop for k being the hash-keys in visited using (hash-value v)
        do (print (list k v))))

;; (defun update-x (x ch)
;;   (case ch
;;     (#\> (1+ x))
;;     (#\< (1- x))
;;     (otherwise x)))

;; (defun update-y (y ch)
;;   (case ch
;;     (#\^ (1+ y))
;;     (#\v (1- y))
;;     (otherwise y)))

(defun robo-visit (s)
  (let ((visited (make-hash-table :test #'equal))
        (stream (make-string-input-stream s)))
    (labels ((visit (q)
               (let ((ch (read-char stream nil nil)))
                 (cond ((null ch) (hash-table-count visited))
                       (t (let ((agent (collections:dequeue q)))
                            (update-location agent ch)
                            (leave-present agent visited)
                            (collections:enqueue q agent)
                            (visit q)))) )))
      (let ((queue (collections:make-linked-queue))
            (santa (make-instance 'agent))
            (robot (make-instance 'agent)))
        (leave-present santa visited)
        (leave-present robot visited)
        (collections:enqueue queue santa)
        (collections:enqueue queue robot)
        (visit queue)))) )

(deftest test-robo-visit ()
  (check
   (= (robo-visit "^v") 3)
   (= (robo-visit "^>v<") 3)
   (= (robo-visit "^v^v^v^v^v") 11)))
   
;; (robo-visit (read-file-as-string "/home/slytobias/lisp/books/Advent/advent/2015/day3.data")) => 2341
        
