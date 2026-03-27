#lang racket
(require rackunit)

;;;;;
;;;;; Machine Learning: Linear Regression
;;;;; Worksheet
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;
;;; Implement the least-squares linear regression algorithm to find the best-fit line for a
;;; two-variable dataset. Given study hours (x) and exam scores (y), find the regression
;;; line y = c + mx that minimizes the total squared distance to all points.
;;;
;;; Reference: Grokking AI Algorithms, Chapter 8, Figure 8.12
;;;

;;;
;;; Preperatory Work: https://tinyurl.com/yfr8dawb
;;;

;;;;
;;;; Constants
;;;;

;;; Dataset: Study Hours vs. Exam Scores (25 students: big-data; 10 students: small-data)
;;; Each entry is (hours score)

;;; Large dataset: 25-student set
(define big-data
  '((2.5 21) (5.1 47) (3.2 27) (8.5 75) (3.5 30)
    (1.5 20) (9.2 88) (5.5 60) (8.3 81) (2.7 25)
    (7.7 85) (5.9 62) (4.5 41) (3.3 42) (1.1 17)
    (8.9 95) (2.5 30) (1.9 24) (6.1 67) (7.4 69)
    (2.7 30) (4.8 54) (3.8 35) (6.9 76) (7.8 86)))

;;; Small dataset: 10-student subset used in the lesson
(define small-data
  '((1.5 20) (2.5 21) (3.5 30) (4.5 41) (5.1 47)
    (5.9 62) (6.9 76) (7.7 85) (8.5 75) (9.2 88)))

;;;;
;;;; Wishes (helper functions)
;;;;

;;; xs : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Extract the x values (first element) from a dataset of pairs.
;;;
;;; data : list of (x y) pairs
;;;
;;; (check-equal? (xs '((1 10) (2 20) (3 30))) '(1 2 3))
;;;
#;
(define (xs data)
  ...)

;;; ys : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Extract the y values (second element) from a dataset of pairs.
;;;
;;; data : list of (x y) pairs
;;;
;;; (check-equal? (ys '((1 10) (2 20) (3 30))) '(10 20 30))
;;;
#;
(define (ys data)
  ...)

;;; mean : (List-of Number) -> Number
;;;
;;; Calculate the arithmetic mean (average) of a list of numbers.
;;;
;;; lst : non-empty list of numbers
;;;
;;; (check-within (mean '(1 2 3 4 5)) 3.0 0.001)
;;; (check-within (mean '(10 20))     15.0 0.001)
;;; (check-within (mean (xs big-data)) 5.012 0.001)
;;; (check-within (mean (ys big-data)) 51.48 0.01)
;;;
#;
(define (mean lst)
  ...)

;;; x-minus-mean-x : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the deviation of each x value from the mean of x.
;;; Each element in the result is (x - mean of x).
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (x-minus-mean-x '((1 10) (2 20) (3 30))) '(-1 0 1))
;;; (check-equal? (x-minus-mean-x '((10 5) (20 5) (30 5))) '(-10 0 10))
;;;
#;
(define (x-minus-mean-x data)
  ...)

;;; y-minus-mean-y : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the deviation of each y value from the mean of y.
;;; Each element in the result is (y - mean of y).
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (y-minus-mean-y '((1 10) (2 20) (3 30))) '(-10 0 10))
;;; (check-equal? (y-minus-mean-y '((1 4) (2 5) (3 6))) '(-1 0 1))
;;;
#;
(define (y-minus-mean-y data)
  ...)

;;; x-minus-mean-x-squared : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the squared deviation of each x value
;;; from the mean of x. Each element in the result is (x - mean of x)^2.
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (x-minus-mean-x-squared '((1 10) (2 20) (3 30))) '(1 0 1))
;;; (check-equal? (x-minus-mean-x-squared '((10 5) (20 5) (30 5))) '(100 0 100))
;;;
#;
(define (x-minus-mean-x-squared data)
  ...)

;;; x-minus-mean-x-times-y-minus-mean-y : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the product of the x and y deviations from
;;; their respective means. Each element is (x - mean of x) * (y - mean of y).
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (x-minus-mean-x-times-y-minus-mean-y '((1 10) (2 20) (3 30)))
;;;              '(10 0 10))
;;; (check-equal? (x-minus-mean-x-times-y-minus-mean-y '((1 2) (2 4) (3 6)))
;;;              '(2 0 2))
;;;
#;
(define (x-minus-mean-x-times-y-minus-mean-y data)
  ...)

;;;;
;;;; Linear Regression
;;;;

;;; slope : (List-of (List Number Number)) -> Number
;;;
;;; Calculate the slope (m) of the least-squares regression line.
;;;
;;; The slope formula is:
;;;   m = sum of (x - mean of x) * (y - mean of y)
;;;       ────────────────────────────────────────
;;;              sum of (x - mean of x)^2
;;;
;;; The numerator measures how x and y co-vary (move together).
;;; The denominator measures the total spread of x.
;;; The ratio tells us: for each unit of x, how much does y change?
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-within (slope '((1 2) (2 4) (3 6))) 2.0 0.001)
;;; (check-within (slope '((1 5) (2 5) (3 5))) 0.0 0.001)
;;;
#;
(define (slope data)
  ...)

;;; intercept : (List-of (List Number Number)) -> Number
;;;
;;; Calculate the y-intercept (c) of the least-squares regression line.
;;;
;;; The regression line passes through (mean of x, mean of y), so:
;;;   mean of y = c + m * mean of x
;;;           c = mean of y - m * mean of x
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-within (intercept '((1 2) (2 4) (3 6))) 0.0 0.001)
;;; (check-within (intercept '((1 3) (2 5) (3 7))) 1.0 0.001)
;;;
#;
(define (intercept data)
  ...)

;;;;
;;;; Run it
;;;;
#;
(define (run data)
  (list (slope data) (intercept data)))

;; (run small-data)
;; (run big-data)
