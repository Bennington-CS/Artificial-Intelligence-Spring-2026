#lang racket
(require rackunit)

;;;;;
;;;;; Machine Learning: Linear Regression
;;;;; Reference Implementation
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
(define (xs data)
  (map car data))

(check-equal? (xs '((1 10) (2 20) (3 30))) '(1 2 3))

;;; ys : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Extract the y values (second element) from a dataset of pairs.
;;;
;;; data : list of (x y) pairs
;;;
;;; (check-equal? (ys '((1 10) (2 20) (3 30))) '(10 20 30))
;;;
(define (ys data)
  (map cadr data))

(check-equal? (ys '((1 10) (2 20) (3 30))) '(10 20 30))

;;; mean : (List-of Number) -> Number
;;;
;;; Calculate the arithmetic mean (average) of a list of numbers.
;;;
;;; lst : non-empty list of numbers
;;;
;;; (check-within (mean '(1 2 3 4 5)) 3.0 0.001)
;;;
(define (mean lst)
  (/ (apply + lst) (length lst)))

(check-within (mean '(1 2 3 4 5)) 3.0 0.001)
(check-within (mean '(10 20))     15.0 0.001)
(check-within (mean (xs big-data)) 5.012 0.001)
(check-within (mean (ys big-data)) 51.48 0.01)

;;; x-minus-mean-x : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the deviation of each x value from the mean of x.
;;; Each element in the result is (x - mean of x).
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (x-minus-mean-x '((1 10) (2 20) (3 30))) '(-1 0 1))
;;;
(define (x-minus-mean-x data)
  (let ([mean-x (mean (xs data))])
    (map (lambda (pair) (- (car pair) mean-x)) data)))

(check-equal? (x-minus-mean-x '((1 10) (2 20) (3 30))) '(-1 0 1))
(check-equal? (x-minus-mean-x '((10 5) (20 5) (30 5))) '(-10 0 10))

;;; y-minus-mean-y : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the deviation of each y value from the mean of y.
;;; Each element in the result is (y - mean of y).
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (y-minus-mean-y '((1 10) (2 20) (3 30))) '(-10 0 10))
;;;
(define (y-minus-mean-y data)
  (let ([mean-y (mean (ys data))])
    (map (lambda (pair) (- (cadr pair) mean-y)) data)))

(check-equal? (y-minus-mean-y '((1 10) (2 20) (3 30))) '(-10 0 10))
(check-equal? (y-minus-mean-y '((1 4) (2 5) (3 6))) '(-1 0 1))

;;; x-minus-mean-x-squared : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the squared deviation of each x value
;;; from the mean of x. Each element in the result is (x - mean of x)^2.
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (x-minus-mean-x-squared '((1 10) (2 20) (3 30))) '(1 0 1))
;;;
(define (x-minus-mean-x-squared data)
  (map sqr (x-minus-mean-x data)))

(check-equal? (x-minus-mean-x-squared '((1 10) (2 20) (3 30))) '(1 0 1))
(check-equal? (x-minus-mean-x-squared '((10 5) (20 5) (30 5))) '(100 0 100))

;;; x-minus-mean-x-times-y-minus-mean-y : (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a dataset of (x y) pairs, return the product of the x and y deviations from
;;; their respective means. Each element is (x - mean of x) * (y - mean of y).
;;;
;;; data : non-empty list of (x y) pairs
;;;
;;; (check-equal? (x-minus-mean-x-times-y-minus-mean-y '((1 10) (2 20) (3 30)))
;;;              '(10 0 10))
;;;
(define (x-minus-mean-x-times-y-minus-mean-y data)
  (map * (x-minus-mean-x data) (y-minus-mean-y data)))

(check-equal? (x-minus-mean-x-times-y-minus-mean-y '((1 10) (2 20) (3 30)))
              '(10 0 10))
;; When x and y move in the same direction, products are positive (positive correlation)
(check-equal? (x-minus-mean-x-times-y-minus-mean-y '((1 2) (2 4) (3 6)))
              '(2 0 2))

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
;;;
(define (slope data)
  (/ (apply + (x-minus-mean-x-times-y-minus-mean-y data))
     (apply + (x-minus-mean-x-squared data))))

(check-within (slope '((1 2) (2 4) (3 6))) 2.0 0.001
              "Perfect linear data with slope 2")
(check-within (slope '((1 5) (2 5) (3 5))) 0.0 0.001
              "Flat data has slope 0")

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
;;;
(define (intercept data)
  (- (mean (ys data)) (* (slope data) (mean (xs data)))))

(check-within (intercept '((1 2) (2 4) (3 6))) 0.0 0.001
              "Line through origin has intercept 0")
(check-within (intercept '((1 3) (2 5) (3 7))) 1.0 0.001
              "y = 2x + 1 has intercept 1")

;;; predict : Number Number Number -> Number
;;;
;;; Predict a y value given the slope (m), intercept (c), and an x value.
;;;
;;;   predicted y = c + m * x
;;;
;;; m : slope of the regression line
;;; c : y-intercept of the regression line
;;; x : the x value to predict for
;;;
;;; (check-within (predict 2.0 1.0 3) 7.0 0.001)
;;;
(define (predict m c x)
  (+ c (* m x)))

(check-within (predict 2.0 1.0 3) 7.0 0.001)
(check-within (predict 0.0 5.0 100) 5.0 0.001
              "Slope 0 always predicts the intercept")

;;;;
;;;; Run it
;;;;
(define (run data)
  (let ([m (slope     data)]
        [c (intercept data)])

    (displayln "Linear Regression Results:")
    (displayln "─────────────────────────")
    (displayln (format "  Slope (m):     ~a" (exact->inexact m)))
    (displayln (format "  Intercept (c): ~a" (exact->inexact c)))
    (displayln (format "  Regression line: y = ~a + ~ax"
                      (~r (exact->inexact c) #:precision '(= 2))
                      (~r (exact->inexact m) #:precision '(= 2))))
    (newline)

    ;; Predict for a few sample values
    (displayln "Predictions:")
    (for ([hours '(4 6 8 10)])
      (displayln (format "  ~a hours → predicted score: ~a"
                        hours
                        (~r (exact->inexact (predict m c hours)) #:precision '(= 1)))))
    (newline)

    ;; Show predictions vs actual
    (displayln "Predictions vs. Actual:")
    (displayln (format "  ~a  ~a  ~a  ~a"
                      (~a "Hours" #:width 7)
                      (~a "Actual" #:width 8)
                      (~a "Predicted" #:width 10)
                      (~a "Difference" #:width 10)))
    (displayln "  ─────────────────────────────────────")
    (for ([pair (in-list data)])
      (let* ([x     (car pair)]
            [y     (cadr pair)]
            [pred  (predict m c x)]
            [resid (- y pred)])
        (displayln (format "  ~a  ~a  ~a  ~a"
                          (~a (~r (exact->inexact x) #:precision '(= 1)) #:width 7)
                          (~a y #:width 8)
                          (~a (~r (exact->inexact pred) #:precision '(= 2)) #:width 10)
                          (~a (~r (exact->inexact resid) #:precision '(= 2)) #:width 10)))))))

;; (run small-data)
;; (run big-data)
