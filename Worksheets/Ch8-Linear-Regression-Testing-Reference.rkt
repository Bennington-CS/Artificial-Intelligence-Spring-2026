#lang racket
(require rackunit)

;;;;;
;;;;; Machine Learning: Linear Regression with Testing
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

;;;;
;;;; Testing / Model Evaluation (no changes to the above to add testing)
;;;;

;;;
;;; After training a model, we need to evaluate it on data it has never seen.
;;; To start, let's suppose you are using a 15-student dataset: small-data, plus
;;; the 5 students below in test-data.
;;;
;;; Reference: Grokking AI Algorithms, Chapter 8, Tables 8.13–8.14, pp. 253–254
;;;

;;; Test dataset: 5 students that are NOT in small-data.
(define test-data
  '((3.3 42) (3.8 35) (6.1 67) (7.8 86) (8.9 95)))

;;; predict-all : Number Number (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Given a trained model (slope m and intercept c) and a dataset, predict the y value
;;; for each x in the dataset.
;;;
;;; m    : slope of the regression line
;;; c    : y-intercept of the regression line
;;; data : list of (x y) pairs
;;;
(define (predict-all m c data)
  (map (lambda (pair) (predict m c (car pair))) data))

(check-equal? (predict-all 2.0 1.0 '((1 10) (2 20) (3 30))) '(3.0 5.0 7.0)
              "y = 1 + 2x predicts 3, 5, 7 for x = 1, 2, 3")
(check-equal? (predict-all 0.0 5.0 '((1 10) (100 20))) '(5.0 5.0)
              "Slope 0 always predicts the intercept")

;;; sum-of-squares : (List-of Number) -> Number
;;;
;;; Sum the squares of a list of numbers.
;;;
;;; lst : list of numbers
;;;
(define (sum-of-squares lst)
  (apply + (map sqr lst)))

(check-equal? (sum-of-squares '(3 4)) 25)
(check-equal? (sum-of-squares '(1 2 3)) 14)
(check-equal? (sum-of-squares '(0 0 0)) 0)

;;; r-squared : (List-of Number) (List-of Number) -> Number
;;;
;;; Calculate R² (R-squared) to measure how well predictions explain the variance
;;; in the actual data.
;;;
;;; Formula (from textbook, p. 253):
;;;
;;;            sum of (predicted y – mean of actual y)²
;;;   R²  =  ──────────────────────────────────────────
;;;            sum of (actual y – mean of actual y)²
;;;
;;; According to the book, an R² of 1.0 means the model perfectly accounts
;;; for all variance.  But this is not quite true, because for test data the
;;; predictions can be spread out more than the actuals; in which case, you get
;;; a result greater than 1.  We can stick with the version of the book, but
;;; just be aware that it tracks overshooting as well as undershooting.
;;;
;;; If you want a version that always gives you a result < 1, which tends to
;;; be what people use for an R² test, it's this:
;;;
;;;                   sum of (actual y - predicted y)²
;;;   R²  =  1 - ──────────────────────────────────────────
;;;                sum of (actual y – mean of actual y)²
;;;
;;; But for our definitions below, we'll stick to what we have in the book (the
;;; first formula).
;;;
;;; actual-ys    : list of actual y values from the test set
;;; predicted-ys : list of predicted y values from the model
;;;
(define (r-squared actual-ys predicted-ys)
  (let* ([mean-y      (mean actual-ys)])
    (/ (sum-of-squares (map (lambda (p) (- p mean-y)) predicted-ys))
       (sum-of-squares (map (lambda (a) (- a mean-y)) actual-ys)))))

;; Perfect predictions → R² = 1.0
(check-within (r-squared '(10 20 30) '(10 20 30)) 1.0 0.001
              "Perfect predictions yield R² = 1")

;; Predicting the mean every time → R² = 0.0
(check-within (r-squared '(10 20 30) '(20 20 20)) 0.0 0.001
              "Predicting the mean yields R² = 0")

;; Test with our study hours data: train on small-data, test on test-data
(check-within (let* ([m     (slope small-data)]
                     [c     (intercept small-data)]
                     [preds (predict-all m c test-data)])
                (exact->inexact (r-squared (ys test-data) preds)))
              0.90 0.01
              "Study hours model achieves R² ≈ 0.90 on test data")

;;; evaluate : (List-of (List Number Number)) (List-of (List Number Number)) -> Void
;;;
;;; Train a model on train-data, test it on test-data, and display the results
;;; including predictions, Differences, and R².
;;;
;;; train-data : list of (x y) pairs used to build the model
;;; test-data  : list of (x y) pairs used to evaluate the model
;;;
(define (evaluate train-data test-data)
  (let* ([m     (slope train-data)]
         [c     (intercept train-data)]
         [preds (predict-all m c test-data)]
         [r2    (r-squared (ys test-data) preds)])

    (displayln "Model Evaluation")
    (displayln "════════════════")
    (displayln (format "  Trained on ~a examples" (length train-data)))
    (displayln (format "  Testing on ~a examples" (length test-data)))
    (displayln (format "  Model: y = ~a + ~ax"
                       (~r (exact->inexact c) #:precision '(= 2))
                       (~r (exact->inexact m) #:precision '(= 2))))
    (newline)

    (displayln "Predictions vs. Actual (test set):")
    (displayln (format "  ~a  ~a  ~a  ~a"
                       (~a "Hours" #:width 7)
                       (~a "Actual" #:width 8)
                       (~a "Predicted" #:width 10)
                       (~a "Difference" #:width 10)))
    (displayln "  ─────────────────────────────────────────")
    (for ([pair (in-list test-data)]
          [pred (in-list preds)])
      (let* ([x     (car pair)]
             [y     (cadr pair)]
             [resid (- y pred)])
        (displayln (format "  ~a  ~a  ~a  ~a"
                           (~a (~r (exact->inexact x) #:precision '(= 1)) #:width 7)
                           (~a y #:width 8)
                           (~a (~r (exact->inexact pred) #:precision '(= 2)) #:width 10)
                           (~a (~r (exact->inexact resid) #:precision '(= 2)) #:width 10)))))
    (newline)
    (displayln (format "  R² = ~a"
                       (~r (exact->inexact r2) #:precision '(= 4))))
    (displayln (format "  The model accounts for ~a% of the variance in the test data."
                       (~r (* 100 (exact->inexact r2)) #:precision '(= 0))))))

;(evaluate small-data test-data)

;;; split-data : (List-of (List Number Number)) Real -> (values (List-of (List Number Number))
;;;                                                             (List-of (List Number Number)))
;;;
;;; Randomly shuffle a dataset, then split it into training data and test data.
;;; The first result is the training portion (the first ratio fraction of the data);
;;; the second is the test portion (the remainder).
;;;
;;; Shuffling before splitting ensures the training and test sets are representative
;;; of the full dataset, rather than biased by whatever order the data was stored in.
;;;
;;; data  : list of (x y) pairs
;;; ratio : fraction of data to use for training (e.g., 0.8 for an 80/20 split)
;;;
(define (split-data data ratio)
  (let* ([shuffled (shuffle data)]
         [n        (inexact->exact (round (* (length shuffled) ratio)))])
    (values (take shuffled n) (drop shuffled n))))

(let-values ([(train test) (split-data '((1 10) (2 20) (3 30) (4 40) (5 50)) 0.8)])
  (check-equal? (length train) 4
                "80% of 5 items is 4 training examples")
  (check-equal? (length test) 1
                "Remaining 20% is 1 test example")
  (check-equal? (sort (append train test) < #:key car)
                '((1 10) (2 20) (3 30) (4 40) (5 50))
                "No data lost: train + test = original data"))

(let-values ([(train test) (split-data big-data 0.8)])
  (check-equal? (length train) 20
                "80% of 25 is 20 training examples")
  (check-equal? (length test) 5
                "20% of 25 is 5 test examples")
  (check-equal? (length (append train test)) 25
                "No data lost in split"))

(let-values ([(train test) (split-data '((1 10) (2 20)) 0.5)])
  (check-equal? (length train) 1
                "50% of 2 items is 1 training example")
  (check-equal? (length test) 1
                "Remaining 50% is 1 test example"))

;;; learn : (List-of (List Number Number)) Real -> (values Number Number Number)
;;;
;;; Split the data into training and test sets, train a linear regression model on the
;;; training set, and evaluate it on the test set. Returns the slope (m), intercept (c),
;;; and R² score.
;;;
;;; This function performs the complete machine learning workflow from the textbook
;;; (Figure 8.15): prepare data (split) → train a model (slope, intercept) → test
;;; the model (R²).
;;;
;;; data  : list of (x y) pairs
;;; ratio : fraction of data to use for training (e.g., 0.8 for an 80/20 split)
;;;
(define (learn data ratio)
  (let-values ([(train-data test-data) (split-data data ratio)])
    (let* ([m     (slope train-data)]
           [c     (intercept train-data)]
           [preds (predict-all m c test-data)]
           [r2    (r-squared (ys test-data) preds)])
      (values m c (exact->inexact r2)))))

;; Perfectly linear data: any split gives slope 2, intercept 0, R² = 1.0
(let-values ([(m c r2) (learn '((1 2) (2 4) (3 6) (4 8) (5 10)) 0.6)])
  (check-within m  2.0 0.001
                "Perfectly linear data has slope 2 regardless of split")
  (check-within c  0.0 0.001
                "Perfectly linear data has intercept 0 regardless of split")
  (check-within r2 1.0 0.001
                "Perfectly linear data has R² = 1.0 regardless of split"))

;; Study hours data: slope should be positive, R² should be strong
(let-values ([(m c r2) (learn big-data 0.8)])
  (check-equal? (positive? m) #t
                "More hours → higher scores, so slope is positive")
  (check-equal? (> r2 0.5) #t
                "R2 should produce more than half"))

;(learn small-data 0.8)
;(learn big-data 0.8)
