#lang racket
(require rackunit)

;;;;;
;;;;; Machine Learning: Perceptron for the Iris Dataset
;;;;; Worksheet
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;
;;; Implement the Perceptron learning algorithm to classify iris flowers as setosa
;;; or versicolor based on petal length and petal width. The Perceptron learns weights
;;; by processing training examples one at a time, adjusting weights after each example
;;; to reduce the error between its prediction and the actual label.
;;;
;;; Reference: Grokking AI Algorithms, Chapter 9, pp. 283–286
;;;            UCI Iris Dataset (https://archive.ics.uci.edu/dataset/53/iris)
;;;

;;;;
;;;; Constants
;;;;

;;; Dataset: Iris (setosa vs. versicolor), 10 flowers
;;; Each entry is (feature1 feature2 label)
;;; Label: 0 = setosa, 1 = versicolor
(define iris-data-small
  '((1.4 0.2 0) (1.3 0.2 0) (1.5 0.4 0) (1.7 0.4 0) (1.4 0.3 0)
    (4.7 1.4 1) (4.5 1.5 1) (3.3 1.0 1) (4.0 1.3 1) (3.5 1.0 1)))


;;; Dataset: Iris (setosa vs. versicolor), 99 flowers
;;; Each entry is (feature1 feature2 label)
;;; Label: 0 = setosa, 1 = versicolor
(define iris-data-large
  '((1.4 0.2 0) (1.4 0.2 0) (1.3 0.2 0) (1.5 0.2 0) (1.4 0.2 0)
    (1.7 0.4 0) (1.4 0.3 0) (1.5 0.2 0) (1.4 0.2 0) (1.5 0.1 0)
    (1.5 0.2 0) (1.6 0.2 0) (1.4 0.1 0) (1.1 0.1 0) (1.2 0.2 0)
    (1.5 0.4 0) (1.3 0.4 0) (1.4 0.3 0) (1.7 0.3 0) (1.5 0.3 0)
    (1.7 0.2 0) (1.5 0.4 0) (1 0.2 0)   (1.7 0.5 0) (1.9 0.2 0)
    (1.6 0.2 0) (1.6 0.4 0) (1.5 0.2 0) (1.4 0.2 0) (1.6 0.2 0)
    (1.6 0.2 0) (1.5 0.4 0) (1.5 0.1 0) (1.4 0.2 0) (1.5 0.1 0)
    (1.2 0.2 0) (1.3 0.2 0) (1.5 0.1 0) (1.3 0.2 0) (1.5 0.2 0)
    (1.3 0.3 0) (1.3 0.3 0) (1.3 0.2 0) (1.6 0.6 0) (1.9 0.4 0)
    (1.4 0.3 0) (1.6 0.2 0) (1.4 0.2 0) (1.5 0.2 0) (1.4 0.2 0)
    (4.7 1.4 1) (4.5 1.5 1) (4.9 1.5 1) (4 1.3 1)   (4.6 1.5 1)
    (4.5 1.3 1) (4.7 1.6 1) (3.3 1 1)   (4.6 1.3 1) (3.9 1.4 1)
    (3.5 1 1)   (4.2 1.5 1) (4 1 1)     (4.7 1.4 1) (3.6 1.3 1)
    (4.4 1.4 1) (4.5 1.5 1) (4.1 1 1)   (4.5 1.5 1) (3.9 1.1 1)
    (4.8 1.8 1) (4 1.3 1)   (4.9 1.5 1) (4.7 1.2 1) (4.3 1.3 1)
    (4.4 1.4 1) (4.8 1.4 1) (5 1.7 1)   (4.5 1.5 1) (3.5 1 1)
    (3.8 1.1 1) (3.7 1 1)   (3.9 1.2 1) (5.1 1.6 1) (4.5 1.5 1)
    (4.5 1.6 1) (4.7 1.5 1) (4.4 1.3 1) (4.1 1.3 1) (4 1.3 1)
    (4.4 1.2 1) (4.6 1.4 1) (4 1.2 1)   (3.3 1 1)   (4.2 1.3 1)
    (4.2 1.2 1) (4.2 1.3 1) (4.3 1.3 1) (3 1.1 1)))


;;; Specify which dataset to use
(define dataset (shuffle iris-data-small))


;;; Feature ranges for min-max scaling, one (min max) pair for each feature.
(define feature-ranges
  (let ([col1 (map car  dataset)]
        [col2 (map cadr dataset)])
    (list (list (apply min col1) (apply max col1))
          (list (apply min col2) (apply max col2)))))


;;; Default learning rate and number of epochs
(define default-lr     0.1)
(define default-epochs 100)


;;;;
;;;; Wishes (helper functions)
;;;;

;;; features : (List-of Any) -> (List-of Number)
;;;
;;; Extract the feature values from a example (the first two elements).
;;;
(define (features example)
  (list (first example)
        (second example)))

(check-equal? (features '(1.4 0.2 0)) '(1.4 0.2))
(check-equal? (features '(4.7 1.4 1)) '(4.7 1.4))

;;; label : (List-of Any) -> Number
;;;
;;; Extract the label value from a example (the last element).
;;;
(define (label example)
  (last example))

(check-equal? (label '(1.4 0.2 0)) 0)
(check-equal? (label '(4.7 1.4 1)) 1)

;;; min-max-scale : Number Number Number -> Number
;;;
;;; Scale a value to the range [0, 1] using min-max normalization.
;;;
;;;   scaled = (value – feature-min) / (feature-max – feature-min)
;;;
;;; The minimum maps to 0, the maximum maps to 1, and all other values fall
;;; proportionally between them.
;;;
;;; value       : the raw feature value
;;; feature-min : the minimum value of this feature in the dataset
;;; feature-max : the maximum value of this feature in the dataset
;;;
#;
(define (min-max-scale value feature-min feature-max)
  ...)

;; (check-equal? (min-max-scale 5 0 10)  1/2 "Midpoint of [0, 10] should scale to 0.5")
;; (check-equal? (min-max-scale 0 0 10)  0   "Minimum should scale to 0")
;; (check-equal? (min-max-scale 10 0 10) 1   "Maximum should scale to 1")
;; (check-equal? (min-max-scale 3 1 5)   1/2 "Midpoint of [1, 5] should scale to 0.5")

;;; scale-inputs : (List-of Number) -> (List-of Number)
;;;
;;; Scale a list of raw feature values to the range [0, 1] using min-max
;;; normalization. Each feature is scaled by its corresponding entry in
;;; feature-ranges.
;;;
;;; raw-features : list of raw feature values, one per feature
;;;
#;
(define (scale-inputs raw-features)
  ...)

;; (let* ([range1  (first feature-ranges)]
;;        [range2  (second feature-ranges)]
;;        [mins    (list (car range1) (car range2))]
;;        [maxs    (list (cadr range1) (cadr range2))]
;;        [scaled-mins (scale-inputs mins)]
;;        [scaled-maxs (scale-inputs maxs)]
;;        [scaled-mid  (scale-inputs (features (first dataset)))])
;;   (check-equal? scaled-mins '(0.0 0.0)
;;                 "Feature min should scale to 0.0")
;;   (check-equal? scaled-maxs '(1.0 1.0)
;;                 "Feature max should scale to 1.0")
;;   (check-true (and (<= 0 (first scaled-mid))  (<= (first scaled-mid) 1)
;;                    (<= 0 (second scaled-mid)) (<= (second scaled-mid) 1))
;;               "Scaled feature values should stay in [0, 1]"))

;;; sigmoid : Number -> Number
;;;
;;; The sigmoid activation function. Maps any real number to a value between 0 and 1.
;;;
;;;   sigmoid(x) = 1 / (1 + e^(-x))
;;;
;;; x : any real number
;;;
#;
(define (sigmoid x)
  ...)

;(check-equal? (sigmoid 0)   0.5       "sigmoid(0) should be 0.5")
;(check-within (sigmoid 10)  1.0 0.001 "Large positive → close to 1")
;(check-within (sigmoid -10) 0.0 0.001 "Large negative → close to 0")

;;; dot-product : (List-of Number) (List-of Number) -> Number
;;;
;;; Compute the dot product of two equal-length lists of numbers.
;;;
;;; xs : first list of numbers
;;; ys : second list of numbers
;;;
#;
(define (dot-product xs ys)
  ...)

;; (check-equal? (dot-product '(1 2 3) '(4 5 6)) 32
;;               "1×4 + 2×5 + 3×6 should be 32")
;; (check-equal? (dot-product '(0.5 0.5) '(0 0)) 0
;;               "Dot product with zeros should be zero")

;;; perceptron-output : (List-of Number) (List-of Number) Number -> Number
;;;
;;; Compute the forward pass of a Perceptron for one example.
;;; Computes the dot product of scaled inputs and weights, adds the bias,
;;; and applies sigmoid.
;;;
;;; scaled-inputs : list of scaled feature values
;;; weights       : list of weights, one per input
;;; bias          : bias term
;;;
#;
(define (perceptron-output scaled-inputs weights bias)
  ...)

;; (check-equal? (perceptron-output '(0.5 0.5) '(0 0) 0) 0.5
;;               "Zero weights should be sigmoid(0) = 0.5")
;; (check-true (> (perceptron-output '(1 1) '(5 5) -3) 0.5)
;;             "Large positive weighted sum should classify high")
;; (check-true (< (perceptron-output '(0 0) '(5 5) -3) 0.5)
;;             "Large negative bias should classify low")

;;; classify : Number -> Number
;;;
;;; Convert a sigmoid output to a class label (0 or 1) using a threshold of 0.5.
;;;
;;; output : sigmoid output, a number between 0 and 1
;;;
(define (classify output)
  (if (> output 0.5) 1 0))

(check-equal? (classify 0.999) 1 "Above threshold should be 1")
(check-equal? (classify 0.055) 0 "Below threshold should be 0")
(check-equal? (classify 0.5)   0 "At threshold should be 0")
(check-equal? (classify 0.501) 1 "Just above threshold should be 1")

;;;;
;;;; Perceptron Learning
;;;;

;;; perceptron-update : (List Number Number Number) (List Number Number) Number Number
;;;                     -> (values (List Number Number) Number)
;;;
;;; Consume one training example with two features: compute the forward pass,
;;; calculate the error, and produce the updated weights and bias.
;;;
;;; The update rule is:
;;;   error  = label – output
;;;   new w1 = old w1 + learning-rate × error × input1
;;;   new w2 = old w2 + learning-rate × error × input2
;;;   new b  = old b  + learning-rate × error
;;;
;;; (define (perceptron-update example weights bias lr)
;;;   (let* ([scaled ...]
;;;          [actual ...]
;;;          [output ...]
;;;          [error ...])
;;;     (values ... ...)))
;;;
;;; example : (feature1 feature2 label)
;;; weights : (w1 w2)
;;; bias    : current bias term
;;; lr      : learning rate
;;;
#;
(define (perceptron-update example weights bias lr)
  ...)

;; (let-values ([(w b) (perceptron-update '(1.4 0.2 0) '(0.0 0.0) 0.0 1.0)])
;;   (check-true (< (first w) 0)
;;               "Setosa example should push w1 downward from zero")
;;   (check-true (< b 0)
;;               "Setosa example should decrease the bias"))
;;
;; (let-values ([(w b) (perceptron-update '(4.7 1.4 1) '(0.0 0.0) 0.0 1.0)])
;;   (check-true (> (first w) 0)
;;               "Versicolor example should push w1 upward from zero")
;;   (check-true (> (second w) 0)
;;               "Versicolor example should push w2 upward from zero")
;;   (check-true (> b 0)
;;               "Versicolor example should increase the bias"))


;;; perceptron-train : (List-of (List-of Any)) Number Number
;;;                    -> (values (List Number Number) Number)
;;;
;;; Train a Perceptron from scratch by running multiple epochs. Starts with
;;; weights (0.0 0.0) and bias 0.0.
;;;
;;; data   : list of training examples
;;; lr     : learning rate
;;; epochs : number of complete passes through the data
;;;
#;
(define (perceptron-train data lr epochs)
  ...)

;; (let-values ([(w1 b1) (perceptron-train dataset 1.0 10)])
;;   (check-true  (real? (first w1))
;;                "Training should produce a real-valued first weight")
;;   (check-true  (real? (second w1))
;;                "Training should produce a real-valued second weight")
;;   (check-true  (real? b1)
;;                "Training should produce a real-valued bias")
;;   (check-false (and (equal? w1 '(0.0 0.0)) (equal? b1 0.0))
;;                "Training should update the initial zero parameters"))


;;;;
;;;; Evaluation
;;;;

;;; perceptron-accuracy : (List-of (List-of Any)) (List-of Number) Number -> Number
;;;
;;; Count how many examples in the dataset are classified correctly by the
;;; Perceptron with the given weights and bias.
;;;
;;; data    : list of training examples
;;; weights : list of weights, one per feature
;;; bias    : bias term
;;;
#;
(define (perceptron-accuracy data weights bias)
  ...)

;; (check-equal? (perceptron-accuracy dataset '(0 0) 0)
;;               (length (filter (lambda (example) (equal? (label example) 0)) dataset))
;;               "Zero weights predict 0 for everything, so accuracy equals number of setosa examples")
;;
;; (let-values ([(w b) (perceptron-train dataset 1.0 10)])
;;   (check-true (<= 0 (perceptron-accuracy dataset w b))
;;               "Accuracy should be nonnegative")
;;   (check-true (<= (perceptron-accuracy dataset w b) (length dataset))
;;               "Accuracy should not exceed dataset size")
;;   (check-true (>= (perceptron-accuracy dataset w b)
;;                   (perceptron-accuracy dataset '(0 0) 0))
;;               "Trained weights should do at least as well as zero weights"))

;;; perceptron-classify-all : (List-of (List-of Any)) (List-of Number) Number
;;;                           -> (List-of Number)
;;;
;;; Classify every example in the dataset using the Perceptron. Returns a list
;;; of predicted labels (0 or 1).
;;;
;;; data    : list of training examples
;;; weights : list of weights, one per feature
;;; bias    : bias term
;;;
#;
(define (perceptron-classify-all data weights bias)
  ...)

;; (check-equal? (length (perceptron-classify-all dataset '(0 0) 0))
;;               (length dataset)
;;               "Classification should return one label per example")
;;
;; (check-true (andmap (lambda (x) (or (equal? x 0) (equal? x 1)))
;;                     (perceptron-classify-all dataset '(0 0) 0))
;;             "Classifications should be only 0 or 1")
;;
;; (check-equal? (perceptron-classify-all dataset '(0 0) 0)
;;               (make-list (length dataset) 0)
;;               "Zero weights classify every example as 0")

;;;;
;;;; Run it
;;;;

#;
(define (run-perceptron data lr epochs)
  (let-values ([(weights bias) (perceptron-train data lr epochs)])

    (displayln "Perceptron Training Results")
    (displayln "═══════════════════════════")
    (displayln (format "  Learning rate: ~a" lr))
    (displayln (format "  Epochs: ~a"        epochs))
    (displayln (format "  Scaling: min-max normalization"))
    (displayln (format "  Feature ranges: ~a"
                       (map (lambda (r) (format "[~a, ~a]" (car r) (cadr r)))
                            feature-ranges)))
    (displayln (format "  Trained weights: ~a"
                       (map (lambda (w) (~r w #:precision '(= 4)))
                            weights)))
    (displayln (format "  Trained bias:    ~a" (~r bias #:precision '(= 4))))
    (newline)

    (displayln "Classifications:")
    (displayln (format "  ~a  ~a  ~a  ~a"
                       (~a "#" #:width 3)
                       (~a "Features" #:width 14)
                       (~a "Label" #:width 6)
                       (~a "Output" #:width 7)))
    (displayln "  ─────────────────────────────────────")
    (for ([example (in-list data)]
          [i       (in-naturals 1)])
      (let* ([scaled (scale-inputs (features example))]
             [actual (label example)]
             [output (perceptron-output scaled weights bias)]
             [pred   (classify output)]
             [ok?    (if (equal? pred actual) "✓" "✗")])
        (displayln (format "  ~a  ~a  ~a  ~a  ~a"
                           (~a i #:width 3)
                           (~a (features example) #:width 14)
                           (~a actual #:width 6)
                           (~a (~r output #:precision '(= 3)) #:width 7)
                           ok?))))
    (newline)
    (displayln (format "  Accuracy: ~a/~a"
                       (perceptron-accuracy data weights bias)
                       (length data)))))

#;
(run-perceptron dataset default-lr default-epochs)
