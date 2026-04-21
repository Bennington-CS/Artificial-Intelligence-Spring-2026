#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 8: Feedforward Neural Network
;;;;; Reference Implementation
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;
;;; Implement a feedforward neural network that accepts data as a list of feature lists,
;;; scales the data using min-max normalization, infers the network topology from a
;;; structured weight list, and computes a forward pass.  The activation function is
;;; passed as a parameter so it can be swapped (sigmoid, ReLU, etc.).
;;;
;;; Weight representation (list of layers, each layer is a list of neurons, each neuron
;;; is a list of weights — one per input from the previous layer):
;;;
;;;   '(((w11 w12)            ; Layer 1, neuron 1: one weight per input
;;;      (w21 w22))           ; Layer 1, neuron 2
;;;     ((w31 w32)))          ; Layer 2 (output), neuron 1: one weight per hidden output

;;;;
;;;; Constants
;;;;

;;; Example dataset: 5 data points with 2 features each.
;;; Used for testing the scaling functions.
;;;
(define sample-data
  '((1.4 0.2)
    (1.3 0.2)
    (1.5 0.4)
    (4.7 1.4)
    (3.3 1.0)))

;;; Example network weights: 2 inputs -> 2 hidden neurons -> 1 output neuron.
;;; Each neuron's list has one weight per input — no bias term.
;;;
;;; Hidden neuron 1: 0.2*x1 + 0.4*x2
;;; Hidden neuron 2: 0.3*x1 + 0.1*x2
;;; Output neuron:   0.5*h1 + (-0.3)*h2
;;;
(define sample-weights
  '(((0.2 0.4)
     (0.3 0.1))
    ((0.5 -0.3))))

;;;;
;;;; Activation Functions
;;;;

;;; sigmoid : Number -> Number
;;;
;;; The sigmoid activation function.  Maps any real number to a value between 0 and 1.
;;;
;;;   sigmoid(x) = 1 / (1 + e^(-x))
;;;
;;; x : any real number
;;;
;;; (check-within (sigmoid 0) 0.5 0.001)
;;;
(define (sigmoid x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(check-within (sigmoid 0)    0.5   0.001 "sigmoid(0) = 0.5")
(check-within (sigmoid 10)   1.0   0.001 "Large positive → near 1")
(check-within (sigmoid -10)  0.0   0.001 "Large negative → near 0")
(check-within (sigmoid 0.44) 0.608 0.001 "sigmoid(0.44) ≈ 0.608")

;;;;
;;;; Part 1: Scaling
;;;;

;;; min-max-scale : Number Number Number -> Number
;;;
;;; Scale a single value to the range [0, 1].
;;;
;;; scaled = (value - feature-min) / (feature-max - feature-min)
;;;
;;; value       : value of the feature
;;; feature-min : minimum value of this feature in the dataset
;;; feature-max : maximum value of this feature in the dataset
;;;
;;; (check-within (min-max-scale 5 0 10) 0.5 0.001)
;;;
(define (min-max-scale value feature-min feature-max)
  (/ (- value feature-min) (- feature-max feature-min)))

(check-within (min-max-scale 5 0 10) 0.5 0.001 "Midpoint of [0, 10] → 0.5")
(check-within (min-max-scale 0 0 10) 0.0 0.001 "Minimum → 0")
(check-within (min-max-scale 10 0 10) 1.0 0.001 "Maximum → 1")
(check-within (min-max-scale 3 1 5) 0.5 0.001 "Midpoint of [1, 5] → 0.5")

;;; compute-feature-ranges : (List-of (List-of Number)) -> (List-of (List Number Number))
;;;
;;; Compute the (min max) pair for each feature column in the dataset.
;;; The dataset is a list of rows, where each row is a list of feature values.
;;;
;;; data : list of rows, each row a list of numbers (all rows same length)
;;;
;;; (check-equal? (compute-feature-ranges '((1 10) (3 20) (5 30)))
;;;               '((1 5) (10 30)))
;;;
(define (compute-feature-ranges data)
  (let ([num-features (length (car data))])
    (for/list ([col (in-range num-features)])
      (let ([vals (map (lambda (row) (list-ref row col)) data)])
        (list (apply min vals) (apply max vals))))))

(check-equal? (compute-feature-ranges '((1 10) (3 20) (5 30)))
              '((1 5) (10 30))
              "Two features: mins and maxes computed per column")
(check-equal? (compute-feature-ranges sample-data)
              '((1.3 4.7) (0.2 1.4))
              "Sample data ranges")

;;; scale-row : (List-of Number) (List-of (List Number Number)) -> (List-of Number)
;;;
;;; Scale one row of feature values using precomputed feature ranges.
;;; Each feature value is scaled by its corresponding (min max) range.
;;;
;;; row    : list of feature values
;;; ranges : list of (min max) pairs, one per feature
;;;
;;; (check-within (scale-row '(3 20) '((1 5) (10 30))) '(0.5 0.5) 0.001)
;;;
(define (scale-row row ranges)
  (map (lambda (val range)
         (min-max-scale val (car range) (cadr range)))
       row
       ranges))

(check-within (scale-row '(3 20) '((1 5) (10 30))) '(0.5 0.5) 0.001
              "Midpoints scale to 0.5")
(check-within (scale-row '(1 10) '((1 5) (10 30))) '(0.0 0.0) 0.001
              "Minimums scale to 0.0")
(check-within (scale-row '(5 30) '((1 5) (10 30))) '(1.0 1.0) 0.001
              "Maximums scale to 1.0")

;;; scale-data : (List-of (List-of Number)) -> (values (List-of (List-of Number))
;;;                                                    (List-of (List Number Number)))
;;;
;;; Scale an entire dataset using min-max normalization.  Computes the feature ranges
;;; from the data, then scales every row.  Returns two values: the scaled data and
;;; the feature ranges (needed to scale new data points the same way).
;;;
;;; data : list of rows, each row a list of feature values
;;;
(define (scale-data data)
  (let ([ranges (compute-feature-ranges data)])
    (values (map (lambda (row) (scale-row row ranges))
                 data)
            ranges)))

;; Scale sample-data and verify a few rows
(let-values ([(scaled ranges) (scale-data sample-data)])
  (check-equal? ranges '((1.3 4.7) (0.2 1.4))
                "Ranges computed from sample-data")
  (check-within (car scaled) '(0.029 0.0) 0.001
                "First row: PL near min, PW at min")
  (check-within (list-ref scaled 3) '(1.0 1.0) 0.001
                "Fourth row: both features at max")
  (check-within (list-ref scaled 4) '(0.588 0.667) 0.001
                "Fifth row: mid-range values"))

;;;;
;;;; Part 2: Inferring the Topology
;;;;

;;; infer-topology : (List-of (List-of (List-of Number))) -> (List-of Number)
;;;
;;; Infer the network topology from the structured weight list.
;;; Returns a list describing the full architecture: the number of inputs followed
;;; by the number of neurons in each layer.
;;;
;;; For example, a network with 2 inputs, 2 hidden neurons, and 1 output neuron
;;; returns '(2 2 1).
;;;
;;; The number of inputs is determined from the first layer's first neuron:
;;; it has one weight per input.  The number of neurons in each layer is the
;;; length of that layer's list.
;;;
;;; weights : structured weight list (list of layers of neurons of weights)
;;;
;;; (check-equal? (infer-topology sample-weights) '(2 2 1))
;;;
(define (infer-topology weights)
  (let ([num-inputs (length (car (car weights)))]
        [layer-sizes (map length weights)])
    (cons num-inputs layer-sizes)))

(check-equal? (infer-topology sample-weights) '(2 2 1)
              "2 inputs → 2 hidden → 1 output")
(check-equal? (infer-topology '(((1 2 3) (1 2 3) (1 2 3) (1 2 3))
                                ((1 2 3 4) (1 2 3 4))))
              '(3 4 2)
              "3 inputs → 4 hidden → 2 outputs")
(check-equal? (infer-topology '(((5.0 5.0))))
              '(2 1)
              "2 inputs → 1 output (no hidden layer)")

;;;;
;;;; Part 3: Forward Pass
;;;;

;;; summation : (List-of Number) (List-of Number) -> Number
;;;
;;; Compute the summation (dot product) of two equal-length lists of numbers.
;;;
;;; xs : first list of numbers
;;; ys : second list of numbers
;;;
;;; (check-within (summation '(1 2 3) '(4 5 6)) 32.0 0.001)
;;;
(define (summation xs ys)
  (apply + (map * xs ys)))

(check-within (summation '(1 2 3) '(4 5 6)) 32.0 0.001
              "1*4 + 2*5 + 3*6 = 32")
(check-within (summation '(0.5 0.5) '(1.0 1.0)) 1.0 0.001
              "0.5*1 + 0.5*1 = 1")

;;; forward-neuron : (List-of Number) (List-of Number) (Number -> Number) -> Number
;;;
;;; Compute the output of a single neuron.  The neuron computes the dot product of
;;; its inputs and weights, then applies the activation function.
;;;
;;; inputs         : list of input values from the previous layer
;;; neuron-weights : list of weights, one per input
;;; activation     : activation function (e.g. sigmoid)
;;;
;;; (check-within (forward-neuron '(0.6 0.8) '(0.2 0.4) sigmoid) 0.608 0.001)
;;;
(define (forward-neuron inputs neuron-weights activation)
  (activation (summation inputs neuron-weights)))

;; Hidden neuron 1: 0.2*0.6 + 0.4*0.8 = 0.44, sigmoid(0.44) ≈ 0.608
(check-within (forward-neuron '(0.6 0.8) '(0.2 0.4) sigmoid) 0.608 0.001
              "Hidden neuron 1 with sample inputs")
;; Hidden neuron 2: 0.3*0.6 + 0.1*0.8 = 0.26, sigmoid(0.26) ≈ 0.565
(check-within (forward-neuron '(0.6 0.8) '(0.3 0.1) sigmoid) 0.565 0.001
              "Hidden neuron 2 with sample inputs")

;;; forward-layer : (List-of Number) (List-of (List-of Number)) (Number -> Number)
;;;                 -> (List-of Number)
;;;
;;; Compute the output of one layer of neurons.  Each neuron in the layer receives
;;; the same inputs and produces one output value.  Returns a list of outputs, one
;;; per neuron.
;;;
;;; inputs        : list of input values (from previous layer or the network input)
;;; layer-weights : list of neuron weight lists for this layer
;;; activation    : activation function
;;;
;;; (check-within (forward-layer '(0.6 0.8) (car sample-weights) sigmoid)
;;;               '(0.608 0.565) 0.001)
;;;
(define (forward-layer inputs layer-weights activation)
  (map (lambda (neuron-weights)
         (forward-neuron inputs neuron-weights activation))
       layer-weights))

(check-within (forward-layer '(0.6 0.8) (car sample-weights) sigmoid)
              '(0.608 0.565) 0.001
              "Hidden layer produces two outputs")
;; With zero inputs and no bias, every neuron gets sigmoid(0) = 0.5
(check-within (forward-layer '(0.0 0.0) (car sample-weights) sigmoid)
              '(0.5 0.5) 0.001
              "Zero inputs, no bias: all neurons output 0.5")

;;; forward-pass : (List-of Number) (List-of (List-of (List-of Number)))
;;;                (Number -> Number) -> (List-of Number)
;;;
;;; Compute the full forward pass through all layers of the network.  The output of
;;; each layer becomes the input to the next layer.  Returns the output of the final
;;; layer as a list (even if there is only one output neuron).
;;;
;;; inputs     : list of input values (one per network input)
;;; weights    : structured weight list (list of layers of neurons of weights)
;;; activation : activation function
;;;
;;; (check-within (forward-pass '(0.6 0.8) sample-weights sigmoid)
;;;               '(0.534) 0.001)
;;;
(define (forward-pass inputs weights activation)
  (foldl (lambda (layer-weights current-inputs)
           (forward-layer current-inputs layer-weights activation))
         inputs
         weights))

;; Full forward pass: 2 inputs → 2 hidden → 1 output
(check-within (forward-pass '(0.6 0.8) sample-weights sigmoid)
              '(0.534) 0.001
              "Full forward pass with sample weights")
(check-within (forward-pass '(0.0 0.0) sample-weights sigmoid)
              '(0.525) 0.001
              "Zero inputs: sigmoid(0) through every neuron")
(check-within (forward-pass '(1.0 1.0) sample-weights sigmoid)
              '(0.536) 0.001
              "Unit inputs through full network")

;; Deeper network: 2 inputs → 3 hidden → 2 hidden → 1 output
(define deep-weights
  '(((0.1 0.2) (0.3 0.4) (-0.1 0.5))
    ((0.2 -0.1 0.3) (0.1 0.4 -0.2))
    ((0.5 0.5))))

(check-equal? (infer-topology deep-weights) '(2 3 2 1)
              "Deep network: 2 inputs → 3 → 2 → 1")
(check-within (forward-pass '(0.5 0.5) deep-weights sigmoid)
              '(0.634) 0.001
              "Forward pass through deeper network")

;;;;
;;;; Putting It All Together
;;;;

;;; classify : (List-of Number) (List-of (List-of (List-of Number)))
;;;            (List-of (List Number Number)) (Number -> Number) -> (List-of Number)
;;;
;;; Scale a data point using precomputed feature ranges, then compute the forward 
;;; pass through the network.  This is the main entry point for using a trained 
;;; network on new data.
;;;
;;; inputs     : list of feature values (unscaled)
;;; weights    : structured weight list
;;; ranges     : feature ranges from scale-data, used to scale the input
;;; activation : activation function
;;;
(define (classify inputs weights ranges activation)
  (forward-pass (scale-row inputs ranges) weights activation))

;; Classify data points through the 2→2→1 network
(let ([ranges '((1.3 4.7) (0.2 1.4))])
  (check-within (classify '(1.4 0.2) sample-weights ranges sigmoid)
                '(0.525) 0.001
                "Near-minimum features → near sigmoid(0)")
  (check-within (classify '(4.7 1.4) sample-weights ranges sigmoid)
                '(0.536) 0.001
                "Maximum features → slightly higher output"))

;;; classify-all : (List-of (List-of Number)) (List-of (List-of (List-of Number)))
;;;                (List-of (List Number Number)) (Number -> Number)
;;;                -> (List-of (List-of Number))
;;;
;;; Classify every data point in a dataset.
;;;
;;; data       : list of feature rows
;;; weights    : structured weight list
;;; ranges     : feature ranges
;;; activation : activation function
;;;
(define (classify-all data weights ranges activation)
  (map (lambda (row) (classify row weights ranges activation))
       data))

;; Classify all sample data
(let* ([ranges  '((1.3 4.7) (0.2 1.4))]
       [results (classify-all sample-data sample-weights ranges sigmoid)])
  (check-within (car results) '(0.525) 0.001
                "First point classified")
  (check-within (list-ref results 3) '(0.536) 0.001
                "Fourth point classified"))

;;; Run it
(displayln "=== Feedforward Neural Network ===")
(displayln "")

(displayln "Network topology:")
(displayln (infer-topology sample-weights))

(displayln "")
(displayln "Forward pass with inputs (0.6, 0.8):")
(displayln (forward-pass '(0.6 0.8) sample-weights sigmoid))

(displayln "")
(displayln "Scaling and classifying sample data:")
(let-values ([(scaled ranges) (scale-data sample-data)])
  (for-each (lambda (orig sc)
              (let ([result (classify orig sample-weights ranges sigmoid)])
                (displayln (format "  ~a → scaled ~a → output ~a"
                                   orig
                                   (map (lambda (v) (/ (round (* v 1000)) 1000.0)) sc)
                                   (map (lambda (v) (/ (round (* v 1000)) 1000.0)) result)))))
            sample-data
            scaled))
