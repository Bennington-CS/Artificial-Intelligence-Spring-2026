#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 9: Feedforward Neural Network with Backpropagation
;;;;; Reference Implementation
;;;;;
;;;;; A configurable feedforward neural network.  The user specifies:
;;;;;   - the number of input features (from the dataset)
;;;;;   - the network topology as a list of layer sizes
;;;;;   - a weight initialization function
;;;;;   - an activation function
;;;;;   - an error (loss) function
;;;;;
;;;;; No bias terms — weights only.
;;;;;

;;;;
;;;; Activation Functions
;;;;

;;; sigmoid : Number -> Number
;;;
;;; Compute the sigmoid activation function.  Maps any real number to the
;;; range (0, 1).
;;;
;;; Strategy: function composition — apply the sigmoid formula directly.
;;;
;;;   sigmoid(x) = 1 / (1 + e^(-x))
;;;
;;; x : any real number
;;;
(define (sigmoid x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(check-within (sigmoid 0)    0.5   0.001 "sigmoid(0) = 0.5")
(check-within (sigmoid 10)   1.0   0.001 "Large positive → near 1")
(check-within (sigmoid -10)  0.0   0.001 "Large negative → near 0")
(check-within (sigmoid 0.44) 0.608 0.001 "sigmoid(0.44) ≈ 0.608")

;;; sigmoid-derivative : Number -> Number
;;;
;;; Compute the derivative of the sigmoid function, given a value that has
;;; ALREADY been passed through sigmoid.  If output = sigmoid(x), then:
;;;
;;;   sigmoid'(x) = output * (1 - output)
;;;
;;; This is the form used in the book's pseudocode: sigmoid_derivative(output).
;;;
;;; Strategy: function composition — apply the derivative formula directly.
;;;
;;; output : a value that is already the result of applying sigmoid
;;;
(define (sigmoid-derivative output)
  (* output (- 1 output)))

(check-within (sigmoid-derivative 0.5)   0.25  0.001 "Maximum derivative at 0.5")
(check-within (sigmoid-derivative 0.0)   0.0   0.001 "Derivative at 0 is 0")
(check-within (sigmoid-derivative 1.0)   0.0   0.001 "Derivative at 1 is 0")
(check-within (sigmoid-derivative 0.608) 0.238 0.001 "Derivative at 0.608")
(check-within (sigmoid-derivative 0.534) 0.249 0.001 "Derivative at 0.534")

;;;;
;;;; Part 1: Scaling
;;;;

;;; min-max-scale : Number Number Number -> Number
;;;
;;; Scale a single value to the range [0, 1] using min-max normalization.
;;;
;;; Strategy: function composition — apply the scaling formula directly.
;;;
;;;   scaled = (value - feature-min) / (feature-max - feature-min)
;;;
;;; value       : the raw feature value to scale
;;; feature-min : minimum value of this feature in the dataset
;;; feature-max : maximum value of this feature in the dataset
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
;;; Strategy: use for/list over column indices; for each column, extract the
;;; values from every row with map and apply min/max.
;;;
;;; data : list of rows, each row a list of numbers (all rows same length)
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
;;; Strategy: use map over parallel lists (row values and their ranges),
;;; applying min-max-scale to each pair.
;;;
;;; row    : list of feature values (one per feature)
;;; ranges : list of (min max) pairs, one per feature
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
;;; Scale an entire dataset using min-max normalization.  Computes the feature
;;; ranges from the data, then scales every row.  Returns two values: the scaled
;;; data and the feature ranges (needed to scale new data points the same way).
;;;
;;; Strategy: function composition — compute ranges once with
;;; compute-feature-ranges, then map scale-row over all rows.  Return both
;;; results using values.
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
;;;; Part 2: Weight Initialization
;;;;

;;; random-weight : -> Number
;;;
;;; Return a small random number uniformly distributed in the range [-1, 1).
;;; This is one possible weight initialization strategy.  The constant
;;; weight-init-fn selects which initializer is used when building the network.
;;;
;;; Strategy: function composition — scale Racket's (random), which returns
;;; [0, 1), to the range [-1, 1) by multiplying by 2 and subtracting 1.
;;;
(define (random-weight)
  (- (* 2.0 (random)) 1.0))

;; Smoke test: random-weight returns values in [-1, 1)
(let ([w (random-weight)])
  (check-equal? (<= -1 w) #t "random-weight >= -1")
  (check-equal? (< w 1)   #t "random-weight < 1"))

;;; init-layer-weights : Number Number (-> Number) -> (List-of (List-of Number))
;;;
;;; Initialize the weight matrix for a single layer.  Each neuron in the layer
;;; gets one weight per input from the previous layer.
;;;
;;; Strategy: nested for/list — outer loop over neurons, inner loop over
;;; inputs, calling init-fn for each weight.
;;;
;;; num-inputs  : number of inputs to each neuron (outputs from the previous layer)
;;; num-neurons : number of neurons in this layer
;;; init-fn     : a zero-argument function that returns one weight value
;;;
(define (init-layer-weights num-inputs num-neurons init-fn)
  (for/list ([_neuron (in-range num-neurons)])
    (for/list ([_input (in-range num-inputs)])
      (init-fn))))

;; 4 neurons, each with 3 inputs → 4 rows of 3 weights
(let ([layer (init-layer-weights 3 4 random-weight)])
  (check-equal? (length layer) 4
                "4 neurons in layer")
  (check-equal? (length (car layer)) 3
                "3 weights per neuron"))

;; Deterministic initializer for testing
(let ([layer (init-layer-weights 2 3 (lambda () 0.5))])
  (check-equal? layer '((0.5 0.5) (0.5 0.5) (0.5 0.5))
                "Constant initializer produces uniform weights"))

;;; init-network-weights : Number (List-of Number) (-> Number)
;;;                        -> (List-of (List-of (List-of Number)))
;;;
;;; Build the full structured weight list for a network, given the number of
;;; inputs and the topology (list of layer sizes).
;;;
;;; The topology list specifies hidden layers followed by the output layer.
;;; For example, (list 20 10 20) with 5 inputs means:
;;;   Layer 1: 20 neurons, each with 5 weights  (inputs → hidden 1)
;;;   Layer 2: 10 neurons, each with 20 weights (hidden 1 → hidden 2)
;;;   Layer 3: 20 neurons, each with 10 weights (hidden 2 → output)
;;;
;;; Strategy: accumulator via named let — loop through the topology list,
;;; tracking the previous layer's size so each layer knows how many inputs
;;; its neurons receive.  Accumulate the layer weight matrices in reverse,
;;; then reverse at the end.
;;;
;;; num-inputs : number of input features
;;; topology   : list of layer sizes (e.g. (list 2 1))
;;; init-fn    : a zero-argument function that returns one weight value
;;;
(define (init-network-weights num-inputs topology init-fn)
  (let loop ([remaining-layers topology]
             [prev-size        num-inputs]
             [acc               '()])
    (if (empty? remaining-layers)
        (reverse acc)
        (let ([layer-size (car remaining-layers)])
          (loop (cdr remaining-layers)
                layer-size
                (cons (init-layer-weights prev-size layer-size init-fn)
                      acc))))))

;; 2 inputs, topology (2 1) → same shape as sample-weights
(let ([w (init-network-weights 2 '(2 1) random-weight)])
  (check-equal? (length w) 2
                "Two weight layers")
  (check-equal? (length (car w)) 2
                "Hidden layer: 2 neurons")
  (check-equal? (length (car (car w))) 2
                "Hidden neurons: 2 weights each (one per input)")
  (check-equal? (length (cadr w)) 1
                "Output layer: 1 neuron")
  (check-equal? (length (car (cadr w))) 2
                "Output neuron: 2 weights (one per hidden neuron)"))

;; 5 inputs, topology (20 10 20)
(let ([w (init-network-weights 5 '(20 10 20) (lambda () 0.1))])
  (check-equal? (length w) 3
                "Three weight layers")
  (check-equal? (length (car w)) 20
                "Layer 1: 20 neurons")
  (check-equal? (length (car (car w))) 5
                "Layer 1 neurons: 5 weights each (one per input)")
  (check-equal? (length (list-ref w 1)) 10
                "Layer 2: 10 neurons")
  (check-equal? (length (car (list-ref w 1))) 20
                "Layer 2 neurons: 20 weights each")
  (check-equal? (length (list-ref w 2)) 20
                "Layer 3: 20 neurons")
  (check-equal? (length (car (list-ref w 2))) 10
                "Layer 3 neurons: 10 weights each"))

;;;;
;;;; Part 3: Forward Pass
;;;;

;;; summation : (List-of Number) (List-of Number) -> Number
;;;
;;; Compute the dot product of two equal-length lists of numbers.
;;;
;;; Strategy: function composition — multiply element-wise with map, then
;;; sum with apply +.
;;;
;;; xs : first list of numbers
;;; ys : second list of numbers
;;;
(define (summation xs ys)
  (apply + (map * xs ys)))

(check-within (summation '(1 2 3) '(4 5 6)) 32.0 0.001
              "1*4 + 2*5 + 3*6 = 32")
(check-within (summation '(0.5 0.5) '(1.0 1.0)) 1.0 0.001
              "0.5*1 + 0.5*1 = 1")

;;; forward-neuron : (List-of Number) (List-of Number) (Number -> Number) -> Number
;;;
;;; Compute the output of a single neuron.  The neuron computes the dot product
;;; of its inputs and weights, then applies the activation function.
;;;
;;; Strategy: function composition — pass the summation result to the
;;; activation function.
;;;
;;; inputs         : list of input values from the previous layer
;;; neuron-weights : list of weights, one per input
;;; activation     : activation function (e.g. sigmoid)
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
;;; Compute the output of one layer of neurons.  Each neuron in the layer
;;; receives the same inputs and produces one output value.  Returns a list of
;;; outputs, one per neuron.
;;;
;;; Strategy: use map to apply forward-neuron to each neuron's weight list in
;;; this layer, all sharing the same input vector.
;;;
;;; inputs        : list of input values (from previous layer or the network input)
;;; layer-weights : list of neuron weight lists for this layer
;;; activation    : activation function
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
;;; Compute the full forward pass through all layers of the network.  The
;;; output of each layer becomes the input to the next layer.  Returns the
;;; output of the final layer as a list (even if there is only one output
;;; neuron).
;;;
;;; Strategy: use foldl to thread the current activation values through each
;;; layer in sequence.  The accumulator is the current layer's output, which
;;; becomes the next layer's input.
;;;
;;; inputs     : list of input values (one per network input)
;;; weights    : structured weight list (list of layers of neurons of weights)
;;; activation : activation function
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

(check-within (forward-pass '(0.5 0.5) deep-weights sigmoid)
              '(0.634) 0.001
              "Forward pass through deeper network")

;;; forward-pass-verbose : (List-of Number) (List-of (List-of (List-of Number)))
;;;                        (Number -> Number) -> (List-of (List-of Number))
;;;
;;; Like forward-pass, but returns ALL layer activations (including the input),
;;; not just the final output.  Backpropagation needs the intermediate
;;; activations to compute weight updates.
;;;
;;; The result is a list of length (number-of-layers + 1):
;;;   - Element 0: the network inputs
;;;   - Element 1: hidden layer 1 outputs
;;;   - ...
;;;   - Element n: final output layer outputs
;;;
;;; Strategy: accumulator via named let — loop through layers, computing each
;;; layer's output with forward-layer and consing it onto an accumulator.
;;; Reverse the accumulator at the end to restore input-to-output order.
;;;
;;; inputs     : list of input values (one per network input)
;;; weights    : structured weight list (list of layers of neurons of weights)
;;; activation : activation function
;;;
;;; For a 2→2→1 network with inputs (0.6, 0.8):
;;;   '((0.6 0.8)          ; inputs
;;;     (0.608 0.565)      ; hidden layer outputs
;;;     (0.534))           ; output layer
;;;
(define (forward-pass-verbose inputs weights activation)
  (let loop ([remaining-layers weights]
             [current-inputs   inputs]
             [activations      (list inputs)])
    (if (empty? remaining-layers)
        (reverse activations)
        (let ([layer-output (forward-layer current-inputs
                                           (car remaining-layers)
                                           activation)])
          (loop (cdr remaining-layers)
                layer-output
                (cons layer-output activations))))))

;; The last element of forward-pass-verbose should match forward-pass
(check-within (last (forward-pass-verbose '(0.6 0.8) sample-weights sigmoid))
              '(0.534) 0.001
              "Verbose last element matches forward-pass")
(check-within (last (forward-pass-verbose '(0.0 0.0) sample-weights sigmoid))
              '(0.525) 0.001
              "Verbose last element matches forward-pass (zero inputs)")

;; Check all intermediate activations for the 2→2→1 network
(let ([acts (forward-pass-verbose '(0.6 0.8) sample-weights sigmoid)])
  (check-equal? (length acts) 3
                "2-layer network produces 3 activation lists (input + 2 layers)")
  (check-within (list-ref acts 0) '(0.6 0.8) 0.001
                "First element is the input")
  (check-within (list-ref acts 1) '(0.608 0.565) 0.001
                "Second element is hidden layer output")
  (check-within (list-ref acts 2) '(0.534) 0.001
                "Third element is final output"))

;; Deeper network: input + 3 layers = 4 activation lists
(let ([acts (forward-pass-verbose '(0.5 0.5) deep-weights sigmoid)])
  (check-equal? (length acts) 4
                "3-layer network produces 4 activation lists")
  (check-within (list-ref acts 0) '(0.5 0.5) 0.001
                "Input preserved")
  (check-within (last acts) '(0.634) 0.001
                "Final output matches forward-pass"))

;;;;
;;;; Part 4: Putting It All Together (Forward Pass)
;;;;

;;; classify : (List-of Number) (List-of (List-of (List-of Number)))
;;;            (List-of (List Number Number)) (Number -> Number) -> (List-of Number)
;;;
;;; Scale a data point using precomputed feature ranges, then compute the
;;; forward pass through the network.  This is the main entry point for using
;;; a trained network on new data.
;;;
;;; Strategy: function composition — scale the input row with scale-row, then
;;; pass the scaled values to forward-pass.
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
;;; Strategy: use map to apply classify to each row.
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

;;;;
;;;; Part 5: Error (Loss) Functions
;;;;

;;; mean-squared-error : (List-of Number) (List-of Number) -> Number
;;;
;;; Compute the mean squared error between expected and predicted output.
;;; This is a common loss function for regression tasks.
;;;
;;;   MSE = (1/n) * sum((expected_i - predicted_i)^2)
;;;
;;; Strategy: use map to compute element-wise squared differences, then
;;; sum with apply + and divide by n.
;;;
;;; expected  : list of expected output values (labels)
;;; predicted : list of predicted output values (from the forward pass)
;;;
(define (mean-squared-error expected predicted)
  (let ([n       (length expected)]
        [sq-diffs (map (lambda (e p) (expt (- e p) 2))
                       expected
                       predicted)])
    (/ (apply + sq-diffs) n)))

(check-within (mean-squared-error '(1.0) '(0.534))
              0.217 0.001
              "MSE for single output: (1 - 0.534)^2 ≈ 0.217")
(check-within (mean-squared-error '(1.0) '(1.0))
              0.0 0.001
              "Perfect prediction → zero MSE")
(check-within (mean-squared-error '(1.0 0.0) '(0.5 0.5))
              0.25 0.001
              "Two outputs: ((0.5^2 + 0.5^2) / 2) = 0.25")
(check-within (mean-squared-error '(0.0) '(1.0))
              1.0 0.001
              "Maximum error for [0,1] range")

;;;;
;;;; Part 6: Backpropagation
;;;;
;;;; Book pseudocode reference (see back_propagation, textbook):
;;;;
;;;;   let cost equal expected_output - output
;;;;
;;;;   let weights_hidden_update equal
;;;;       hidden • (2 * cost * sigmoid_derivative(output))
;;;;   let weights_input_update equal
;;;;       input • (2 * cost * sigmoid_derivative(output) * weights_hidden)
;;;;             * sigmoid_derivative(hidden)
;;;;
;;;;   let weights_hidden equal weights_hidden + weights_hidden_update
;;;;   let weights_input  equal weights_input  + weights_input_update
;;;;
;;;; The symbol • means matrix multiplication (outer product).
;;;;

;;; compute-error : (List-of Number) (List-of Number) -> (List-of Number)
;;;
;;; Compute the element-wise error between expected and predicted output.
;;; Following the book's pseudocode: cost = expected_output - output.
;;; This returns a vector (one value per output neuron), not a scalar loss.
;;; Used internally by backprop for gradient computation.
;;;
;;; Strategy: use map with subtraction over the two parallel lists.
;;;
;;; expected  : list of expected output values (labels)
;;; predicted : list of predicted output values (from the forward pass)
;;;
(define (compute-error expected predicted)
  (map - expected predicted))

;; Output is 0.534, expected is 1 → error is positive (undershoot)
(check-within (compute-error '(1.0) '(0.534))
              '(0.466) 0.001
              "Expected 1, got 0.534 → error ≈ 0.466")
;; Output is 0.534, expected is 0 → error is negative (overshoot)
(check-within (compute-error '(0.0) '(0.534))
              '(-0.534) 0.001
              "Expected 0, got 0.534 → error ≈ −0.534")
;; Perfect prediction → zero error
(check-within (compute-error '(0.5) '(0.5))
              '(0.0) 0.001
              "Perfect prediction → zero error")
;; Multiple output neurons
(check-within (compute-error '(1.0 0.0) '(0.7 0.3))
              '(0.3 -0.3) 0.001
              "Two outputs: element-wise subtraction")

;;; compute-output-delta : (List-of Number) (List-of Number) -> (List-of Number)
;;;
;;; Compute the delta (gradient signal) for the output layer.
;;; From the book's pseudocode: 2 * cost * sigmoid_derivative(output).
;;; Applied element-wise across output neurons.
;;;
;;; Strategy: use map over parallel lists (cost and output), applying the
;;; delta formula to each pair.
;;;
;;; cost   : error list from compute-error (expected - predicted)
;;; output : output layer activations (already through sigmoid)
;;;
(define (compute-output-delta cost output)
  (map (lambda (c o) (* 2 c (sigmoid-derivative o)))
       cost
       output))

(check-within (compute-output-delta '(0.466) '(0.534))
              '(0.232) 0.001
              "Output delta for expected=1 case")
(check-within (compute-output-delta '(0.0) '(0.534))
              '(0.0) 0.001
              "Zero cost → zero delta")

;;; compute-hidden-delta : (List-of Number) (List-of (List-of Number))
;;;                        (List-of Number) -> (List-of Number)
;;;
;;; Propagate the delta from the next layer backwards to compute the delta
;;; for a hidden layer.  From the book's pseudocode:
;;;
;;;   (2 * cost * sigmoid_derivative(output) * weights_hidden)
;;;     * sigmoid_derivative(hidden)
;;;
;;; The first part (already computed as delta-next) is multiplied by the
;;; weights connecting this layer to the next, then multiplied by
;;; sigmoid_derivative of this layer's activations.
;;;
;;; For each hidden neuron i:
;;;   error-signal_i = sum over j of (delta-next_j * weights-next_j_i)
;;;   delta_i = error-signal_i * sigmoid-derivative(activation_i)
;;;
;;; Strategy: for/list over each hidden neuron index i.  For each i, use an
;;; inner for/list to sum the contributions from all neurons j in the next
;;; layer (delta-next_j times the weight from i to j), then multiply by
;;; sigmoid-derivative of this neuron's activation.
;;;
;;; delta-next   : delta from the layer above (e.g. output delta)
;;; weights-next : weights connecting this layer to the layer above
;;;                (list of neurons, each neuron is a list of weights from
;;;                this layer)
;;; activations  : this layer's activated outputs
;;;
(define (compute-hidden-delta delta-next weights-next activations)
  (let ([num-neurons (length activations)])
    (for/list ([i (in-range num-neurons)])
      (let ([error-signal
             (apply + (for/list ([j (in-range (length delta-next))])
                        (* (list-ref delta-next j)
                           (list-ref (list-ref weights-next j) i))))])
        (* error-signal (sigmoid-derivative (list-ref activations i)))))))

;; Hidden delta for the 2→2→1 network, expected=1
;; hidden = (0.608, 0.565), output delta ≈ 0.232, weights_hidden = ((0.5, -0.3))
;; neuron 0: 0.232 * 0.5 * sigmoid'(0.608)  ≈ 0.028
;; neuron 1: 0.232 * (-0.3) * sigmoid'(0.565) ≈ -0.017
(check-within (compute-hidden-delta '(0.232) '((0.5 -0.3)) '(0.608 0.565))
              '(0.028 -0.017) 0.001
              "Hidden delta propagated from output")
;; Zero delta propagates to zero
(check-within (compute-hidden-delta '(0.0) '((0.5 -0.3)) '(0.608 0.565))
              '(0.0 0.0) 0.001
              "Zero delta → zero hidden delta")

;;; compute-weight-update : (List-of Number) (List-of Number)
;;;                         -> (List-of (List-of Number))
;;;
;;; Compute the weight update matrix for one layer.  This is the outer product
;;; (the • in the book's pseudocode) of the previous layer's activations and
;;; the current layer's delta.
;;;
;;; For each neuron j in this layer and each input i from the previous layer:
;;;   update_j_i = activation_i * delta_j
;;;
;;; The result has the same shape as the weight matrix for this layer.
;;;
;;; Strategy: nested map — outer map over delta values (one per neuron),
;;; inner map over activations (one per input to each neuron), multiplying
;;; each pair.
;;;
;;; activations-prev : outputs of the previous layer (inputs to this layer)
;;; delta            : delta for this layer
;;;
(define (compute-weight-update activations-prev delta)
  (map (lambda (d)
         (map (lambda (a) (* a d))
              activations-prev))
       delta))

;; Weight update for the hidden→output layer
(check-within (compute-weight-update '(0.608 0.565) '(0.232))
              '((0.141 0.131)) 0.001
              "Hidden→output weight update")

;; Weight update for the input→hidden layer
(check-within (compute-weight-update '(0.6 0.8) '(0.028 -0.017))
              '((0.017 0.022) (-0.010 -0.014)) 0.001
              "Input→hidden weight update")

;;; add-weights : (List-of (List-of Number)) (List-of (List-of Number))
;;;              -> (List-of (List-of Number))
;;;
;;; Element-wise addition of a weight matrix and an update matrix.
;;; Both must have the same shape (same number of neurons, same number of
;;; weights per neuron).
;;;
;;; From the book's pseudocode:
;;;   weights_hidden = weights_hidden + weights_hidden_update
;;;   weights_input  = weights_input  + weights_input_update
;;;
;;; Strategy: nested map — outer map pairs corresponding neuron rows, inner
;;; map adds corresponding weights using +.
;;;
;;; weights : current weight matrix for one layer
;;; updates : update matrix from compute-weight-update
;;;
(define (add-weights weights updates)
  (map (lambda (w-row u-row)
         (map + w-row u-row))
       weights
       updates))

(check-within (add-weights '((0.5 -0.3)) '((0.141 0.131)))
              '((0.641 -0.169)) 0.001
              "Output layer weights updated")
(check-within (add-weights '((0.2 0.4) (0.3 0.1)) '((0.017 0.022) (-0.010 -0.014)))
              '((0.217 0.422) (0.290 0.086)) 0.001
              "Input layer weights updated")

;;; backprop-step : (List-of Number) (List-of Number)
;;;                 (List-of (List-of (List-of Number))) (Number -> Number)
;;;                 ((List-of Number) (List-of Number) -> Number)
;;;                 -> (values (List-of (List-of (List-of Number))) Number)
;;;
;;; Perform one complete backpropagation step: forward pass, compute error,
;;; compute deltas for each layer, compute weight updates, and return the
;;; updated weights together with the scalar loss.
;;;
;;; This generalizes the book's pseudocode to any number of layers.  The book
;;; describes two layers (input→hidden and hidden→output); we loop backwards
;;; through all layers.
;;;
;;; Strategy: first, run forward-pass-verbose to collect all layer activations.
;;; Compute the element-wise error vector (for gradient computation) and the
;;; scalar loss (via loss-fn, for monitoring).  Compute the output delta, then
;;; use a named let to loop backwards from the last layer to the first, at
;;; each step computing the weight update (outer product of previous
;;; activations and current delta), applying it, and propagating the delta
;;; backwards via compute-hidden-delta.  Return both updated weights and loss.
;;;
;;; inputs     : list of input values
;;; expected   : list of expected output values (labels)
;;; weights    : structured weight list
;;; activation : activation function
;;; loss-fn    : scalar loss function (e.g. mean-squared-error)
;;;
(define (backprop-step inputs expected weights activation loss-fn)
  ;; Forward pass: collect all activations
  (let* ([activations (forward-pass-verbose inputs weights activation)]
         [output      (last activations)]
         ;; Scalar loss for monitoring/convergence
         [loss        (loss-fn expected output)]
         ;; cost = expected_output - output (element-wise, for gradient)
         [cost        (compute-error expected output)]
         ;; Output delta: 2 * cost * sigmoid_derivative(output)
         [output-delta (compute-output-delta cost output)]
         ;; Number of weight layers
         [num-layers  (length weights)])
    ;; Work backwards through layers, accumulating updated weights and
    ;; propagating delta from output back to the first hidden layer.
    (let loop ([L           (- num-layers 1)]  ; current layer index, starts at last
               [delta       output-delta]       ; delta for current layer
               [new-weights weights])           ; accumulate updated weight layers
      (let* (;; activations[L] is the input to layer L (output of layer L-1)
             [acts-prev    (list-ref activations L)]
             ;; weight update: activations-prev • delta (outer product)
             [update       (compute-weight-update acts-prev delta)]
             ;; weights = weights + update
             [updated-layer (add-weights (list-ref weights L) update)]
             ;; Replace layer L's weights in the accumulator
             [new-weights  (append (take new-weights L)
                                   (list updated-layer)
                                   (drop new-weights (+ L 1)))])
        (if (= L 0)
            ;; Done: return updated weights and loss
            (values new-weights loss)
            ;; Propagate delta backwards for the next layer
            (let ([hidden-delta (compute-hidden-delta delta
                                                      (list-ref weights L)
                                                      (list-ref activations L))])
              (loop (- L 1) hidden-delta new-weights)))))))

;; One backprop step should move the output closer to the target

;; Training toward 1: output should increase from 0.534 toward 1.0
(let-values ([(new-w loss) (backprop-step '(0.6 0.8) '(1.0) sample-weights
                                           sigmoid mean-squared-error)])
  (let ([new-out (forward-pass '(0.6 0.8) new-w sigmoid)])
    (check-within new-out '(0.574) 0.001
                  "After one step toward 1, output increases to ≈ 0.574")
    (check-within loss 0.217 0.001
                  "MSE before update ≈ 0.217")
    ;; Verify individual weight values match the traced computation
    (check-within (car (car new-w)) '(0.217 0.422) 0.001
                  "Input→hidden neuron 1 weights updated")
    (check-within (cadr (car new-w)) '(0.290 0.086) 0.001
                  "Input→hidden neuron 2 weights updated")
    (check-within (car (cadr new-w)) '(0.641 -0.169) 0.001
                  "Hidden→output neuron weights updated")))

;; Training toward 0: output should decrease from 0.534 toward 0.0
(let-values ([(new-w loss) (backprop-step '(0.6 0.8) '(0.0) sample-weights
                                           sigmoid mean-squared-error)])
  (let ([new-out (forward-pass '(0.6 0.8) new-w sigmoid)])
    (check-within new-out '(0.487) 0.001
                  "After one step toward 0, output decreases to ≈ 0.487")
    (check-within loss 0.285 0.001
                  "MSE before update ≈ 0.285")))
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

;;; Network topology: a list of layer sizes (hidden layers followed by the output layer).
;;; The number of inputs is inferred from the dataset.
;;;
;;; Example: (list 2 1) means 2 hidden neurons, 1 output neuron.
;;;          (list 20 10 20) means two hidden layers (20, 10) and an output layer (20).
;;;
(define network-topology (list 2 1))

;;; Weight initialization function.
;;; Must accept no arguments and return a single number.
;;; This constant selects which initializer is used when building the network.
;;;
(define weight-init-fn random-weight)

;;; Activation function.
;;; Must accept a single number and return a single number.
;;; This constant selects which activation is used throughout the network.
;;;
(define activation-fn sigmoid)

;;; Error (loss) function.
;;; Must accept two lists of numbers (expected, predicted) and return a single
;;; number representing the total error.  This constant selects which loss
;;; function is used for training.
;;;
(define error-fn mean-squared-error)

;;; Hand-crafted weights for testing (matches the original sample-weights).
;;; 2 inputs → 2 hidden → 1 output, no bias.
;;;
(define sample-weights
  '(((0.2 0.4)
     (0.3 0.1))
    ((0.5 -0.3))))
;;;;
;;;; Run It
;;;;

(displayln "")
(displayln "=== Network Configuration ===")
(displayln "")
(displayln (format "  Topology:    ~a" network-topology))
(displayln (format "  Activation:  sigmoid"))
(displayln (format "  Error fn:    mean-squared-error"))
(displayln (format "  Weight init: random-weight (uniform in [-1, 1))"))
(displayln "")

;; Build a network from constants and run a forward pass
(let* ([num-inputs (length (car sample-data))]
       [weights    (init-network-weights num-inputs network-topology weight-init-fn)]
       [output     (forward-pass '(0.6 0.8) weights activation-fn)]
       [loss       (error-fn '(1.0) output)])
  (displayln (format "  Num inputs:          ~a" num-inputs))
  (displayln (format "  Forward pass (0.6, 0.8): ~a" output))
  (displayln (format "  Error (expected=1): ~a" loss)))

(displayln "")
(displayln "=== Backpropagation ===")
(displayln "")

(let* ([num-inputs (length (car sample-data))]
       [weights (init-network-weights num-inputs network-topology weight-init-fn)]
       [acts    (forward-pass-verbose '(0.6 0.8) weights sigmoid)]
       [output  (last acts)])
  (let-values ([(new-w loss) (backprop-step '(0.6 0.8) '(1.0) weights
                                             sigmoid error-fn)])
    (let* ([new-out  (forward-pass '(0.6 0.8) new-w sigmoid)]
           [new-loss (error-fn '(1.0) new-out)])
      (displayln "Forward pass with inputs (0.6, 0.8):")
      (displayln (format "  Input:   ~a" (list-ref acts 0)))
      (displayln (format "  Hidden:  ~a" (list-ref acts 1)))
      (displayln (format "  Output:  ~a" output))
      (displayln (format "  Error:   ~a" loss))
      (displayln "")
      (displayln "After one backprop step (expected=1):")
      (displayln (format "  New output: ~a" new-out))
      (displayln (format "  New Error:  ~a" new-loss))
      (displayln (format "  Weights:    ~a" new-w)))))
