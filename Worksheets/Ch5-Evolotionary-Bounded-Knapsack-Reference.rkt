#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 5: Bounded Knapsack Problem
;;;;; Item Data
;;;;;

;;;;
;;;; Data Definitions
;;;;

;;; An Item is a (list Number String Number Number Number)
;;; - id       : Natural  -- unique item identifier
;;; - name     : String   -- human-readable item name
;;; - weight   : Natural  -- item weight in kg
;;; - value    : Natural  -- item value in dollars
;;; - quantity : Natural  -- maximum number of copies available

;;; item-id       : Item -> Natural
;;; item-name     : Item -> String
;;; item-weight   : Item -> Natural
;;; item-value    : Item -> Natural
;;; item-quantity : Item -> Natural
(define (item-id       item) (first   item))
(define (item-name     item) (second  item))
(define (item-weight   item) (third   item))
(define (item-value    item) (fourth  item))
(define (item-quantity item) (fifth   item))

;;;;
;;;; Constants
;;;;

;;; Knapsack capacity in kg (Table 5.1)
(define knapsack-capacity 6404180)

;;; items : List of Items
;;;
;;; All 26 items from Table 5.1, each represented as (list id name weight value quantity).
;;;
(define items
  (list
    ;;    id name              weight   value   qty
    (list  1 "Axe"              32252   68674   19)
    (list  2 "Bronze coin"     225790  471010   14)
    (list  3 "Crown"           468164  944620    2)
    (list  4 "Diamond statue"  489494  962094    9)
    (list  5 "Emerald belt"     35384   78344   11)
    (list  6 "Fossil"          265590  579152    6)
    (list  7 "Gold coin"       497911  902698    4)
    (list  8 "Helmet"          800493 1686515   10)
    (list  9 "Ink"             823576 1688691    7)
    (list 10 "Jewel box"       552202 1056157    3)
    (list 11 "Knife"           323618  677562    5)
    (list 12 "Long sword"      382846  833132   13)
    (list 13 "Mask"             44676   99192   15)
    (list 14 "Necklace"        169738  376418    8)
    (list 15 "Opal badge"      610876 1253986    4)
    (list 16 "Pearls"          854190 1853562    9)
    (list 17 "Quiver"          671123 1320297   12)
    (list 18 "Ruby ring"       698180 1301637   17)
    (list 19 "Silver bracelet" 446517  859835   16)
    (list 20 "Timepiece"       909620 1677534    7)
    (list 21 "Uniform"         904818 1910501    6)
    (list 22 "Venom potion"    730061 1528646    9)
    (list 23 "Wool scarf"      931932 1827477    3)
    (list 24 "Crossbow"        952360 2068204    1)
    (list 25 "Yesteryear book" 926023 1746556    7)
    (list 26 "Zinc cup"        978724 2100851    2)))

;;;;
;;;; Tests
;;;;

(check-equal? (length items) 26
              "There should be 26 items")

(check-equal? (item-id (first items)) 1
              "First item should have id 1")

(check-equal? (item-name (first items)) "Axe"
              "First item should be Axe")

(check-equal? (item-weight (first items)) 32252
              "Axe should weigh 32252")

(check-equal? (item-value (first items)) 68674
              "Axe should be worth 68674")

(check-equal? (item-quantity (first items)) 19
              "Axe should have quantity 19")

(check-equal? (item-name (last items)) "Zinc cup"
              "Last item should be Zinc cup")

(check-equal? (item-quantity (last items)) 2
              "Zinc cup should have quantity 2")

;;; generate-individual : List of Items Real -> Individual
;;;
;;; Generate a single random individual for the bounded knapsack problem.
;;; For each item, randomly decide whether to include it with the given
;;; probability. If included, choose a random quantity between 1 and the
;;; item's maximum quantity. If excluded, the gene is 0.
;;;
;;; An Individual is a List of Natural
;;; Interpretation: each position corresponds to one item; the value is the
;;; number of copies selected (0 means "leave it", 1..qty means "take that many").
;;;
;;; knapsack-items   : List of Items -- the items available to select from
;;; selection-chance : Real          -- probability of including each item (0.0->1.0)
;;;
;;; Examples:
;;; (generate-individual items 0.1)
;;; (generate-individual items 0.5)
;;; (generate-individual items 0.9)
;;;
(define (generate-individual knapsack-items selection-chance)
  (map (lambda (item)
         (if (< (random) selection-chance)
             (+ 1 (random (item-quantity item)))   ; selected: [1, qty]
             0))                                   ; not selected
       knapsack-items))

;;
;; Tests
;;
(check-equal? (length (generate-individual items 0.5)) 26
              "Individual should have one gene per item")

(check-equal? (andmap (lambda (gene item) (<= 0 gene (item-quantity item)))
                      (generate-individual items 0.5)
                      items) #t
              "Every gene should be between 0 and the item's max quantity")

(check-equal? (andmap zero? (generate-individual items 0.0)) #t
              "Selection chance 0.0 should produce all zeros")

(check-equal? (andmap positive? (generate-individual items 1.0)) #t
              "Selection chance 1.0 should produce all positive values")

;;; generate-initial-population : List-of-Items Real Natural -> Population
;;;
;;; Generate an initial population of random individuals for the bounded knapsack
;;; problem. Each individual is a list of genes (0..qty), one gene per item.
;;; Each population is a list of individuals.
;;;
;;; knapsack-items   : List of Items -- items available; determines chromosome length
;;; selection-chance : Real          -- probability of including each item [0.0, 1.0]
;;; population-size  : Natural       -- number of individuals to generate
;;;
(define (generate-initial-population knapsack-items selection-chance population-size) 
  (for/list ([individual (in-range population-size)])
    (generate-individual knapsack-items selection-chance)))

(check-equal? (length (generate-initial-population items 0.5 10)) 10
              "Population has correct number of individuals")

(check-equal? (length (first (generate-initial-population items 0.5 10))) 26
              "Each individual has one gene per item")

(check-equal? (andmap (lambda (individual)
                        (andmap (lambda (gene item) (<= 0 gene (item-quantity item)))
                                individual items))
                      (generate-initial-population items 0.5 10)) #t
              "All genes in all individuals are within valid range")

;;; calculate-individual-fitness : Individual List Number -> Number
;;;
;;; Calculate the fitness of an individual by summing the values of all selected items, multiplied
;;; by the quantity chosen for each. If the total weight of selected items exceeds the knapsack
;;; capacity, fitness is 0 (incorrect solution).
;;;
;;; Algorithm:
;;;   For each gene in the individual:
;;;     add gene * item-weight and gene * item-value to running totals
;;;   If total weight exceeds max weight, return 0
;;;   Otherwise return total value
;;;
;;; individual          : Individual    -- list of quantities, one per item (0..qty)
;;; knapsack-items      : List of Items -- the items available to select from
;;; knapsack-max-weight : Natural       -- maximum weight the knapsack can hold
;;;
(define (calculate-individual-fitness individual knapsack-items knapsack-max-weight)
  ;; Combine the individual's genes with the corresponding items, then accumulate total-weight and
  ;; total-value across all selected genes.
  (let* ([genes-and-items (map list individual knapsack-items)]
         [total-weight    (for/sum ([gene-and-item (in-list genes-and-items)])
                            (let ([gene (first  gene-and-item)]
                                  [item (second gene-and-item)])
                              (* gene (item-weight item))))]
         [total-value     (for/sum ([gene-and-item (in-list genes-and-items)])
                            (let ([gene (first  gene-and-item)]
                                  [item (second gene-and-item)])
                              (* gene (item-value item))))])
    (if (> total-weight knapsack-max-weight)
        0
        total-value)))

;;; Selecting no items gives fitness 0
(check-equal? (calculate-individual-fitness
                '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                items knapsack-capacity)
              0
              "Empty selection should have fitness 0")

;;; Selecting 1 Axe (weight 32252, value 68674)
(check-equal? (calculate-individual-fitness
                '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                items knapsack-capacity)
              68674
              "Selecting 1 Axe should give its value as fitness")

;;; Selecting 3 Axes (weight 32252*3 = 96756, value 68674*3 = 206022)
(check-equal? (calculate-individual-fitness
                '(3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                items knapsack-capacity)
              206022
              "Selecting 3 Axes should give 3 times the Axe value")

;;; Selecting 1 Axe + 2 Emerald belts (value: 68674 + 2*78344 = 225362)
(check-equal? (calculate-individual-fitness
                '(1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                items knapsack-capacity)
              225362
              "Selecting 1 Axe + 2 Emerald belts should give combined value")

;;; Selecting max quantity of everything should exceed capacity
(check-equal? (calculate-individual-fitness
                '(19 14 2 9 11 6 4 10 7 3 5 13 15 8 4 9 12 17 16 7 6 9 3 1 7 2)
                items knapsack-capacity)
              0
              "All items at max quantity exceeds capacity, fitness should be 0")

;;; build-slices : List of (list Individual Number) -> List of Slices
;;;
;;; A Slice is a (list Individual Number Number)
;;; - individual : Individual -- the chromosome this slice represents
;;; - low        : Number     -- lower bound of this slice on [0,1]
;;; - high       : Number     -- upper bound of this slice on [0,1]
;;;
;;; Given a list of (individual, weight) pairs, build slices tiling [0, 1] with
;;; widths proportional to each weight. When all weights are zero, each individual
;;; gets an equal slice so that selection can still proceed uniformly.
;;;
;;; weighted : List of (list Individual Number) -- individuals paired with weights
;;;
(define (build-slices weighted)
  (let ([total-weight (apply + (map second weighted))])
    (if (zero? total-weight)
        ;; All weights are zero: give each individual an equal slice
        (let ([n (length weighted)])
          (map (lambda (pair i)
                 (list (first pair) (/ i n) (/ (+ i 1) n)))
               weighted
               (build-list n values)))
        ;; Normal case: slices proportional to weight
        (let loop ([remaining weighted]
                   [total     0]
                   [result    '()])
          (if (empty? remaining)
              (reverse result)
              (let* ([probability (/ (second (car remaining)) total-weight)]
                     [slice       (list (first (car remaining)) total (+ total probability))])
                (loop (cdr remaining)
                      (+ total probability)
                      (cons slice result))))))))

;;; --- build-slices tests ---

;;; Two individuals with known weights
(define test-weighted (list (list '(1 0) 3) (list '(0 1) 7)))
(define test-slices   (build-slices test-weighted))

(check-equal? (length test-slices) 2
              "build-slices returns one slice per individual")

(check-equal? (second (first test-slices)) 0
              "First slice starts at 0")

(check-equal? (third (first test-slices)) 3/10
              "First slice ends at weight/total = 3/10")

(check-equal? (second (second test-slices)) 3/10
              "Second slice starts where first ends")

(check-equal? (third (second test-slices)) 1
              "Last slice ends at 1")

;;; All-zero weights produce equal slices
(define test-zero-slices (build-slices (list (list '(1 0) 0) (list '(0 1) 0))))

(check-equal? (second (first test-zero-slices)) 0
              "First zero-weight slice starts at 0")

(check-equal? (third (first test-zero-slices)) 1/2
              "First zero-weight slice ends at 1/2")

(check-equal? (second (second test-zero-slices)) 1/2
              "Second zero-weight slice starts at 1/2")

(check-equal? (third (second test-zero-slices)) 1
              "Second zero-weight slice ends at 1")

;;; Three individuals: verify slices tile [0, 1] without gaps
(define test-three-slices (build-slices (list (list '(a) 1) (list '(b) 2) (list '(c) 3))))

(check-equal? (second (first test-three-slices)) 0
              "Three-slice: first starts at 0")

(check-equal? (third (first test-three-slices))
              (second (second test-three-slices))
              "Three-slice: first ends where second starts (no gap)")

(check-equal? (third (second test-three-slices))
              (second (third test-three-slices))
              "Three-slice: second ends where third starts (no gap)")

(check-equal? (third (third test-three-slices)) 1
              "Three-slice: last ends at 1")

;;; roulette-slices : Population List Number -> List of Slices
;;;
;;; Build a roulette wheel where each individual's slice width is proportional to its fitness.
;;; Fitter individuals get wider slices and are therefore more likely to be selected.
;;;
;;; population          : Population    -- list of individuals
;;; knapsack-items      : List of Items -- items used to evaluate fitness
;;; knapsack-max-weight : Natural       -- weight limit used to evaluate fitness
;;;
(define (roulette-slices population knapsack-items knapsack-max-weight)
  (build-slices (map (lambda (ind)
                       (list ind (calculate-individual-fitness
                                   ind knapsack-items knapsack-max-weight)))
                     population)))

;;; rank-slices : Population List Number -> List of Slices
;;;
;;; Build a roulette wheel where individuals are sorted by fitness ascending, then assigned rank
;;; numbers (1 = worst, N = best). Slice widths are proportional to rank rather than raw fitness,
;;; compressing the spread so that dominant individuals don't crowd out the rest.
;;;
;;; population          : Population    -- list of individuals
;;; knapsack-items      : List of Items -- items used to evaluate fitness
;;; knapsack-max-weight : Natural       -- weight limit used to evaluate fitness
;;;
(define (rank-slices population knapsack-items knapsack-max-weight)
  (let* ([scored (map (lambda (ind)
                        (list ind (calculate-individual-fitness
                                    ind knapsack-items knapsack-max-weight)))
                      population)]
         [sorted (sort scored (lambda (a b) (< (second a) (second b))))])
    (build-slices (map (lambda (pair rank)
                         (list (first pair) rank))
                       sorted
                       (build-list (length sorted) add1)))))

;;; select : List of Slices -> Slice
;;;
;;; Spin the wheel once: generate a random float in [0.0, 1.0) and return the
;;; slice whose range contains it.
;;;
;;; slices : List of Slices -- from roulette-slices or rank-slices
;;;
(define (select slices)
  (let ([spin (random)])
    (first (filter (lambda (slice)
                     (and (<= (second slice) spin)
                          (< spin (third slice))))
                   slices))))

;;; test-pop : Population
;;;
;;; Two individuals for deterministic fitness testing.
;;;   individual 1 - 1 Axe           (weight   32252, value   68674)
;;;   individual 2 - 2 Emerald belts (weight   70768, value  156688)
;;;   total fitness: 68674 + 156688 = 225362
;;;
(define test-pop
  (list '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)   ; 1 Axe
        '(0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))  ; 2 Emerald belts

(define test-roulette (roulette-slices test-pop items knapsack-capacity))
(define test-rank     (rank-slices     test-pop items knapsack-capacity))

;;; --- roulette-slices tests ---
;;; Fitness: Axe = 68674, Emerald belts = 156688, total = 225362
;;; Axe slice = [0, 68674/225362], Emerald belt slice = [68674/225362, 1]

(check-equal? (length test-roulette) 2
              "roulette-slices returns one slice per individual")

(check-equal? (third (first test-roulette))
              34337/112681
              "Axe slice ends at fitness/total = 68674/225362")

(check-equal? (third (second test-roulette))
              1
              "Last roulette slice ends at 1")

;;; --- rank-slices tests ---
;;; Axe (68674) < Emerald belts (156688), so Axe gets rank 1, Emerald belts rank 2.
;;; rank-total = 1 + 2 = 3.  Axe slice = [0, 1/3], Emerald belt slice = [1/3, 1].

(check-equal? (length test-rank) 2
              "rank-slices should return one slice per individual")

(check-equal? (third (first test-rank))
              1/3
              "Weakest individual (Axe) should get rank 1, slice ends at 1/3")

(check-equal? (second (second test-rank))
              1/3
              "Strongest individual (Emerald belts) should start at 1/3")

(check-equal? (third (second test-rank))
              1
              "Last rank slice should end at 1")

;;; --- select tests (works with both slice strategies) ---

(check-equal? (length (select test-roulette)) 3
              "select should return a slice with three elements from roulette")

(check-equal? (length (select test-rank)) 3
              "select should return a slice with three elements from rank")

;;; uniform-crossover : Individual Individual -> List of Individuals
;;;
;;; Produce two children using a randomly generated mask. For each position, if the mask bit is 1,
;;; child 1 takes from parent-a and child 2 takes from parent-b. If the mask bit is 0, child 1 takes
;;; from parent-b and child 2 takes from parent-a. The mask is generated randomly each call.
;;;
;;; Because each gene in the child is copied whole from one parent or the other (never combined),
;;; children are guaranteed to have valid quantities as long as both parents do.
;;;
;;; parent-a : Individual -- first parent chromosome (list of quantities, 0..qty)
;;; parent-b : Individual -- second parent chromosome (list of quantities, 0..qty)
;;;
(define (uniform-crossover parent-a parent-b)
  (let* ([mask    (build-list (length parent-a) (lambda (_) (random 2)))]
         [pairs   (map list mask parent-a parent-b)]
         [child-1 (map (lambda (triple)
                         (if (equal? (first triple) 1)
                             (second triple)    ; mask=1: take from parent-a
                             (third triple)))   ; mask=0: take from parent-b
                       pairs)]
         [child-2 (map (lambda (triple)
                         (if (equal? (first triple) 1)
                             (third triple)     ; mask=1: take from parent-b
                             (second triple)))  ; mask=0: take from parent-a
                       pairs)])
    (list child-1 child-2)))

;;; Should produce exactly two children
(check-equal? (length (uniform-crossover
                        '(1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                        '(0 3 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
              2
              "Should produce exactly two children")

;;; Each child should be the same length as the parents
(check-equal? (length (first (uniform-crossover
                               '(1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                               '(0 3 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
              26
              "Each child should be the same length as the parents")

;;; At each position, the two children's genes should be the two parents' genes (one each)
(check-equal? (let* ([parent-a '(1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)]
                     [parent-b '(0 3 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)]
                     [result   (uniform-crossover parent-a parent-b)]
                     [child-1  (first result)]
                     [child-2  (second result)])
                (andmap (lambda (g1 g2 pa pb)
                          (or (and (equal? g1 pa) (equal? g2 pb))
                              (and (equal? g1 pb) (equal? g2 pa))))
                        child-1 child-2 parent-a parent-b))
              #t
              "At each position, children should swap: one gets parent-a, the other parent-b")

;;; reproduce-children : List of Slices -> Population
;;;
;;; Produce children by pairing adjacent selected individuals and applying uniform crossover.
;;; Pairs are (0+1, 2+3, ...). Returns a flat list of all children produced.
;;;
;;; the-chosen : List of Slices -- selected individuals from select
;;;
(define (reproduce-children the-chosen)
  (let loop ([remaining the-chosen]
             [result    '()])
    (cond
      [(or (empty? remaining) (empty? (cdr remaining)))
       (reverse result)]
      [else
        (let* ([parent-a  (first (car remaining))]
               [parent-b  (first (cadr remaining))]
               [children  (uniform-crossover parent-a parent-b)])
          (loop (cddr remaining)
                (append (reverse children) result)))])))

(check-equal? (length (reproduce-children (roulette-slices test-pop items knapsack-capacity)))
              2
              "Two slices produce two children")

(check-equal? (length (first (reproduce-children (roulette-slices test-pop items knapsack-capacity))))
              26
              "Each child has one gene per item")

;;; mutate-individual : Individual List of Items Natural Natural -> Individual
;;;
;;; Produce a new individual with genes-to-mutate randomly chosen genes mutated. For each mutated
;;; gene, a random integer in [-max-mutation, +max-mutation] is added to the current value. The
;;; result is clamped to [0, item-quantity] so that the gene never goes negative or exceeds the
;;; item's maximum.
;;;
;;; individual      : Individual    -- the chromosome to mutate (list of quantities, 0..qty)
;;; knapsack-items  : List of Items -- items list; provides max quantity per gene
;;; genes-to-mutate : Natural       -- number of genes to mutate
;;; max-mutation    : Natural       -- mutation range; each mutation adds a value
;;;                                    in [-max-mutation, +max-mutation]
;;;
(define (mutate-individual individual knapsack-items genes-to-mutate max-mutation)
  (let ([indices (for/list ([_ (in-range genes-to-mutate)])
                   (random (length individual)))])
    (map (lambda (gene item index)
           (if (member index indices)
               (let ([mutated (+ gene (- (random (+ max-mutation max-mutation 1)) max-mutation))])
                 (min (max mutated 0) (item-quantity item)))
               gene))
         individual
         knapsack-items
         (build-list (length individual) values))))

(check-equal? (length (mutate-individual
                         '(1 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                         items 3 2))
              26
              "Mutated individual should have same length as original")

(check-equal? (andmap (lambda (gene item) (<= 0 gene (item-quantity item)))
                      (mutate-individual
                        '(19 14 2 9 11 6 4 10 7 3 5 13 15 8 4 9 12 17 16 7 6 9 3 1 7 2)
                        items 5 10)
                      items)
              #t
              "All genes stay within [0, qty] even with large mutations at max values")

(check-equal? (andmap (lambda (gene item) (<= 0 gene (item-quantity item)))
                      (mutate-individual
                        '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                        items 5 10)
                      items)
              #t
              "All genes stay within [0, qty] even with large mutations at zero values")

;;; generational-merge : Population Population -> Population
;;;
;;; Generational model: the children fully replace the old population.
;;; If no children were produced (reproduction failed), fall back to the
;;; old population.
;;;
;;; population : Population -- the current generation (discarded)
;;; children   : Population -- the new generation (returned as-is)
;;;
(define (generational-merge population children)
  (if (empty? children)
      population
      children))

(check-equal? (generational-merge '((1 2 3) (4 5 6)) '((7 8 9)))
              '((7 8 9))
              "Children replace the population")

(check-equal? (generational-merge '((1 2 3) (4 5 6)) '())
              '((1 2 3) (4 5 6))
              "Empty children falls back to old population")

;;; mutate-children : Population List of Items Natural Natural -> Population
;;;
;;; Apply mutate-individual to every individual in the population.
;;;
;;; children        : Population    -- list of individuals to mutate
;;; knapsack-items  : List of Items -- items list; provides max quantity per gene
;;; genes-to-mutate : Natural       -- number of genes to mutate per individual
;;; max-mutation    : Natural       -- mutation range [-max-mutation, +max-mutation]
;;;
(define (mutate-children children knapsack-items genes-to-mutate max-mutation)
  (map (lambda (individual)
         (mutate-individual individual knapsack-items genes-to-mutate max-mutation))
       children))

;;; calculate-population-fitness : Population List Number -> Number
;;;
;;; Return the maximum fitness of any individual in the population.
;;; This is the "current best" fitness for the generation.
;;;
;;; population          : Population    -- list of individuals to evaluate
;;; knapsack-items      : List of Items -- items used to evaluate fitness
;;; knapsack-max-weight : Natural       -- weight limit used to evaluate fitness
;;;
(define (calculate-population-fitness population knapsack-items knapsack-max-weight)
  (apply max (map (lambda (individual)
                    (calculate-individual-fitness individual knapsack-items knapsack-max-weight))
                  population)))

(check-equal? (calculate-population-fitness test-pop items knapsack-capacity)
              156688
              "Best fitness in test-pop is 2 Emerald belts = 156688")

(check-equal? (calculate-population-fitness
                (list '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                items knapsack-capacity)
              0
              "All-zero population has fitness 0")

;;; run-ga : List of Items Natural Natural Natural Real
;;;          (Population List Number -> List of Slices)
;;;          Natural Natural -> Number
;;;
;;; Run the genetic algorithm for the bounded knapsack problem. Evolves a population over
;;; number-of-generations generations using uniform crossover, generational merge, and the
;;; given selection strategy (roulette-slices or rank-slices). Returns the best fitness found.
;;;
;;; Algorithm:
;;;   1. Generate initial population
;;;   2. For each generation:
;;;      a. Track best fitness seen so far
;;;      b. Build slices using selection-strategy
;;;      c. Spin population-size times to select parents
;;;      d. Pair adjacent parents and apply uniform crossover
;;;      e. Mutate all children
;;;      f. Replace population with children (generational merge)
;;;
;;; knapsack-items       : List of Items -- items available; determines chromosome length
;;; knapsack-max-weight  : Natural       -- maximum weight the knapsack can hold
;;; population-size      : Natural       -- number of individuals in the population
;;; number-of-generations: Natural       -- how many generations to evolve
;;; selection-chance     : Real          -- probability of including each item in initial population
;;; selection-strategy   : Procedure     -- roulette-slices or rank-slices
;;; genes-to-mutate      : Natural       -- number of genes to mutate per individual per generation
;;; max-mutation         : Natural       -- mutation range [-max-mutation, +max-mutation]
;;;
(define (run-ga knapsack-items knapsack-max-weight
                population-size number-of-generations
                selection-chance selection-strategy
                genes-to-mutate max-mutation)
  (let loop ([generation        0]
             [global-population (generate-initial-population knapsack-items
                                                             selection-chance
                                                             population-size)]
             [best-global-fitness 0])
    (if (equal? generation number-of-generations)
        best-global-fitness
        (let* ([current-best-fitness (calculate-population-fitness
                                       global-population knapsack-items knapsack-max-weight)]
               [new-best-fitness     (max current-best-fitness best-global-fitness)]
               [slices               (selection-strategy
                                       global-population knapsack-items knapsack-max-weight)]
               [the-chosen           (for/list ([_ (in-range (+ population-size
                                                                (remainder population-size 2)))])
                                       (select slices))]
               [the-children         (reproduce-children the-chosen)]
               [the-children         (mutate-children the-children knapsack-items
                                                      genes-to-mutate max-mutation)]
               [global-population    (generational-merge global-population the-children)])
          (loop (+ generation 1)
                global-population
                new-best-fitness)))))

;;; Run it
(run-ga items knapsack-capacity 100 200 0.5 roulette-slices 3 2)
(run-ga items knapsack-capacity 100 200 0.3 rank-slices 1 1)
