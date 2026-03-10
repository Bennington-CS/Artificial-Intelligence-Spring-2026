#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 4: Knapsack Problem
;;;;; Item Data
;;;;;

;;;;
;;;; Data Definitions
;;;;

;;; An Item is a (list Number String Number Number)
;;; - id     : Natural  -- unique item identifier
;;; - name   : String   -- human-readable item name
;;; - weight : Natural  -- item weight in kg
;;; - value  : Natural  -- item value in dollars

;;; item-id     : Item -> Natural
;;; item-name   : Item -> String
;;; item-weight : Item -> Natural
;;; item-value  : Item -> Natural
(define (item-id     item) (first  item))
(define (item-name   item) (second item))
(define (item-weight item) (third  item))
(define (item-value  item) (fourth item))

;;;;
;;;; Constants
;;;;

;;; Knapsack capacity in kg (Table 4.2)
(define knapsack-capacity 6404180)

;;; items : List of Item
;;;
;;; All 26 items from Table 4.2, each represented as (list id name weight value).
;;;
(define items
  (list
    ;;        id name              weight   value ($)
    (list  1 "Axe"              32252   68674)
    (list  2 "Bronze coin"     225790  471010)
    (list  3 "Crown"           468164  944620)
    (list  4 "Diamond statue"  489494  962094)
    (list  5 "Emerald belt"     35384   78344)
    (list  6 "Fossil"          265590  579152)
    (list  7 "Gold coin"       497911  902698)
    (list  8 "Helmet"          800493 1686515)
    (list  9 "Ink"             823576 1688691)
    (list 10 "Jewel box"       552202 1056157)
    (list 11 "Knife"           323618  677562)
    (list 12 "Long sword"      382846  833132)
    (list 13 "Mask"             44676   99192)
    (list 14 "Necklace"        169738  376418)
    (list 15 "Opal badge"      610876 1253986)
    (list 16 "Pearls"          854190 1853562)
    (list 17 "Quiver"          671123 1320297)
    (list 18 "Ruby ring"       698180 1301637)
    (list 19 "Silver bracelet" 446517  859835)
    (list 20 "Timepiece"       909620 1677534)
    (list 21 "Uniform"         904818 1910501)
    (list 22 "Venom potion"    730061 1528646)
    (list 23 "Wool scarf"      931932 1827477)
    (list 24 "Crossbow"        952360 2068204)
    (list 25 "Yesteryear book" 926023 1746556)
    (list 26 "Zinc cup"        978724 2100851)))

;;;;
;;;; Data Definitions
;;;;

;;; A Gene is one of:
;;; - 0
;;; - 1
;;; Interpretation: whether an item is excluded (0) or included (1) in the knapsack.

;;; An Individual is a List of Genes
;;; Interpretation: a chromosome representing one candidate solution.
;;; Each position corresponds to one item; 1 means "take it", 0 means "leave it".
;;; Example: '(1 0 1 0 0) means items 1 and 3 are selected, items 2, 4, 5 are not.

;;; A Population is a List of Individuals
;;; Interpretation: a collection of candidate solutions to be evolved.

;;;;
;;;; Wishes (helper functions)
;;;;

;;; random-gene : -> Gene
;;;
;;; Generate a random gene: 0 or 1 with equal probability.
;;;
;;; (define (random-gene)
;;;   (random 2))
;;;
(define (random-gene)
  (random 2))

;;; generate-individual : Number -> Individual
;;;
;;; Generate a single random individual (chromosome) of the given size.
;;; Each gene is independently chosen to be 0 or 1 at random.
;;;
;;; (define (generate-individual individual-size)
;;;   (for/list ...))
;;;
(define (generate-individual individual-size)
  (for/list ([gene (in-range individual-size)])
    (random-gene)))

(check-equal? (length (generate-individual 5)) 5
              "Individual has correct number of genes")
(check-equal? (length (generate-individual 26)) 26
              "Individual for 26-item knapsack has 26 genes")
(check-equal? (andmap (lambda (g) (not (false? (member g '(0 1)))))
                      (generate-individual 10))
              #t
              "All genes in individual are 0 or 1")

;;; generate-initial-population : Number Number -> Population
;;;
;;; Generate an initial population of random individuals for a genetic algorithm.
;;; Each individual is a list of genes (0 or 1), one gene per item.
;;; Each population is a list of individuals.
;;;
;;; This corresponds to the pseudocode:
;;;   for individual in range 0 to population_size:
;;;     for gene in range 0 to individual_size:
;;;       append random 0 or 1 to current_individual
;;;     append current_individual to population
;;;
;;; (define (generate-initial-population population-size individual-size)
;;;   (for/list ...))
;;;
(define (generate-initial-population population-size individual-size)
  (for/list ([individual (in-range population-size)])
    (generate-individual individual-size)))

(check-equal? (length (generate-initial-population 10 5)) 10
              "Population has correct number of individuals")
(check-equal? (length (first (generate-initial-population 10 5))) 5
              "Each individual has correct number of genes")
(check-equal? (length (generate-initial-population 100 26)) 100
              "Large population has correct number of individuals")
(check-equal? (length (first (generate-initial-population 100 26))) 26
              "Each individual in large population has 26 genes")
(check-equal? (andmap (lambda (individual)
                        (andmap (lambda (g) (not (false? (member g '(0 1))))) individual))
                      (generate-initial-population 10 5))
              #t
              "All genes in all individuals are 0 or 1")

;;; calculate-individual-fitness : Individual List Number -> Number
;;;
;;; Calculate the fitness of an individual by summing the values of all selected items (gene = 1). If
;;; the total weight of selected items exceeds the knapsack capacity, fitness is 0 (incorrect
;;; solution).
;;;
;;; Algorithm:
;;;   For each gene in the individual:
;;;     if the gene is 1, add the corresponding item's weight and value
;;;       to running totals
;;;   If total weight exceeds max weight, return 0
;;;   Otherwise return total value
;;;
;;; individual          : Individual    -- list of 0s and 1s, one per item
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
                              (if (= gene 1)
                                  (item-weight item)
                                  0)))]
         [total-value     (for/sum ([gene-and-item (in-list genes-and-items)])
                            (let ([gene (first  gene-and-item)]
                                  [item (second gene-and-item)])
                              (if (= gene 1)
                                  (item-value item)
                                  0)))])
    (if (> total-weight knapsack-max-weight)
        0
        total-value)))

;;;;
;;;; Tests
;;;;

;;; Selecting no items gives fitness 0
(check-equal? (calculate-individual-fitness
                '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                items knapsack-capacity)
              0
              "Empty selection should have fitness 0")

;;; Selecting only the Axe (item 1, value 68674, weight 32252)
(check-equal? (calculate-individual-fitness
                '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                items knapsack-capacity)
              68674
              "Selecting only the Axe should give its value as fitness")

;;; Selecting only the Zinc cup (item 26, value 2100851, weight 978724)
(check-equal? (calculate-individual-fitness
                '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                items knapsack-capacity)
              2100851
              "Selecting only the Zinc cup gives its value as fitness")

;;; Selecting all items exceeds capacity, so fitness is 0
(check-equal? (calculate-individual-fitness
                '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                items knapsack-capacity)
              0
              "All items selected exceeds capacity, fitness should be 0")

;;; Selecting Axe + Emerald belt + Mask (light items, all well within capacity)
;;; Expected value: 68674 + 78344 + 99192 = 246210
(check-equal? (calculate-individual-fitness
                '(1 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
                items knapsack-capacity)
              246210
              "Selecting Axe + Emerald belt + Mask should give combined value as fitness")

;;; set-probabilities-of-population : Population List Number -> List of ScoredIndividual
;;;
;;; A ScoredIndividual is a (list Individual Number)
;;; - individual  : Individual -- the chromosome
;;; - probability : Number     -- likelihood of selection; fitness / total-fitness
;;;
;;; Assign each individual a probability of selection equal to its fitness divided
;;; by the total fitness of the whole population (fitness-proportionate selection).
;;;
;;; population          : Population    -- list of individuals to score
;;; knapsack-items      : List of Items -- items used to evaluate fitness
;;; knapsack-max-weight : Natural       -- weight limit used to evaluate fitness
;;;
;;; (check-equal? (length (set-probabilities-of-population test-pop items knapsack-capacity)) 2
;;;               "Returns one scored entry per individual")
;;; (check-equal? (second (first (set-probabilities-of-population test-pop items knapsack-capacity)))
;;;               68674/147018
;;;               "Axe-only individual gets correct probability")
;;;
(define (set-probabilities-of-population population knapsack-items knapsack-max-weight)
  ;; total-fitness: sum of all individual fitness values; used as denominator
  (let ([total-fitness (for/sum ([individual (in-list population)])
                         (calculate-individual-fitness
                           individual knapsack-items knapsack-max-weight))])
    (map (lambda (individual)
           (list individual
                 ;; Guard against division by zero when no individuals are selected
                 (if (zero? total-fitness)
                     0
                     (/ (calculate-individual-fitness individual knapsack-items knapsack-max-weight)
                        total-fitness))))
         population)))

;;; test-pop : Population
;;;
;;; Two individuals for deterministic fitness testing.
;;;   individual 1 - Axe only          (weight   32252, value   68674)
;;;   individual 2 - Emerald belt only  (weight   35384, value   78344)
;;;   total fitness: 68674 + 78344 = 147018
;;;
(define test-pop
  (list '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)   ; Axe only
        '(0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))  ; Emerald belt only

(check-equal? (length (set-probabilities-of-population test-pop items knapsack-capacity))
              2
              "Returns one scored entry per individual")

(check-equal? (second (first (set-probabilities-of-population test-pop items knapsack-capacity)))
              68674/147018
              "Axe-only individual gets correct probability")

(check-equal? (second (second (set-probabilities-of-population test-pop items knapsack-capacity)))
              78344/147018
              "Emerald-belt-only individual gets correct probability")

;;; roulette-wheel-selection : Population List Number -> List of Slices
;;;
;;; A Slice is a (list Individual Number Number)
;;; - individual : Individual -- the chromosome this slice represents
;;; - low        : Number     -- lower bound of this slice on [0,1] (exclusive)
;;; - high       : Number     -- upper bound of this slice on [0,1] (inclusive)
;;;
(define (roulette-wheel-selection population knapsack-items knapsack-max-weight)
  (let* (;; Assign each individual a selection probability proportional to its fitness. Each entry is
         ;; (list individual probability).
         [possible-probabilities (set-probabilities-of-population
                                   population knapsack-items knapsack-max-weight)]
         ;; Build slices that tile the interval [0, 1]. Each slice is (list individual low high),
         ;; where low and high are the boundaries of that individual's segment on the wheel. A fitter
         ;; individual gets a wider slice and is therefore more likely to be selected.
         [slices
           (let loop ([remaining possible-probabilities] ; scored individuals not yet sliced
                      [total     0]                      ; running cumulative probability
                      [result    '()])                   ; slices built so far (reversed)
             (if (empty? remaining)
                 (reverse result)
                 (let* ([individual  (first  (car remaining))]  ; the chromosome
                        [probability (second (car remaining))]  ; its share of the wheel
                        ;; This slice runs from the current total up to total + probability,
                        ;; advancing the boundary for the next individual.
                        [slice       (list individual total (+ total probability))])
                   (loop (cdr remaining)
                         (+ total probability)
                         (cons slice result)))))]
         ;; Spin the wheel: a random float in [0.0, 1.0).
         ;; Whichever slice's range contains this value is selected.
         [spin (random)])
    ;; Return the slice where low < spin <= high.
    ;; Exactly one slice will match since the slices tile [0, 1] without gaps.
    (filter (lambda (slice)
              (and (<= (second slice) spin)
                   (< spin (third slice))))
            slices)))

(check-equal? (list? (roulette-wheel-selection test-pop items knapsack-capacity))
              #t
              "roulette-wheel-selection returns a list")

(check-equal? (length (roulette-wheel-selection test-pop items knapsack-capacity))
              1
              "Exactly one slice is selected per spin")

;;; one-point-crossover : Individual Individual Natural -> List of Individual
;;;
;;; Produce two children by splitting both parents at xover-point and swapping their tails. Child 1
;;; gets the head of parent-a and the tail of parent-b; child 2 gets the head of parent-b and the tail
;;; of parent-a.
;;;
;;; parent-a    : Individual -- first parent chromosome
;;; parent-b    : Individual -- second parent chromosome
;;; xover-point : Natural    -- index at which to split both parents
;;;
(define (one-point-crossover parent-a parent-b xover-point)
  (let* ([child-1 (append (take parent-a xover-point)    ; genes 0 to xover-point from parent-a
                          (drop parent-b xover-point))]  ; genes xover-point to end from parent-b
         [child-2 (append (take parent-b xover-point)    ; genes 0 to xover-point from parent-b
                          (drop parent-a xover-point))]) ;genes xover-point to end from parent-a
    (list child-1 child-2)))

(check-equal? (one-point-crossover '(1 1 1 1) '(0 0 0 0) 2)
              '((1 1 0 0) (0 0 1 1))
              "Children should swap tails at crossover point")

(check-equal? (one-point-crossover '(1 1 1 1) '(0 0 0 0) 0)
              '((0 0 0 0) (1 1 1 1))
              "Crossover at 0 should swap parents entirely")

(check-equal? (one-point-crossover '(1 1 1 1) '(0 0 0 0) 4)
              '((1 1 1 1) (0 0 0 0))
              "Crossover at end should leave parents unchanged")

(check-equal? (one-point-crossover '(1 1 0 0 1) '(0 0 1 1 0) 3)
              '((1 1 0 1 0) (0 0 1 0 1))
              "Crossover at position 3 should be asymmetric")

;;; mutate-individual : Individual Natural -> Individual
;;;
;;; Produce a new individual with one randomly chosen gene flipped (0->1 or 1->0).
;;;
;;; individual        : Individual -- the chromosome to mutate
;;; chromosome-length : Natural    -- number of genes in the individual
;;;
(define (mutate-individual individual chromosome-length)
  (let* ([random-index (random chromosome-length)]       ; pick a random gene position to flip
         [current-gene (list-ref individual random-index)]; get the gene at that position
         [flipped-gene (if (= current-gene 1) 0 1)])     ; flip: 1->0 or 0->1
    ;; Rebuild the individual with the flipped gene at random-index,
    ;; keeping all other genes unchanged.
    (append (take individual random-index)
            (list flipped-gene)
            (drop individual (+ random-index 1)))))

(check-equal? (length (mutate-individual '(1 0 1 0 1) 5))
              5
              "Mutated individual should have same length as original")

;;; calculate-population-fitness : Population List Number -> Number
;;;
;;; Return the maximum fitness of any individual in the population.
;;; This is the "current best" fitness for the generation.
;;;
;;; population          : Population   -- list of individuals to evaluate
;;; knapsack-items      : List of Item -- items used to evaluate fitness
;;; knapsack-max-weight : Natural      -- weight limit used to evaluate fitness
;;;
(define (calculate-population-fitness population knapsack-items knapsack-max-weight)
  ;; Note: (apply max '(3 7 2 9 1)) is equivalent to (max 3 7 2 9 1).
  (apply max (map (lambda (individual)
                    (calculate-individual-fitness individual knapsack-items knapsack-max-weight))
                  population)))

;;; reproduce-children : List of Slice Natural -> Population
;;;
;;; Produce children by pairing adjacent selected individuals and applying
;;; one-point crossover at xover-point. Pairs are (0+1, 2+3, ...).
;;; Returns a flat list of all children produced.
;;;
;;; the-chosen  : List of Slice -- selected individuals from roulette-wheel-selection
;;; xover-point : Natural       -- index at which to split chromosomes for crossover
;;;
(define (reproduce-children the-chosen xover-point)
  (let loop ([remaining the-chosen]
             [result    '()])
    (cond
      [(or (empty? remaining) (empty? (cdr remaining)))
       (reverse result)]
      [else
        (let* ([parent-a  (first (car remaining))]   ; individual from first slice
               [parent-b  (first (cadr remaining))]  ; individual from second slice
               [children  (one-point-crossover parent-a parent-b xover-point)])
          (loop (cddr remaining)
                (append (reverse children) result)))])))

;;; mutate-children : Population Natural -> Population
;;;
;;; Apply mutate-individual to every individual in the population.
;;; Returns a new population with each individual mutated once.
;;;
;;; children          : Population -- list of individuals to mutate
;;; chromosome-length : Natural    -- number of genes per individual
;;;
(define (mutate-children children chromosome-length)
  (map (lambda (individual)
         (mutate-individual individual chromosome-length))
       children))

;;; merge-population-and-children : Population Population -> Population
;;;
;;; Generational model: the children fully replace the old population.
;;; The old population is discarded entirely.
;;;
;;; population : Population -- the current generation (discarded)
;;; children   : Population -- the new generation (returned as-is)
;;;
(define (merge-population-and-children population children)
  (if (empty? children)
      population  ; fall back to old population if reproduction failed
      children))

;;; run-ga : Natural Natural Natural Natural -> Number
;;;
;;; Run the genetic algorithm for the knapsack problem.
;;; Evolves a population over number-of-generations generations,
;;; tracking the best fitness seen across all generations.
;;; Returns the best fitness value found.
;;;
;;; This corresponds to the pseudocode:
;;;   let best_global_fitness = 0
;;;   let global_population = generate_initial_population(population_size)
;;;   for generation in range(number_of_generations):
;;;     current_best_fitness = calculate_population_fitness(...)
;;;     if current_best_fitness > best_global_fitness:
;;;       best_global_fitness = current_best_fitness
;;;     the_chosen   = roulette_wheel_selection(global_population)
;;;     the_children = reproduce_children(the_chosen)
;;;     the_children = mutate_children(the_children)
;;;     global_population = merge_population_and_children(global_population, the_children)
;;;
;;; population-size      : Natural -- number of individuals in the population
;;; number-of-generations: Natural -- how many generations to evolve
;;; xover-point          : Natural -- crossover point for one-point-crossover
;;; chromosome-length    : Natural -- number of genes per individual (= number of items)
;;;
(define (run-ga population-size number-of-generations xover-point chromosome-length)
  (let loop ([generation          0]
             [global-population   (generate-initial-population population-size chromosome-length)]
             [best-global-fitness 0])
    (if (equal? generation number-of-generations)
        best-global-fitness
        (let* ([current-best-fitness (calculate-population-fitness
                                       global-population items knapsack-capacity)]
               [new-best-fitness     (if (> current-best-fitness best-global-fitness)
                                         current-best-fitness
                                         best-global-fitness)]
               [the-chosen           (roulette-wheel-selection
                                       global-population items knapsack-capacity)]
               [the-children         (reproduce-children the-chosen xover-point)]
               [the-children         (mutate-children the-children chromosome-length)]
               [global-population    (merge-population-and-children global-population the-children)])
          (loop (+ generation 1)
                global-population
                new-best-fitness)))))

;;; Run it
(define result (run-ga 100 200 13 26))
(displayln "Best fitness found:")
(displayln result)
