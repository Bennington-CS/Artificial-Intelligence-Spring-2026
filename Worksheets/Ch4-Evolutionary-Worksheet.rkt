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
;#
(define (generate-individual individual-size)
  ...)

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
#;
(define (generate-initial-population population-size individual-size)
  ...)

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
#;
(define (calculate-individual-fitness individual knapsack-items knapsack-max-weight)
  ...)

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
#;
(define (set-probabilities-of-population population knapsack-items knapsack-max-weight)
  ...)

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
#;
(define (roulette-wheel-selection population knapsack-items knapsack-max-weight)
  ...)

(check-equal? (list? (roulette-wheel-selection test-pop items knapsack-capacity))
              #t
              "roulette-wheel-selection returns a list")

(check-equal? (length (roulette-wheel-selection test-pop items knapsack-capacity))
              1
              "Exactly one slice is selected per spin")
