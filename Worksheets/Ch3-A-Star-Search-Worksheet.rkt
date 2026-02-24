#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 3: A* Search Algorithm
;;;;; Worksheet
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;;
;;;; Implement the A* search algorithm to find the shortest path in a grid-based map with obstacles.
;;;; Unlike bfs which explores uniformly, A* uses a heuristic to guide the search toward the goal more
;;;; efficiently.
;;;;

;;;;
;;;; Constants
;;;;

;;; Map representation as a two-dimensional grid
;;; - 0 represents an open cell (passable)
;;; - 1 represents a blocked cell (wall/obstacle)
;;; - Grid uses (row, col) indexing starting from (0, 0) at top-left
;;;
;;; Visual representation:
;;;   ★ . . . .    (★ is goal at (0, 0))
;;;   . █ █ █ .
;;;   . █ # █ .    (# is start at (2, 2))
;;;   . █ . . .
;;;   . . . . .
;;;
(define map-grid
  '((0 0 0 0 0)   ; Row 0 (goal at (0, 0))
    (0 1 1 1 0)   ; Row 1
    (0 1 0 1 0)   ; Row 2 (start at (2, 2))
    (0 1 0 0 0)   ; Row 3
    (0 0 0 0 0))) ; Row 4

;;; Start and goal positions as (row, col) pairs
(define start-pos '(2 2))
(define goal-pos '(0 0))

;;; Movement costs
(define horizontal-cost 1)  ; Cost for left/right movement
(define vertical-cost 5)    ; Cost for up/down movement

;;;;
;;;; Wishes (helper functions)
;;;;

;;; movement-cost : List List -> Number
;;;
;;; Calculate the cost of moving from one position to an adjacent position.
;;; Horizontal moves (left/right) cost 1, vertical moves (up/down) cost 5.
;;;
;;; (define (movement-cost pos1 pos2)
;;;   (if (= row1 row2) ...))
;;;
;;; pos1 : (row col) starting position
;;; pos2 : (row col) ending position (must be adjacent)
;;;
;;; (check-equal? (movement-cost '(0 0) '(0 1)) 1
;;;               "Moving right costs 1")
;;; (check-equal? (movement-cost '(0 0) '(1 0)) 5
;;;               "Moving down costs 5")
;;; (check-equal? (movement-cost '(2 2) '(2 1)) 1
;;;               "Moving left costs 1")
;;; (check-equal? (movement-cost '(2 2) '(1 2)) 5
;;;               "Moving up costs 5")
;;;
#;
(define (movement-cost pos1 pos2)
  ...
  (if (= row1 row2)
      horizontal-cost  ; Same row means horizontal movement
      vertical-cost))  ; Different row means vertical movement

;;; in-bounds? : List Number Number -> Boolean
;;;
;;; Check if a position is within the grid boundaries.
;;;
;;; (define (in-bounds? grid row col)
;;;   (and ...))
;;;
;;; grid : two-dimensional list representing the map
;;; row  : the row index
;;; col  : the column index
;;;
;;; (check-equal? (in-bounds? map-grid 0 0) #t
;;;               "Top-left corner is in bounds")
;;; (check-equal? (in-bounds? map-grid 4 4) #t
;;;               "Bottom-right corner is in bounds")
;;; (check-equal? (in-bounds? map-grid 5 5) #f
;;;               "Position outside grid is out of bounds")
;;; (check-equal? (in-bounds? map-grid -1 0) #f
;;;               "Negative row is out of bounds")
;;;
#;
(define (in-bounds? grid row col)
  ...)

;;; passable? : List List -> Boolean
;;;
;;; Check if a position is passable (not a wall).
;;;
;;; (define (passable? grid pos)
;;;   (and ...))
;;;
;;; grid : two-dimensional list representing the map
;;; pos  : (row col) position to check
;;;
;;; (check-equal? (passable? map-grid '(0 0)) #t
;;;               "Goal position is passable")
;;; (check-equal? (passable? map-grid '(1 1)) #f
;;;               "Wall is not passable")
;;; (check-equal? (passable? map-grid '(2 2)) #t
;;;               "Start position is passable")
;;;
#;
(define (passable? grid pos)
  ...)

;;; get-neighbours : List List -> List
;;;
;;; Get all valid neighbouring positions (up, down, left, right) that are
;;; passable and within bounds.
;;;
;;; (define (get-neighbours grid pos)
;;;   (for/list ([neighbour (in-list (list ...))]  ; list of 4 directions
;;;              #:when (passable? grid neighbour))
;;;     neighbour))
;;;
;;; grid : two-dimensional list representing the map
;;; pos  : (row col) current position
;;;
;;; (check-equal? (get-neighbours map-grid '(0 0)) '((1 0) (0 1))
;;;               "Top-left corner has 2 valid neighbours")
;;;
#;
(define (get-neighbours grid pos)
  ...)

;;; manhattan-distance : List List -> Number
;;;
;;; Calculate Manhattan distance heuristic between two positions.
;;; This is an heuristic for A* (but we could have others).
;;;
;;; (define (manhattan-distance pos1 pos2)
;;;   (+ (abs ...) (abs ...)))
;;;
;;; pos1 : (row col) first position
;;; pos2 : (row col) second position
;;;
;;; (check-equal? (manhattan-distance '(0 0) '(2 2)) 4
;;;               "Distance from (0,0) to (2,2) is 4")
;;;
#;
(define (manhattan-distance pos1 pos2)
  ...)

;;;;
;;;; A* Algorithm
;;;;

;;; reconstruct-path : List List -> List
;;;
;;; Reconstruct the path from start to goal by following parent pointers
;;; stored in the parent-map.
;;;
;;; (define (reconstruct-path goal-pos parent-map)
;;;   (let loop ([current-pos ...]
;;;              [path ...])
;;;     ...))
;;;
;;; goal-pos   : (row col) the goal position
;;; parent-map : association list mapping position -> parent position
;;;
;;; (check-equal? (reconstruct-path '(0 0) (list (cons '(0 0) '())))
;;;               '((0 0))
;;;               "Single node path (goal is start)")
;;; (check-equal? (reconstruct-path '(0 1) (list (cons '(0 1) '(0 0))
;;;                                              (cons '(0 0) '())))
;;;               '((0 0) (0 1))
;;;               "Two node path")
;;; (check-equal? (reconstruct-path '(0 2) (list (cons '(0 2) '(0 1))
;;;                                              (cons '(0 1) '(0 0))
;;;                                              (cons '(0 0) '())))
;;;               '((0 0) (0 1) (0 2))
;;;               "Three node path")
;;;
(define (reconstruct-path goal-pos parent-map)
  (let loop ([current-pos goal-pos]
             [path (list goal-pos)])
    (let ([parent-entry (assoc current-pos parent-map)])
      (if parent-entry
          (let ([parent (cdr parent-entry)])
            (if (empty? parent)
                path
                (loop parent (cons parent path))))
          path))))

(check-equal? (reconstruct-path '(0 0) (list (cons '(0 0) '())))
              '((0 0))
              "Single node path (goal is start)")
(check-equal? (reconstruct-path '(0 1) (list (cons '(0 1) '(0 0))
                                             (cons '(0 0) '())))
              '((0 0) (0 1))
              "Two node path")
(check-equal? (reconstruct-path '(0 2) (list (cons '(0 2) '(0 1))
                                             (cons '(0 1) '(0 0))
                                             (cons '(0 0) '())))
              '((0 0) (0 1) (0 2))
              "Three node path")

;;; a-star : List List List -> List
;;;
;;; Find the shortest path from start to goal using A* algorithm. Uses a priority queue (sorted list)
;;; to explore nodes with lowest f-cost first, where f = g (cost from start) + h (heuristic to goal).
;;;
;;; Algorithm (as outlined in book):
;;; 1. Add root node to stack
;;; 2. Is stack empty? If yes, go to step 3; else go to step 4
;;; 3. Return "No path to goal"
;;; 4. Pop node from stack as current node
;;; 5. Is current node visited? If yes, go to step 2; else go to step 6
;;; 6. Mark current node as visited
;;; 7. Is goal reached? If yes, go to step 8; else go to step 9
;;; 8. Return path using current node
;;; 9. Current has next neighbour? If yes, go to step 11; else go to step 10
;;; 10. Sort stack by cost ascending
;;; 11. Set current node as parent of neighbour
;;; 12. Calculate cost for neighbour (f = g + h)
;;; 13. Add neighbour to stack, go to step 9
;;;
;;; Important node for step 12: the code in the book is missing h; it just calculates g as the cost.
;;; In this implementation, we introduce an h function (Manhattan distance from the neighbour location
;;; to the goal), as follows: [new-f (+ new-g (manhattan-distance neighbour goal))]
;;;
;;; (define (a-star grid start goal)
;;;   (let loop ([stack      (list (list start 0 (manhattan-distance start goal) '()))]
;;;              [visited    '()]
;;;              [parent-map '()])
;;;     ...))
;;;
;;; grid  : two-dimensional list representing the map
;;; start : (row col) starting position
;;; goal  : (row col) goal position
;;;
;;; (check-equal? (a-star map-grid '(0 0) '(0 0)) '((0 0))
;;;               "A* handles start equals goal")
;;; (check-equal? (a-star map-grid '(0 0) '(0 1)) '((0 0) (0 1))
;;;               "A* finds path between adjacent positions")
;;; (check-equal? (car (a-star map-grid '(2 2) '(0 0))) '(2 2)
;;;               "Path starts at start position")
;;; (check-equal? (last (a-star map-grid '(2 2) '(0 0))) '(0 0)
;;;               "Path ends at goal position")
;;; (check-equal? (a-star map-grid '(2 2) '(1 2)) #f
;;;               "A* returns #f when no path exists to blocked position")
;;;
#;
(define (a-star grid start goal)
  ...)

