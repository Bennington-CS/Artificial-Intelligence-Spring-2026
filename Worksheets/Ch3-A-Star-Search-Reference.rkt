#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 3: A* Pathfinding Algorithm
;;;;; Reference Implementation
;;;;;

;;;;
;;;; Special Note: The pseudocode in the book does not use a heuristic; it simply calculates the cost
;;;; so far.  In this implementation, we choose to create a Manhattan Distance function to use as the
;;;; heuristic estimation function (h).
;;;;

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
;;;               "Moving right should cost 1")
;;; (check-equal? (movement-cost '(0 0) '(1 0)) 5
;;;               "Moving down should cost 5")
;;; (check-equal? (movement-cost '(2 2) '(2 1)) 1
;;;               "Moving left should cost 1")
;;; (check-equal? (movement-cost '(2 2) '(1 2)) 5
;;;               "Moving up should cost 5")
;;;
(define (movement-cost pos1 pos2)
  (let ([row1 (car pos1)]
        [col1 (cadr pos1)]
        [row2 (car pos2)]
        [col2 (cadr pos2)])
    (if (= row1 row2)
        horizontal-cost  ; Same row means horizontal movement
        vertical-cost))) ; Different row means vertical movement

(check-equal? (movement-cost '(0 0) '(0 1)) 1
              "Moving right should cost 1")
(check-equal? (movement-cost '(0 0) '(1 0)) 5
              "Moving down should cost 5")
(check-equal? (movement-cost '(2 2) '(2 1)) 1
              "Moving left should cost 1")
(check-equal? (movement-cost '(2 2) '(1 2)) 5
              "Moving up should cost 5")

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
;;;               "Top-left corner should be in bounds")
;;; (check-equal? (in-bounds? map-grid 4 4) #t
;;;               "Bottom-right corner should be in bounds")
;;; (check-equal? (in-bounds? map-grid 5 5) #f
;;;               "Position outside grid should be out of bounds")
;;; (check-equal? (in-bounds? map-grid -1 0) #f
;;;               "Negative row should be out of bounds")
;;;
(define (in-bounds? grid row col)
  (let ([num-rows (length grid)]
        [num-cols (length (car grid))])
    (and (>= row 0) (< row num-rows)
         (>= col 0) (< col num-cols))))

(check-equal? (in-bounds? map-grid 0 0) #t
              "Top-left corner should be in bounds")
(check-equal? (in-bounds? map-grid 4 4) #t
              "Bottom-right corner should be in bounds")
(check-equal? (in-bounds? map-grid 5 5) #f
              "Position outside grid should be out of bounds")
(check-equal? (in-bounds? map-grid -1 0) #f
              "Negative row should be out of bounds")

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
;;;               "Goal position should be passable")
;;; (check-equal? (passable? map-grid '(1 1)) #f
;;;               "Wall should not not passable")
;;; (check-equal? (passable? map-grid '(2 2)) #t
;;;               "Start position should be passable")
;;;
(define (passable? grid pos)
  (let ([row (car pos)]
        [col (cadr pos)])
    (and (in-bounds? grid row col)
         (= 0 (list-ref (list-ref grid row) col)))))

(check-equal? (passable? map-grid '(0 0)) #t
              "Goal position should be passable")
(check-equal? (passable? map-grid '(1 1)) #f
              "Wall should not be passable")
(check-equal? (passable? map-grid '(2 2)) #t
              "Start position should be passable")

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
;;;               "Top-left corner should have 2 valid neighbours")
;;;
(define (get-neighbours grid pos)
  (let ([row (car pos)]
        [col (cadr pos)])
    (for/list ([neighbour (in-list (list (list (- row 1) col)    ; up
                                         (list (+ row 1) col)    ; down
                                         (list row (- col 1))    ; left
                                         (list row (+ col 1))))] ; right
               #:when (passable? grid neighbour))
      neighbour)))

(check-equal? (get-neighbours map-grid '(0 0)) '((1 0) (0 1))
              "Top-left corner should have 2 valid neighbours")

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
;;;               "Distance from (0,0) to (2,2) should be 4")
;;; (check-equal? (manhattan-distance '(2 2) '(0 0)) 4
;;;               "Distance for (2,2) to (0,0) should be 4")
;;;
(define (manhattan-distance pos1 pos2)
  (let ([row1 (car pos1)]
        [col1 (cadr pos1)]
        [row2 (car pos2)]
        [col2 (cadr pos2)])
    (+ (abs (- row2 row1))
       (abs (- col2 col1)))))

(check-equal? (manhattan-distance '(0 0) '(2 2)) 4
              "Distance from (0,0) to (2,2) should be 4")
(check-equal? (manhattan-distance '(2 2) '(0 0)) 4
              "Distance for (2,2) to (0,0) should be 4")

;;;;
;;;; A* Algorithm
;;;;

;;; reconstruct-path : List List -> List
;;;
;;; Reconstruct the path from start to goal by following parent pointers stored in the parent-map.
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
;;;               "Path should be single node (goal is start)")
;;; (check-equal? (reconstruct-path '(0 1) (list (cons '(0 1) '(0 0))
;;;                                              (cons '(0 0) '())))
;;;               '((0 0) (0 1))
;;;               "Path should be two nodes")
;;; (check-equal? (reconstruct-path '(0 2) (list (cons '(0 2) '(0 1))
;;;                                              (cons '(0 1) '(0 0))
;;;                                              (cons '(0 0) '())))
;;;               '((0 0) (0 1) (0 2))
;;;               "Path should be three nodes")
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
              "Path should be single node (goal is start)")
(check-equal? (reconstruct-path '(0 1) (list (cons '(0 1) '(0 0))
                                             (cons '(0 0) '())))
              '((0 0) (0 1))
              "Path should be two nodes")
(check-equal? (reconstruct-path '(0 2) (list (cons '(0 2) '(0 1))
                                             (cons '(0 1) '(0 0))
                                             (cons '(0 0) '())))
              '((0 0) (0 1) (0 2))
              "Path should be three nodes")

;;; a-star : List List List -> List
;;;
;;; Find the shortest path from start to goal using A* algorithm. Uses a priority queue (sorted list)
;;; to explore nodes with lowest f-cost first, where f = g (cost from start) + h (heuristic to goal).
;;;
;;; Note: The "stack" is actually a list that's repeatedly sorted.  The book calls it a stack, I
;;; think, because you push neighbours into it, and pop nodes off of it.  But since you sort it, it
;;; is functionally a priority queue, which is not really a stack.  But whatever.
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
(define (a-star grid start goal)
  ;; loop:       named let for the A* search loop
  ;; stack:      priority queue, each item is (list position g-cost f-cost parent-pos)
  ;; visited:    list of all positions we've already explored
  ;; parent-map: association list mapping each position to its parent position
  ;; 
  ;; f-cost note: g + h, so to start it's (0 + manhattan-distance start goal)
  ;; initial stack item for loop:    posn  g-cost f-cost                          root
  (let loop ([stack      (list (list start 0      (manhattan-distance start goal) '()))]
             [visited    '()]
             [parent-map '()])
    ;; Step 2: Is stack empty?
    (if (empty? stack)
        ;; Step 3: Return "No path to goal"
        #f
        ;; Step 4: Pop node from stack as current node
        (let* ([current-item (car stack)]           ; The item (a list) we're exploring
               [current-pos  (car current-item)]    ; The position we're exploring
               [g-cost       (cadr current-item)]   ; Cost from start to this position
               [f-cost       (caddr current-item)]  ; Total estimated cost (g + h)
               [parent       (cadddr current-item)] ; Parent position for path reconstruction
               [rest-stack   (cdr stack)])          ; Remaining items in stack
          ;; Step 5: Is current node visited?
          (if (member current-pos visited)
              ;; Yes, already visited, skip and recurse with rest of stack
              (loop rest-stack visited parent-map)
              ;; No, Step 6: Mark current node as visited; [extend parent map]
              (let ([new-visited    (cons current-pos visited)]
                    [new-parent-map (cons (cons current-pos parent) parent-map)])
                ;; Step 7: Is goal reached?
                (if (equal? current-pos goal)
                    ;; Yes, Step 8: Return path using current node
                    (reconstruct-path current-pos new-parent-map)
                    ;; No, Step 9-13: Process neighbours
                    (let* ([neighbours (get-neighbours grid current-pos)]
                           ;; Filter to only unvisited neighbours (avoid cycles)
                           [unvisited-neighbours 
                             (filter-not (lambda (n) (member n new-visited)) neighbours)]
                           ;; Step 11: Set current node as parent of neighbour nodes
                           ;; Step 12: Calculate cost for neighbour nodes
                           [neighbour-nodes
                             (map (lambda (neighbour)
                                    ;; move-cost: cost to move from current position to this neighbour
                                    ;;            by invoking movement-cost, which is currently set to
                                    ;;            1 for horizontal, 5 for vertical
                                    ;; new-g:     total cost from start to this neighbour, by taking
                                    ;;            g-cost (which we already know), and adding move-cost
                                    ;; new-f:     the A* formula: f = g + h, where g is the actual
                                    ;;            cost from start to the neighbour; h is the heuristic
                                    ;;            estimate (the Manhattan distance) to the goal
                                    (let* ([move-cost (movement-cost current-pos neighbour)]
                                           [new-g     (+ g-cost move-cost)]
                                           [new-f     (+ new-g (manhattan-distance neighbour goal))])
                                      (list neighbour new-g new-f current-pos)))
                                  unvisited-neighbours)]
                           ;; Step 13: Add neighbour(s) to stack
                           ;; Step 10: Sort stack by cost ascending
                           ;; Add the new nodes we just created for unvisited neighbours to the rest
                           ;; of the stack, and sort according to f-cost.  This ensures the position
                           ;; with the lowest f-cost will be explored next.
                           [new-stack (sort (append neighbour-nodes rest-stack)
                                            (lambda (a b) (< (caddr a) (caddr b))))])
                      ;; Recurse with updated stack, visited list, and parent map
                      (loop new-stack new-visited new-parent-map)))))))))

(check-equal? (a-star map-grid '(0 0) '(0 0)) '((0 0))
              "A* should handle start equals goal")
(check-equal? (a-star map-grid '(0 0) '(0 1)) '((0 0) (0 1))
              "A* should find path between adjacent positions")
(check-equal? (car (a-star map-grid '(2 2) '(0 0))) '(2 2)
              "Path should start at start position")
(check-equal? (last (a-star map-grid '(2 2) '(0 0))) '(0 0)
              "Path should end at goal position")
(check-equal? (a-star map-grid '(2 2) '(1 2)) #f
              "A* should return #f when no path exists to blocked position")
