#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 9: Q-Learning — Parking Lot Simulation
;;;;; Worksheet (Part 1: Environment)
;;;;;
;;;;; This file implements the simulation environment for Q-learning:
;;;;;   - Grid representation of a parking lot with cars, pedestrians, and a goal
;;;;;   - Movement in four cardinal directions (north, south, east, west)
;;;;;   - Reward function based on cell type
;;;;;   - State encoding as a single integer
;;;;;
;;;;; The agent (a car) must navigate from its start position to the goal
;;;;; (a person with arms raised) while avoiding parked cars and pedestrians.
;;;;; The agent CAN pass through obstacles but receives negative rewards.
;;;;;

;;;;
;;;; Constants
;;;;

;;; Cell type symbols
(define empty      '-)
(define car-cell   'C)
(define pedestrian 'P)
(define goal       'G)
(define agent      'A)

;;; Grid dimensions
(define grid-size-x 10)  ; number of rows
(define grid-size-y 10)  ; number of columns

;;; Reward values
(define grid-empty-reward           100)
(define grid-goal-reward            500)
(define grid-obstacle-car-reward   -100)
(define grid-obstacle-person-reward -500)
(define grid-out-of-bounds-reward  -150)

;;; Action symbols
(define command-north 'north)
(define command-south 'south)
(define command-east  'east)
(define command-west  'west)

;;; All possible actions (used by Q-learning later)
(define actions (list command-north command-south command-east command-west))

;;; The parking lot grid — 10×10
;;; Read from the diagram: A = agent start, G = goal, C = car, P = pedestrian, - = empty
;;;
;;;      y0 y1 y2 y3 y4 y5 y6 y7 y8 y9
;;; x0:  -  C  C  C  -  C  -  -  C  -
;;; x1:  -  P  -  -  -  -  -  P  -  P
;;; x2:  -  C  C  -  -  C  C  -  -  C
;;; x3:  -  -  -  -  -  -  C  C  -  C
;;; x4:  C  P  P  C  -  -  P  A  -  -     ← agent will start at (4, 7)
;;; x5:  -  -  -  C  -  -  -  C  C  C
;;; x6:  -  C  C  C  P  -  -  C  C  P
;;; x7:  -  -  -  P  C  -  -  P  -  -
;;; x8:  C  P  -  -  -  -  -  -  -  C
;;; x9:  P  C  -  C  C  G  -  C  -  P     ← goal at (9, 5)
;;;
;;; Note: The agent start position is stored as EMPTY in the grid because the
;;; agent's position is tracked separately.  The grid represents the static
;;; environment (what's "on the grid").
;;;
(define grid
  ;  0 1 2 3 4 5 6 7 8 9
  '((- C C C - C - - C -)   ; Row 0
    (- P - - - - - P - P)   ; Row 1
    (- C C - - C C - - C)   ; Row 2
    (- - - - - - C C - C)   ; Row 3
    (C P P C - - P - - -)   ; Row 4 (agent start is empty)
    (- - - C - - - C C C)   ; Row 5
    (- C C C P - - C C P)   ; Row 6
    (- - - P C - - P - -)   ; Row 7
    (C P - - - - - - - C)   ; Row 8
    (P C - C C G - C - P))) ; Row 9

;;; Agent start and goal positions as (x, y) pairs
;;; NOTE: Following the book's convention: x = row, y = column
;;; (see move_agent where command_north reduces x by 1)
(define agent-start-x 4)
(define agent-start-y 7)
(define goal-x 9)
(define goal-y 5)

;;;;
;;;; Grid Access
;;;;

;;; grid-ref : (List-of (List-of Symbol)) Number Number -> Symbol
;;;
;;; Look up the cell value at position (x, y) in the grid.
;;; Strategy: index into the nested list structure.
;;;
;;; grid : the grid grid (list of rows)
;;; x    : row index (0-based)
;;; y    : column index (0-based)
;;;
(define (grid-ref grid x y)
  (void))

(check-equal? (grid-ref grid 0 0) '-
              "Top-left corner is empty")
(check-equal? (grid-ref grid 0 1) 'C
              "Row 0, col 1 is a car")
(check-equal? (grid-ref grid 4 7) '-
              "Agent start position is empty in the grid")
(check-equal? (grid-ref grid 9 5) 'G
              "Goal position")
(check-equal? (grid-ref grid 1 1) 'P
              "Pedestrian at (1,1)")

;;;;
;;;; Simulation Helper Functions
;;;;

;;; is-within-bounds : Number Number -> Boolean
;;;
;;; Check whether the position (next-x, next-y) is within the grid boundaries.
;;; Strategy: chained comparison on both coordinates.
;;;
;;; next-x : candidate row index
;;; next-y : candidate column index
;;;
(define (is-within-bounds next-x next-y)
  (void))

(check-equal? (is-within-bounds 0 0) #t
              "Top-left corner is in bounds")
(check-equal? (is-within-bounds 9 9) #t
              "Bottom-right corner is in bounds")
(check-equal? (is-within-bounds 10 0) #f
              "Row 10 is out of bounds")
(check-equal? (is-within-bounds 0 10) #f
              "Column 10 is out of bounds")
(check-equal? (is-within-bounds -1 0) #f
              "Negative row is out of bounds")
(check-equal? (is-within-bounds 0 -1) #f
              "Negative column is out of bounds")

;;; cost-movement : Number Number -> Number
;;;
;;; Determine the reward for moving to position (next-x, next-y) based on
;;; the cell type at that position.
;;; Strategy: look up the cell and use cond to return the appropriate reward.
;;;
;;; next-x : row of the target cell
;;; next-y : column of the target cell
;;;
(define (cost-movement next-x next-y)
  (void))

(check-equal? (cost-movement 0 0) 100
              "Empty cell → +100")
(check-equal? (cost-movement 0 1) -100
              "Car → -100")
(check-equal? (cost-movement 1 1) -500
              "Pedestrian → -500")
(check-equal? (cost-movement 9 5) 500
              "Goal → +500")
(check-equal? (cost-movement 4 7) 100
              "Agent start position is empty → +100")

;;; is-goal-achieved : Number Number -> Boolean
;;;
;;; Determine whether the agent has reached the goal.
;;; Strategy: compare agent position to the goal position.
;;;
;;; agent-x : current row of the agent
;;; agent-y : current column of the agent
;;;
;;; (check-equal? (is-goal-achieved 9 5) #t)
;;; (check-equal? (is-goal-achieved 0 0) #f)
;;;
(define (is-goal-achieved agent-x agent-y)
  (void))

(check-equal? (is-goal-achieved 9 5) #t
              "At the goal")
(check-equal? (is-goal-achieved 0 0) #f
              "Not at the goal")
(check-equal? (is-goal-achieved 9 4) #f
              "Adjacent to goal but not there")

;;; get-state : Number Number -> Number
;;;
;;; Convert a (row, col) position to a unique state number.
;;; Strategy: linearize the 2D position into a single integer.
;;;
;;; The state space has grid-size-x × grid-size-y = 100 possible states,
;;; numbered 0 through 99.
;;;
;;; agent-x : row of the agent
;;; agent-y : column of the agent
;;;
(define (get-state agent-x agent-y)
  (void))

(check-equal? (get-state 0 0) 0
              "Top-left corner is state 0")
(check-equal? (get-state 0 9) 9
              "Top-right corner is state 9")
(check-equal? (get-state 4 7) 47
              "Agent start is state 47")
(check-equal? (get-state 9 5) 95
              "Goal is state 95")
(check-equal? (get-state 9 9) 99
              "Bottom-right corner is state 99")

;;; next-position : Number Number Symbol -> (values Number Number)
;;;
;;; Compute the candidate next position given the current position and an action.
;;; Strategy: cond on the action symbol to adjust x or y.
;;;
;;; Returns two values: next-x and next-y.
;;;
;;; agent-x : current row
;;; agent-y : current column
;;; action  : one of command-north, command-south, command-east, command-west
;;;
(define (next-position agent-x agent-y action)
  (void))

(let-values ([(nx ny) (next-position 4 7 'north)])
  (check-equal? (list nx ny) '(3 7)
                "North from (4,7) → (3,7)"))
(let-values ([(nx ny) (next-position 4 7 'south)])
  (check-equal? (list nx ny) '(5 7)
                "South from (4,7) → (5,7)"))
(let-values ([(nx ny) (next-position 4 7 'east)])
  (check-equal? (list nx ny) '(4 8)
                "East from (4,7) → (4,8)"))
(let-values ([(nx ny) (next-position 4 7 'west)])
  (check-equal? (list nx ny) '(4 6)
                "West from (4,7) → (4,6)"))

;;; move-agent : Number Number Symbol -> (values Number Number Number)
;;;
;;; Execute one movement step.  Computes the candidate next position, checks
;;; bounds, and returns the new position and the reward earned.
;;;
;;; From the book's pseudocode:
;;;   - Compute next_x, next_y from action
;;;   - If in bounds: reward = cost_movement(next_x, next_y), move there
;;;   - If out of bounds: reward = grid_OUT_OF_BOUNDS_REWARD, stay put
;;;
;;; Returns three values: new-agent-x, new-agent-y, reward.
;;;
;;; agent-x : current row
;;; agent-y : current column
;;; action  : one of command-north, command-south, command-east, command-west
;;;
(define (move-agent agent-x agent-y action)
  (void))

;; Moving north from (4,7) → (3,7) which is a car → -100
(let-values ([(ax ay reward) (move-agent 4 7 'north)])
  (check-equal? (list ax ay reward) '(3 7 -100)
                "North from agent start → empty cell, +100"))

;; Moving south from (4,7) → (5,7) which is a car → -100
(let-values ([(ax ay reward) (move-agent 4 7 'south)])
  (check-equal? (list ax ay reward) '(5 7 -100)
                "South from agent start → car, -100"))

;; Moving east from (4,7) → (4,8) which is empty → +100
(let-values ([(ax ay reward) (move-agent 4 7 'east)])
  (check-equal? (list ax ay reward) '(4 8 100)
                "East from agent start → empty cell, +100"))

;; Moving west from (4,7) → (4,6) which is a pedestrian → -500
(let-values ([(ax ay reward) (move-agent 4 7 'west)])
  (check-equal? (list ax ay reward) '(4 6 -500)
                "West from agent start → pedestrian, -500"))

;; Moving north from (0,0) → out of bounds → stay put, -150
(let-values ([(ax ay reward) (move-agent 0 0 'north)])
  (check-equal? (list ax ay reward) '(0 0 -150)
                "North from top-left → out of bounds, -150"))

;; Moving west from (0,0) → out of bounds → stay put, -150
(let-values ([(ax ay reward) (move-agent 0 0 'west)])
  (check-equal? (list ax ay reward) '(0 0 -150)
                "West from top-left → out of bounds, -150"))

;; Moving to the goal: south from (8,5) → (9,5) which is the goal → +500
(let-values ([(ax ay reward) (move-agent 8 5 'south)])
  (check-equal? (list ax ay reward) '(9 5 500)
                "South into goal → +500"))

;;;;
;;;; Run It
;;;;

(define (run)
  (displayln "")
  (displayln "=== Q-Learning Simulation Environment ===")
  (displayln "")
  (displayln (format "Grid size: ~a × ~a" grid-size-x grid-size-y))
  (displayln (format "Agent start: (~a, ~a)  [state ~a]"
                     agent-start-x agent-start-y
                     (get-state agent-start-x agent-start-y)))
  (displayln (format "Goal:        (~a, ~a)  [state ~a]"
                     goal-x goal-y
                     (get-state goal-x goal-y)))
  (displayln (format "State space: ~a states" (* grid-size-x grid-size-y)))
  (displayln (format "Actions:     ~a" actions))
  (displayln "")
  (displayln "Reward structure:")
  (displayln (format "  Empty cell:     ~a" grid-empty-reward))
  (displayln (format "  Goal:           ~a" grid-goal-reward))
  (displayln (format "  Car:            ~a" grid-obstacle-car-reward))
  (displayln (format "  Pedestrian:     ~a" grid-obstacle-person-reward))
  (displayln (format "  Out of bounds:  ~a" grid-out-of-bounds-reward))
  (displayln "")

  ;; Quick demo: move the agent from start in each direction
  (displayln "Demo: one step from agent start (4, 7) in each direction:")
  (for ([action actions])
    (let-values ([(ax ay reward) (move-agent agent-start-x agent-start-y action)])
      (displayln (format "  ~a → (~a, ~a)  reward: ~a  cell: ~a"
                         action ax ay reward
                         (if (and (equal? ax agent-start-x) (equal? ay agent-start-y))
                             "OUT OF BOUNDS"
                             (grid-ref grid ax ay)))))))

; (run)
