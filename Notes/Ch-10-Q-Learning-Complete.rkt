#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 10: Q-Learning — Parking Lot Simulation
;;;;; Reference Implementation (Part 1: Environment)
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
;;;;; NOTE: In order to make it so we can lay around with grid-* constants,
;;;;; I've modified the check-expects to account for variables rather than 
;;;;; hardcoding the constants.

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
(define grid-empty-reward           -1)
(define grid-goal-reward             500)
(define grid-obstacle-car-reward    -1000)
(define grid-obstacle-person-reward -1000)
(define grid-out-of-bounds-reward   -150)

;; Demonstration notes:
;;
;; A few things to note: as the constants above stand, it doesn't quite work, because
;; the reward for stepping into an empty space is too big (encouraging cycling), and
;; the penalty for going through car is too low (encouraging running into cars).
;; The solution is to: set grid-empty-reward to -1, which discourages moving around
;; for the sake of moving; and set obstacle-car-reward to -1000.
;;
;; ALSO: According to the grid below, there's no rectilinear way out!  Change the grid
;; so that the agent doesn't have to crash through another car.

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
    (- P - - - - - - - P)   ; Row 1
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
  (list-ref (list-ref grid x) y))

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
  (and (<= 0 next-x) (< next-x grid-size-x)
       (<= 0 next-y) (< next-y grid-size-y)))

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
  (let ([cell (grid-ref grid next-x next-y)])
    (cond
      [(equal? cell pedestrian) grid-obstacle-person-reward]
      [(equal? cell car-cell)   grid-obstacle-car-reward]
      [(equal? cell goal)       grid-goal-reward]
      [else                     grid-empty-reward])))

(check-equal? (cost-movement 0 0) grid-empty-reward
              "Empty cell → grid-empty-reward")
(check-equal? (cost-movement 0 1) grid-obstacle-car-reward
              "Car → grid-obstacle-car-reward")
(check-equal? (cost-movement 1 1) grid-obstacle-person-reward
              "Pedestrian → grid-obstacle-person-reward")
(check-equal? (cost-movement 9 5) grid-goal-reward
              "Goal → grid-goal-reward")
(check-equal? (cost-movement 4 7) grid-empty-reward
              "Agent start position is empty → grid-empty-reward")

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
  (and (equal? agent-x goal-x)
       (equal? agent-y goal-y)))

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
  (+ (* grid-size-x agent-x) agent-y))

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
  (cond
    [(equal? action command-north) (values (- agent-x 1) agent-y)]
    [(equal? action command-south) (values (+ agent-x 1) agent-y)]
    [(equal? action command-east)  (values agent-x       (+ agent-y 1))]
    [(equal? action command-west)  (values agent-x       (- agent-y 1))]))

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
  (let-values ([(next-x next-y) (next-position agent-x agent-y action)])
    (if (is-within-bounds next-x next-y)
        (values next-x next-y (cost-movement next-x next-y))
        (values agent-x agent-y grid-out-of-bounds-reward))))

;; Moving north from (4,7) → (3,7) which is a car → -100
(let-values ([(ax ay reward) (move-agent 4 7 'north)])
  (check-equal? (list ax ay reward) (list 3 7 grid-obstacle-car-reward)
                "North from agent start → car, grid-obstacle-car-reward"))

;; Moving south from (4,7) → (5,7) which is a car → grid-obstacle-car-reward
(let-values ([(ax ay reward) (move-agent 4 7 'south)])
  (check-equal? (list ax ay reward) (list 5 7 grid-obstacle-car-reward)
                "South from agent start → car, grid-obstacle-car-reward"))

;; ***Changed from worksheet***
;; In order to accommodate new value for entering empty cell.
;; Moving east from (4,7) → (4,8) which is empty → -1
(let-values ([(ax ay reward) (move-agent 4 7 'east)])
  (check-equal? (list ax ay reward) (list 4 8 grid-empty-reward)
                "East from agent start → empty cell, grid-empty-reward"))

;; Moving west from (4,7) → (4,6) which is a pedestrian → grid-obstacle-person-reward
(let-values ([(ax ay reward) (move-agent 4 7 'west)])
  (check-equal? (list ax ay reward) (list 4 6 grid-obstacle-person-reward)
                "West from agent start → pedestrian, grid-obstacle-person-reward"))

;; Moving north from (0,0) → out of bounds → stay put, grid-out-of-bounds-reward
(let-values ([(ax ay reward) (move-agent 0 0 'north)])
  (check-equal? (list ax ay reward) (list 0 0 grid-out-of-bounds-reward)
                "North from top-left → out of bounds, grid-out-of-bounds-reward"))

;; Moving west from (0,0) → out of bounds → stay put, grid-out-of-bounds-reward
(let-values ([(ax ay reward) (move-agent 0 0 'west)])
  (check-equal? (list ax ay reward) (list 0 0 grid-out-of-bounds-reward)
                "West from top-left → out of bounds, grid-out-of-bounds-reward"))

;; Moving to the goal: south from (8,5) → (9,5) which is the goal → +500
(let-values ([(ax ay reward) (move-agent 8 5 'south)])
  (check-equal? (list ax ay reward) (list 9 5 grid-goal-reward)
                "South into goal → grid-goal-reward"))

;;;;
;;;; Q-Table Representation
;;;;
;;;; The Q-table is represented as an immutable list of lists with shape
;;;; [num-states][num-actions].  Each cell q-table[s][a] stores the expected
;;;; cumulative reward of taking action a from state s.  Updates rebuild the
;;;; table immutably rather than mutating in place.
;;;;

;;; action-index : Symbol -> Number
;;;
;;; Look up the index of an action symbol in the global actions list.
;;; Strategy: linear search through actions with index-of.
;;;
;;; action : one of command-north, command-south, command-east, command-west
;;;
(define (action-index action)
  (index-of actions action))

(check-equal? (action-index 'north) 0 "north is index 0")
(check-equal? (action-index 'south) 1 "south is index 1")
(check-equal? (action-index 'east)  2 "east is index 2")
(check-equal? (action-index 'west)  3 "west is index 3")

;;; make-q-table : Number Number -> (List-of (List-of Number))
;;;
;;; Build a Q-table initialized with zeros.
;;; Strategy: nested build-list to produce a num-states × num-actions matrix.
;;;
;;; num-states  : size of the observation space
;;; num-actions : size of the action space
;;;
(define (make-q-table num-states num-actions)
  (build-list num-states
              (lambda (_)
                (build-list num-actions (const 0)))))

(check-equal? (make-q-table 2 3) '((0 0 0) (0 0 0))
              "2 states × 3 actions, all zeros")
(check-equal? (length (make-q-table 100 4)) 100
              "100 states")
(check-equal? (length (car (make-q-table 100 4))) 4
              "4 actions per state")

;;; q-ref : (List-of (List-of Number)) Number Number -> Number
;;;
;;; Look up the Q-value for a (state, action) pair.
;;; Strategy: nested list-ref into the matrix.
;;;
;;; q-table : the Q-table
;;; state   : state index
;;; action  : action index
;;;
(define (q-ref q-table state action)
  (list-ref (list-ref q-table state) action))

(check-equal? (q-ref '((1 2) (3 4)) 0 0) 1 "Top-left")
(check-equal? (q-ref '((1 2) (3 4)) 1 1) 4 "Bottom-right")
(check-equal? (q-ref '((1 2) (3 4)) 0 1) 2 "Top-right")

;;; q-set : (List-of (List-of Number)) Number Number Number
;;;         -> (List-of (List-of Number))
;;;
;;; Return a new Q-table with the value at (state, action) replaced.
;;; Strategy: rebuild only the affected row, share other rows by reference.
;;;
;;; q-table : the Q-table
;;; state   : state index
;;; action  : action index
;;; value   : the new Q-value
;;;
(define (q-set q-table state action value)
  (append (take q-table state)
          (list (let ([row (list-ref q-table state)])
                  (append (take row action)
                          (list value)
                          (drop row (+ action 1)))))
          (drop q-table (+ state 1))))

(check-equal? (q-set '((0 0) (0 0)) 0 0 5) '((5 0) (0 0))
              "Set top-left to 5")
(check-equal? (q-set '((0 0) (0 0)) 1 1 7) '((0 0) (0 7))
              "Set bottom-right to 7")
(check-equal? (q-set '((1 2 3) (4 5 6)) 0 1 99) '((1 99 3) (4 5 6))
              "Set middle of first row")

;;;;
;;;; Action Selection
;;;;

;;; argmax-action : (List-of (List-of Number)) Number -> Symbol
;;;
;;; Choose the action with the highest Q-value for the given state.
;;; Ties are broken by the order of actions in the global actions list
;;; (i.e. the first occurrence of the max wins).
;;; Strategy: pair each Q-value with its action, find the max by Q-value.
;;;
;;; q-table : the Q-table
;;; state   : state index
;;;
(define (argmax-action q-table state)
  (let* ([q-values (list-ref q-table state)]
         [paired   (map cons q-values actions)])
    (cdr (argmax car paired))))

;; With all zeros, ties are broken in favour of the first action (north)
(check-equal? (argmax-action (make-q-table 100 4) 0) 'north
              "All-zero row: first action wins on tie")
;; Construct a table where action 2 (east) has the highest value
(let ([q (q-set (make-q-table 100 4) 5 2 10)])
  (check-equal? (argmax-action q 5) 'east
                "East has highest Q-value at state 5"))
;; Action 3 (west) has the highest value
(let ([q (q-set (make-q-table 100 4) 7 3 100)])
  (check-equal? (argmax-action q 7) 'west
                "West has highest Q-value at state 7"))

;;; choose-action : (List-of (List-of Number)) Number Number -> Symbol
;;;
;;; Select an action using an epsilon-greedy strategy.
;;; With probability chance-of-random-move, select a random action;
;;; otherwise select the greedy action (highest Q-value for this state).
;;;
;;; NOTE: The book's pseudocode reads
;;;   if random.uniform(0, 1) > chance_of_random_move: get_random_move()
;;; which would make chance_of_random_move the probability of being GREEDY,
;;; not random — opposite of what the name says.  We flip the comparison
;;; to < so the parameter name matches the behavior: a value of 0.1 means
;;; "explore 10% of the time".
;;;
;;; Strategy: roll a uniform random number, compare against the threshold.
;;;
;;; q-table              : the Q-table
;;; state                : current state index
;;; chance-of-random-move : probability in [0, 1] of choosing randomly
;;;
(define (choose-action q-table state chance-of-random-move)
  (if (< (random) chance-of-random-move)
      (list-ref actions (random (length actions)))
      (argmax-action q-table state)))

;; With chance-of-random-move = 0, always pick the greedy action
(let ([q (q-set (make-q-table 100 4) 0 1 10)])
  (check-equal? (choose-action q 0 0) 'south
                "chance=0: always greedy"))
;; With chance-of-random-move = 1, always pick randomly; result is in actions
(for ([_ (in-range 20)])
  (check-not-false (member (choose-action (make-q-table 100 4) 0 1) actions)
                   "chance=1: random action is one of the four"))

;;;;
;;;; Q-Learning Update Rule
;;;;

;;; q-update : (List-of (List-of Number)) Number Symbol Number Number
;;;            Number Number -> (List-of (List-of Number))
;;;
;;; Apply the Q-learning update rule and return a new Q-table:
;;;
;;;   new_value = (1 - α) · Q(s,a) + α · (reward + γ · max(Q(s', ·)))
;;;
;;; where α is the learning rate and γ is the discount factor.
;;; Strategy: compute the new value, then use q-set to rebuild the table.
;;;
;;; q-table       : current Q-table
;;; state         : state index s
;;; action        : action symbol a (will be converted to index)
;;; reward        : observed reward
;;; next-state    : state index s' reached after taking action
;;; learning-rate : α in [0, 1]
;;; discount      : γ in [0, 1]
;;;
(define (q-update q-table state action reward next-state learning-rate discount)
  (let* ([a-idx          (action-index action)]
         [current-value  (q-ref q-table state a-idx)]
         [next-max-value (apply max (list-ref q-table next-state))]
         [new-value      (+ (* (- 1 learning-rate) current-value)
                            (* learning-rate
                               (+ reward (* discount next-max-value))))])
    (q-set q-table state a-idx new-value)))

;; From a zero Q-table, one update at state 0 / action 'north' with reward 100,
;; α=0.5, γ=0.9, next-state=10 (also all zeros):
;;   new = (1 - 0.5)·0 + 0.5·(100 + 0.9·0) = 50
(let* ([q0 (make-q-table 100 4)]
       [q1 (q-update q0 0 'north 100 10 0.5 0.9)])
  (check-within (q-ref q1 0 0) 50.0 0.001
                "Single update from zero table: α=0.5, reward=100"))

;; With α=1.0, the new value is just reward + γ · max(next)
(let* ([q0 (make-q-table 100 4)]
       [q1 (q-update q0 0 'east 50 5 1.0 0.9)])
  (check-within (q-ref q1 0 2) 50.0 0.001
                "α=1.0: new value is reward when next state is all zeros"))

;; With α=0.0, the Q-table is unchanged
(let* ([q0 (q-set (make-q-table 100 4) 0 0 42)]
       [q1 (q-update q0 0 'north 100 10 0.0 0.9)])
  (check-within (q-ref q1 0 0) 42.0 0.001
                "α=0.0: Q-value unchanged"))

;; Bootstrapping: if next state has a known high value, it propagates back
(let* ([q0 (q-set (make-q-table 100 4) 10 0 100)]   ; Q(10, north) = 100
       [q1 (q-update q0 0 'north 0 10 1.0 0.9)])    ; α=1, γ=0.9
  (check-within (q-ref q1 0 0) 90.0 0.001
                "Bootstrapping: next max 100, γ=0.9 → new value 90"))

;;;;
;;;; Training
;;;;

;;; train-with-q-learning : Number Number Number Number Number
;;;                         -> (List-of (List-of Number))
;;;
;;; Train a Q-table by running Q-learning for the specified number of
;;; iterations.  Each iteration is one attempt to reach the goal from the
;;; agent's start position.  Returns the trained Q-table.
;;;
;;; Algorithm (per the book's pseudocode):
;;;   1. Initialize Q-table as a zero matrix
;;;   2. For each iteration:
;;;      a. Reset the agent to the start position
;;;      b. While the goal is not reached (and step cap not exceeded):
;;;         - Choose an action (random with probability ε, else greedy)
;;;         - Apply the action via move-agent → reward, next state
;;;         - Update Q(s, a) using the Q-learning update rule
;;;         - Set state ← next state
;;;
;;; Note: With the book's reward structure (empty cells = +100), the agent
;;; can get rationally stuck in a cycle between two empty cells (each loop
;;; step harvests +100, geometric series of +100 with γ=0.9 exceeds the
;;; +500 goal reward). The step cap bounds episode length so training time
;;; stays tractable and so we don't spend all our compute reinforcing loops.
;;; This is a useful teaching moment: the +100 step reward is a poor design
;;; choice that the cap doesn't actually fix — the learned policy may still
;;; cycle. Standard RL practice is a small negative step cost instead.
;;;
;;; observation-space     : number of states (e.g. 100 for the 10×10 grid)
;;; action-space          : number of actions (always 4 here)
;;; number-of-iterations  : how many episodes to run
;;; learning-rate         : α in [0, 1]
;;; discount              : γ in [0, 1]
;;; chance-of-random-move : ε in [0, 1] (probability of exploring)
;;; max-steps-per-episode : hard cap on steps per episode
;;;
;;; Strategy: outer named let over iterations, inner named let over the
;;; episode steps.  Position is threaded explicitly as (agent-x, agent-y);
;;; move-agent returns (values new-x new-y reward) so no simulator object
;;; is needed.
;;;
(define (train-with-q-learning observation-space
                               action-space
                               number-of-iterations
                               learning-rate
                               discount
                               chance-of-random-move
                               max-steps-per-episode)
  (let iter-loop ([i       0]
                  [q-table (make-q-table observation-space action-space)])
    (if (= i number-of-iterations)
        q-table
        ;; One episode: start fresh, step until goal reached or cap hit
        (let episode-loop ([agent-x agent-start-x]
                           [agent-y agent-start-y]
                           [steps   0]
                           [q       q-table])
          (let ([state (get-state agent-x agent-y)])
            (cond
              ;; Goal reached: episode done, start the next iteration
              [(is-goal-achieved agent-x agent-y)
               (iter-loop (+ i 1) q)]
              ;; Step cap hit: episode done, start the next iteration
              [(>= steps max-steps-per-episode)
               (iter-loop (+ i 1) q)]
              [else
               ;; Choose action, step, update Q
               (let ([action (choose-action q state chance-of-random-move)])
                 (let-values ([(new-x new-y reward) (move-agent agent-x agent-y action)])
                   (let* ([next-state (get-state new-x new-y)]
                          [new-q      (q-update q state action reward next-state
                                                learning-rate discount)])
                     (episode-loop new-x new-y (+ steps 1) new-q))))]))))))

;; Smoke test: training runs and returns a Q-table of the right shape
;; (let ([trained (train-with-q-learning 100 4 5 0.1 0.9 0.5 200)])
;;   (check-equal? (length trained) 100
;;                 "Trained Q-table has 100 state rows")
;;   (check-equal? (length (car trained)) 4
;;                 "Trained Q-table has 4 action columns per row"))

;; Goal-adjacent state: after some training, the value of stepping into the
;; goal should be positive.  From state 85 (= row 8, col 5), action 'south
;; lands on the goal (+500 reward).
;; (let ([trained (train-with-q-learning 100 4 50 0.5 0.9 0.3 200)])
;;   (check-equal? (> (q-ref trained 85 (action-index 'south)) 0) #t
;;                 "After training, Q(state-above-goal, south) > 0"))

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

;;;;
;;;; Run Training
;;;;

;;; greedy-path : (List-of (List-of Number)) Number -> (List-of (List Number Number))
;;;
;;; Trace the greedy path from the start position using a trained Q-table.
;;; Stops when the goal is reached or after max-steps to avoid infinite loops
;;; (the greedy policy can cycle if training was insufficient).
;;; Strategy: step-by-step simulation, accumulating positions in a list.
;;;
;;; q-table   : the trained Q-table
;;; max-steps : safety bound on path length
;;;
(define (greedy-path q-table max-steps)
  (let loop ([agent-x agent-start-x]
             [agent-y agent-start-y]
             [steps   0]
             [path    (list (list agent-start-x agent-start-y))])
    (cond
      [(is-goal-achieved agent-x agent-y) (reverse path)]
      [(>= steps max-steps)               (reverse (cons 'cycled path))]
      [else
       (let ([action (argmax-action q-table (get-state agent-x agent-y))])
         (let-values ([(new-x new-y _) (move-agent agent-x agent-y action)])
           (loop new-x new-y (+ steps 1) (cons (list new-x new-y) path))))])))

;;; run-training : Number -> Void
;;;
;;; Train a Q-table with the given number of iterations and print a summary.
;;; Demonstrates the trained policy by tracing the greedy path from start.
;;;
;;; number-of-iterations : how many episodes to train for
;;;
(define (run-training number-of-iterations [learning-rate 0.1] [discount 0.9] [epsilon 0.3] [max-steps 50])
  (displayln "")
  (displayln "=== Q-Learning Training ===")
  (displayln "")
  (displayln (format "Iterations:            ~a" number-of-iterations))
  (displayln (format "Learning rate (α):     ~a" learning-rate))
  (displayln (format "Discount (γ):          ~a" discount))
  (displayln (format "Random move ε:         ~a" epsilon))
  (displayln (format "Max steps / episode:   ~a" max-steps))
  (displayln "")
  (displayln "Training...")
  (let* ([start-time (current-inexact-milliseconds)]
         [trained    (train-with-q-learning 100 4 number-of-iterations learning-rate discount
                                            epsilon max-steps)]
         [elapsed    (- (current-inexact-milliseconds) start-time)])
    (displayln (format "Done in ~a ms" (round elapsed)))
    (displayln "")
    (displayln (format "Q-values at agent start (state ~a):"
                       (get-state agent-start-x agent-start-y)))
    (let ([row (list-ref trained (get-state agent-start-x agent-start-y))])
      (for ([action actions]
            [q-val row])
        (displayln (format "  ~a: ~a" action (real->decimal-string q-val 3)))))
    (displayln "")
    (displayln "Greedy path from start (capped at 50 steps):")
    (let ([path (greedy-path trained 50)])
      (displayln (format "  Length: ~a steps" (- (length path) 1)))
      (displayln (format "  Path:   ~a" path)))))

(run-training 1000)
