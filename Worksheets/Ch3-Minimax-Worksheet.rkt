#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 3: Minimax Search Algorithm
;;;;; Worksheet
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;
;;; Implement the minimax search algorithm to find the best move in a two-player adversarial game.
;;; Unlike simple search algorithms that look for a single goal, minimax assumes both players play
;;; optimally and alternates between maximizing and minimizing scores to find the best outcome.
;;;
;;; We'll use tic-tac-toe as our example game, where:
;;; - x is the maximizing player (trying to get the highest score)
;;; - o is the minimizing player (trying to get the lowest score)
;;; - The algorithm looks ahead multiple moves to choose the best strategy
;;;

;;;;
;;;; Constants
;;;;

;;; Board representation as a flat list of 9 positions
;;; Each position contains: 'x, 'o, or '_ (empty)
;;; Positions numbered 0-8, read left-to-right, top-to-bottom:
;;;   0 1 2
;;;   3 4 5
;;;   6 7 8
;;;
;;; Example board:
;;;   x o _
;;;   _ x _
;;;   _ _ o
;;;
(define example-board '(x o _ _ x _ _ _ o))

;;; Example mid-game board for minimax demonstration
;;; Visual representation:
;;;   x o x
;;;   _ x _
;;;   o _ o
;;;
(define example-midgame '(x o x _ x _ o _ o))

;;; generate-next-boards : List -> List-of-Boards
;;;
;;; Generate all possible next board states by making one move. Determines which player moves next by
;;; counting filled positions:
;;; - Even number of filled positions -> x moves next
;;; - Odd number of filled positions  -> o moves next
;;;
;;; (define (generate-next-boards board)
;;;   (let* ([filled-count ...]    ; How many positions are not '_
;;;          [current-player  ...] ; Even count means 'x, otherwise 'o
;;;          [empty-positions ...] ; Indices of all empty positions
;;;     (map (...) empty-positions))))
;;;
;;; board : list of 9 symbols, each 'x, 'o, or '_
;;;
;;; (check-equal? (generate-next-boards '(_ _ _ _ _ _ _ _ _))
;;;               '((x _ _ _ _ _ _ _ _)
;;;                 (_ x _ _ _ _ _ _ _)
;;;                 (_ _ x _ _ _ _ _ _)
;;;                 (_ _ _ x _ _ _ _ _)
;;;                 (_ _ _ _ x _ _ _ _)
;;;                 (_ _ _ _ _ x _ _ _)
;;;                 (_ _ _ _ _ _ x _ _)
;;;                 (_ _ _ _ _ _ _ x _)
;;;                 (_ _ _ _ _ _ _ _ x))
;;;               "Empty board should generate 9 boards with x in each position")
;;;
;;; (check-equal? (generate-next-boards '(x _ _ _ _ _ _ _ _))
;;;               '((x o _ _ _ _ _ _ _)
;;;                 (x _ o _ _ _ _ _ _)
;;;                 (x _ _ o _ _ _ _ _)
;;;                 (x _ _ _ o _ _ _ _)
;;;                 (x _ _ _ _ o _ _ _)
;;;                 (x _ _ _ _ _ o _ _)
;;;                 (x _ _ _ _ _ _ o _)
;;;                 (x _ _ _ _ _ _ _ o))
;;;               "After x's move, o should have 8 possible moves")
;;;
#;
(define (generate-next-boards board)
  (let* ([filled-count ...]    ; How many positions are not '_
         [current-player  ...] ; Even count means 'x, otherwise 'y
         [empty-positions ...] ; Indices of all empty positions
    (map (...) empty-positions))))

;;; check-winner : Board -> Symbol
;;;
;;; Determine the winner of a tic-tac-toe board.
;;; Produces 'x if x has won, 'o if o has won, 'draw if board is full with no winner, or '_ if game
;;; is still in progress.
;;;
;;; board : list of 9 symbols, each 'x, 'o, or '_
;;;
;;; (check-equal? (check-winner '(x x x _ _ _ _ _ _)) 'x
;;;               "x should win with top row")
;;; (check-equal? (check-winner '(o _ _ o _ _ o _ _)) 'o
;;;               "o should win with left column")
;;; (check-equal? (check-winner '(x _ _ _ x _ _ _ x)) 'x
;;;               "x should win with diagonal")
;;; (check-equal? (check-winner '(x o x o x o o x o)) 'draw
;;;               "Full board with no winner should be a draw")
;;; (check-equal? (check-winner '(x o _ _ _ _ _ _ _)) '_
;;;               "Game in progress should produce '_")
;;;
#;
(define (check-winner board)
  ...)

;;; evaluate-board : Board -> Number
;;;
;;; Evaluate a terminal board state from x's perspective.
;;; Produces: +10 if x wins, -10 if o wins, 0 otherwise
;;;
;;; board : list of 9 symbols, each 'x, 'o, or '_
;;;
;;; (check-equal? (evaluate-board '(x x x _ _ _ _ _ _)) 10
;;;               "x win should score +10")
;;; (check-equal? (evaluate-board '(o o o _ _ _ _ _ _)) -10
;;;               "o win should score -10")
;;; (check-equal? (evaluate-board '(x o x o x o o x o)) 0
;;;               "Draw should score 0")
;;;
#;
(define (evaluate-board board)
  ...)

;;; This is not required for minimax, but you may use it to generate the game tree.  Minimax doesn't
;;; need it, because it calls generate-next-boards on the fly.
;;;
;;; build-game-tree : List Number -> Tree
;;;
;;; Build a game tree to the specified depth, where each node contains the board state and a list of
;;; child nodes representing all possible moves. A tree node is represented as: (list board (list
;;; child1 child2 ...))
;;;
;;; (define (build-game-tree board depth)
;;;   (if ...
;;;       ...
;;;       ...))
;;;
;;; board : list of 9 symbols, each 'x, 'o, or '_
;;; depth : non-negative integer representing how many levels deep to build
;;;
;;; (check-equal? (build-game-tree '(x x x _ _ _ _ _ _) 1)
;;;               '((x x x _ _ _ _ _ _) ())
;;;               "Terminal board (x wins) should have no children")
;;; (check-equal? (build-game-tree '(_ _ _ _ _ _ _ _ _) 0)
;;;               '((_ _ _ _ _ _ _ _ _) ())
;;;               "Depth 0 should produce just the board with no children")
;;;
#;
(define (build-game-tree board depth)
  (if (or (= depth 0)
          (not (equal? (check-winner board) '_)))
      ;; Base case: reached depth limit or game is over
      (list board '())
      ;; Recursive case: generate all next boards and build their subtrees
      (let ([next-boards (generate-next-boards board)])
        (list board
              (map (lambda (next-board)
                     (build-game-tree next-board (- depth 1)))
                   next-boards)))))

;;; minimax : Board Number Boolean -> (List (or Number False) Number)
;;;
;;; Find the best move for the current player using the minimax algorithm.
;;; Produces a list containing (best-move best-score).
;;; Uses simple evaluation (only terminal states: win=10, loss=-10, draw=0).
;;;
;;; Algorithm (as outlined in book):
;;; 1. Given a game state, whether the current mode is minimization or maximization, and a current
;;;    depth, the algorithm can start
;;; 2. Is current an end state or depth is 0?
;;; 3. Produce the current score and last move
;;; 4. Is current mode MAX?
;;; 5. Set best known score as +∞ (if minimizing)
;;; 6. Set best known score as -∞ (if maximizing)
;;; 7. Get all possible moves, given current game state
;;; 8. Has next valid move?
;;; 9. Copy current game state as game_n
;;; 10. Simulate by applying move to game state game_n
;;; 11. Set best_n as the result of running this algorithm recursively
;;; 12. If current mode is MAX?
;;; 13. Is best_n less than known best? (for MAX mode)
;;; 14. Is best_n greater than known best? (for MIN mode)
;;; 15. Set known best as best_n
;;;
;;; board         : list of 9 symbols, each 'x, 'o, or '_
;;; depth         : non-negative integer for search depth  
;;; is-maximizing : #t if maximizing player (x), #f if minimizing player (o)
;;;
;;; Produces (list move score) where move is #f if terminal/depth-0; otherwise pos 0-8
;;;
#;
(define (minimax board depth is-maximizing)
  ...)

;; Test: depth 0 produces evaluation
;; Board: x x _
;;        o o _
;;        _ _ _
;; (check-equal? (minimax '(x x _ o o _ _ _ _) 0 #t)
;;               '(#f 0)
;;               "At depth 0, should produce evaluation with no move")

;; Test: x can win immediately (2 pieces = even, x's turn)
;; Board: x x _
;;        _ _ _
;;        _ _ _
;; (check-equal? (car (minimax '(x x _ _ _ _ _ _ _) 1 #t))
;;               2
;;               "x should play position 2 to complete top row")
;; (check-equal? (cadr (minimax '(x x _ _ _ _ _ _ _) 1 #t))
;;               10
;;               "x should see winning score")

;; Test: x should win (not just block) when possible (4 pieces = even, x's turn)
;; Board: x o _
;;        x o _
;;        _ _ _
;; (check-equal? (car (minimax '(x o _ x o _ _ _ _) 1 #t))
;;               6
;;               "x should win at position 6 (left column)")
;; (check-equal? (cadr (minimax '(x o _ x o _ _ _ _) 1 #t))
;;               10
;;               "x should see winning score")

;; Test: x must block o's threat (4 pieces = even, x's turn)
;; Board: x o _
;;        _ o _
;;        _ _ x
;; (check-equal? (car (minimax '(x o _ _ o _ _ _ x) 2 #t))
;;               7
;;               "x should block o's middle column at position 7")

;; Test: o should take winning move (3 pieces = odd, o's turn)
;; Board: x _ _
;;        o o _
;;        _ _ _
;; (check-equal? (car (minimax '(x _ _ o o _ _ _ _) 1 #f))
;;               5
;;               "o should win at position 5")
;; (check-equal? (cadr (minimax '(x _ _ o o _ _ _ _) 1 #f))
;;               -10
;;               "o should see winning score")

;; Test: terminal state - x already won
;; Board: x x x
;;        _ _ _
;;        _ _ _
;; (check-equal? (minimax '(x x x _ _ _ _ _ _) 3 #t)
;;               '(#f 10)
;;               "x already won, no move should be needed")

;; Test: terminal state - o already won
;; Board: o o o
;;        _ _ _
;;        _ _ _
;; (check-equal? (minimax '(o o o _ _ _ _ _ _) 3 #t)
;;               '(#f -10)
;;               "o already won, no move should be needed")

;; Test: draw board
;; Board: x o x
;;        o x o
;;        o x o
;; (check-equal? (minimax '(x o x o x o o x o) 3 #t)
;;               '(#f 0)
;;               "Draw board should score 0")
