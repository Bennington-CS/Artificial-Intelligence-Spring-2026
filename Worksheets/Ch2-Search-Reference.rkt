#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 2: Adjacency Matrices and Search
;;;;; Reference Implementation
;;;;;

;;;;
;;;; Problem Statement
;;;;

;;;
;;; Use an adjacency matrix representation of a simple (undirected, unweighted) map and perform a
;;; breadth-first search.
;;; 
;;; Sample visual representation of the map:
;;;
;;;   A --- B --- C
;;;   |     |     |
;;;   D --- E --- F
;;;   |     |     |
;;;   G --- H --- I
;;;   |     |     |
;;;   J --- K --- L
;;;   |     |     |
;;;   M --- N --- O

;;;;
;;;; Constants
;;;; 

;;; Representation of the map as an adjacency matrix:
;;; - Each row represents a node (A=0, B=1), ...
;;; - Each column represents a possible connexion
;;; - 1 indicates an edge; 0 indicates no edge
(define map-matrix
  ;; A B C D E F G H I J K L M N O
  '((0 1 0 1 0 0 0 0 0 0 0 0 0 0 0)   ; A: connects to B, D
    (1 0 1 0 1 0 0 0 0 0 0 0 0 0 0)   ; B: connects to A, C, E
    (0 1 0 0 0 1 0 0 0 0 0 0 0 0 0)   ; C: connects to B, F
    (1 0 0 0 1 0 1 0 0 0 0 0 0 0 0)   ; D: connects to A, E, G
    (0 1 0 1 0 1 0 1 0 0 0 0 0 0 0)   ; E: connects to B, D, F, H
    (0 0 1 0 1 0 0 0 1 0 0 0 0 0 0)   ; F: connects to C, E, I
    (0 0 0 1 0 0 0 1 0 1 0 0 0 0 0)   ; G: connects to D, H, J
    (0 0 0 0 1 0 1 0 1 0 1 0 0 0 0)   ; H: connects to E, G, I, K
    (0 0 0 0 0 1 0 1 0 0 0 1 0 0 0)   ; I: connects to F, H, L
    (0 0 0 0 0 0 1 0 0 0 1 0 1 0 0)   ; J: connects to G, K, M
    (0 0 0 0 0 0 0 1 0 1 0 1 0 1 0)   ; K: connects to H, J, L, N
    (0 0 0 0 0 0 0 0 1 0 1 0 0 0 1)   ; L: connects to I, K, O
    (0 0 0 0 0 0 0 0 0 1 0 0 0 1 0)   ; M: connects to J, N
    (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1)   ; N: connects to K, M, O
    (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))) ; O: connects to L, N

;;;;
;;;; Wishes (helper functions)
;;;;

;;; get-neighbours : List Number -> List
;;;
;;; Get all the neighbours of a node from an adjacency matrix.  This function consumes a matrix and a
;;; number corresponding to the row of the matrix, and produces a list of all nodes that have a 1 (a
;;; connexion) in that row.
;;;
;;; (define (get-neighbours matrix node)
;;;   ... neighbours ...)
;;;
;;; matrix  : a list of lists representing an adjacency matrix
;;; node    : a number representing the node index to find neighbours for
;;;
;;; (check-equal? (get-neighbours map-matrix 3)  '(0 4 6)
;;;               "D connects to A, E, G")
;;; (check-equal? (get-neighbours map-matrix 14) '(11 13)
;;;               "O connects to L, N")
;;;
(define (get-neighbours matrix node)
  (let ([row (list-ref matrix node)])
    ;; Use for/list to get dual iterators
    (for/list ([connection (in-list row)] ; Walk through the connection values (0s and 1s) in row
               [index (in-naturals)]      ; Provide corresponding indices (0, 1, 2, ...)
               #:when (= connection 1))   ; Filter for cases where connection is 1
      index)))

(check-equal? (get-neighbours map-matrix 3)  '(0 4 6)
              "D connects to A, E, G")
(check-equal? (get-neighbours map-matrix 14) '(11 13)
              "O connects to L, N")

;;; Syntax help: Here is an example of how for/list interacts with #:when
#;
(for/list ([i '(1 2 3)]
           [j "abc"]
           #:when (odd? i))
  (list i j))

;;; pretty-print : List -> String
;;;
;;; Convert a list of node indices to a human-readable path string. This function consumes a list of
;;; node indices and produces a string with node letters joined by arrows, or an empty string if the
;;; input is false.
;;;
;;; (define (pretty-print result)
;;;   (if result
;;;       (string-join ... integer->char ...)
;;;       "")
;;;
;;; (check-equal? (pretty-print '(0 1 4)) "A -> B -> E"
;;;               "Path from A -> E via B")
;;; (check-equal? (pretty-print #f) ""
;;;               "No path produces empty string")
;;;
(define (pretty-print result)
  (if result
      ;; Convert each node index to a letter (0->A, 1->B, ...) and join with arrows
      (string-join (map (lambda (n) (string (integer->char (+ n 65)))) result) " -> ")
      ""))

(check-equal? (pretty-print '(0 1 4)) "A -> B -> E"
              "Path from A -> E via B")
(check-equal? (pretty-print #f) ""
              "No path produces empty string")

;;;;
;;;; Definitions
;;;;

;;; bfs-shortest-path : List Number Number -> List
;;;
;;; Given an adjacency matrix, a starting node, and an ending node, use breadth-first search to return
;;; a list of nodes that must be traversed.  Produces the discovered path as a list, or #f if there is
;;; no path.
;;;
;;; (define (bfs-shortest-path matrix start goal)
;;;   ...)
;;;
;;; matrix : list of lists representing the adjacency matrix
;;; start  : a number representing the node index to start from
;;; goal   : a number representing the node index to end at
;;;
;;; Algorithm:
;;; - Start with a queue containing the start node and its path
;;; - Mark the start node as visited
;;; - While queue is not empty:
;;;    - Dequeue the first item
;;;    - If it's the goal, return the path
;;;    - Otherwise, get all unvisited neighbours
;;;    - Add neighbours to queue with updated paths
;;;    - Mark newly discovered neighbours as visited
;;; - If queue becomes empty, no path exists
;;;
;;; (check-equal? (bfs-shortest-path map-matrix 0 4) '(0 1 4)
;;;               "Shortest path from A -> E should be A -> B -> E")
;;; (check-equal? (bfs-shortest-path map-matrix 0 14) '(0 1 2 5 8 11 14)
;;;               "Path from A -> O should traverse the grid")
;;; (check-equal? (bfs-shortest-path map-matrix 5 5) '(5)
;;;               "Path from a node to itself should be just that node")
;;; (check-equal? (bfs-shortest-path '((0 1) (1 0) (0 0)) 0 2) #f
;;;               "No path to disconnected node")
;;;
(define (bfs-shortest-path matrix start goal)
  ;; loop:    named let for the bfs loop
  ;; queue:   each item in the queue is (list node path-to-node)
  ;; visited: list of all nodes that we've seen
  (let loop ([queue (list (list start (list start)))]
             [visited (list start)])
    ;; Base case: if queue is empty, return #f because no path was found
    (if (empty? queue)
        #f
        ;; Otherwise, destructure: current item is (list node path), rest is remaining queue
        (let* ([current-item (car queue)]        ; Eg, '((0 (1 3)) (1 (0 2 4)))  -> '(0 (1 3))
               [current-node (car current-item)] ; Eg, '(0 (1 3))                -> 0
               [path (cadr current-item)]        ; Eg. '(0 (1 3))                -> '(1 3)
               [rest-queue (cdr queue)])         ; Eg, '((0 (1 3)) (1 (0 2 4)))  -> '(1 (0 2 4))
          (if (equal? current-node goal)
              ;; Success: return the path
              path
              ;; Continue exploring: add neighbours to queue
              (let* ([neighbours (get-neighbours matrix current-node)]
                     ;; Filter to only unvisited neighbours (avoid cycles)
                     [unvisited-neighbours
                       (filter-not (lambda (n) (member n visited)) neighbours)]
                     ;; Update visited list with new neighbours
                     [new-visited
                       (append visited unvisited-neighbours)]
                     ;; Create queue items for each unvisited neighbour; each item contains the
                     ;; neighbour and the path to reach it
                     [new-queue-items
                       (map (lambda (n) (list n (append path (list n)))) unvisited-neighbours)]
                     ;; Add new items to end of queue (breadth-first)
                     [new-queue
                       (append rest-queue new-queue-items)])
                ;; Recurse with updated queue and visited list
                (loop new-queue new-visited)))))))

#|
Trace: queue, visited

┌──────┬─────────────────────────────────────────────┬─────────────────┬──────────────┬────────────┬─────────────┬──────────────────┬─────────────────────────────────────────┐
│ Iter │ Queue (at start of iteration)               │ Visited         │ Current Node │ Path       │ Goal Found? │ Neighbours       │ Queue (at end, after adding new items)  │
├──────┼─────────────────────────────────────────────┼─────────────────┼──────────────┼────────────┼─────────────┼──────────────────┼─────────────────────────────────────────┤
│  0   │ ((0 (0)))                                   │ (0)             │ 0            │ (0)        │ No          │ (1 3)            │ ((1 (0 1)) (3 (0 3)))                   │
│      │                                             │                 │              │            │             │ unvisited: (1 3) │                                         │
├──────┼─────────────────────────────────────────────┼─────────────────┼──────────────┼────────────┼─────────────┼──────────────────┼─────────────────────────────────────────┤
│  1   │ ((1 (0 1)) (3 (0 3)))                       │ (0 1 3)         │ 1            │ (0 1)      │ No          │ (0 2 4)          │ ((3 (0 3)) (2 (0 1 2)) (4 (0 1 4)))     │
│      │                                             │                 │              │            │             │ unvisited: (2 4) │                                         │
├──────┼─────────────────────────────────────────────┼─────────────────┼──────────────┼────────────┼─────────────┼──────────────────┼─────────────────────────────────────────┤
│  2   │ ((3 (0 3)) (2 (0 1 2)) (4 (0 1 4)))         │ (0 1 3 2 4)     │ 3            │ (0 3)      │ No          │ (0 4 6)          │ ((2 (0 1 2)) (4 (0 1 4)) (6 (0 3 6)))   │
│      │                                             │                 │              │            │             │ unvisited: (6)   │                                         │
├──────┼─────────────────────────────────────────────┼─────────────────┼──────────────┼────────────┼─────────────┼──────────────────┼─────────────────────────────────────────┤
│  3   │ ((2 (0 1 2)) (4 (0 1 4)) (6 (0 3 6)))       │ (0 1 3 2 4 6)   │ 2            │ (0 1 2)    │ No          │ (1 5)            │ ((4 (0 1 4)) (6 (0 3 6)) (5 (0 1 2 5))) │
│      │                                             │                 │              │            │             │ unvisited: (5)   │                                         │
├──────┼─────────────────────────────────────────────┼─────────────────┼──────────────┼────────────┼─────────────┼──────────────────┼─────────────────────────────────────────┤
│  4   │ ((4 (0 1 4)) (6 (0 3 6)) (5 (0 1 2 5)))     │ (0 1 3 2 4 6 5) │ 4            │ (0 1 4)    │ Yes         │ n/a              │ n/a                                     │
└──────┴─────────────────────────────────────────────┴─────────────────┴──────────────┴────────────┴─────────────┴──────────────────┴─────────────────────────────────────────┘

Result: (0 1 4) → "A -> B -> E"
|#

;;; Tests
(check-equal? (bfs-shortest-path map-matrix 0 4) '(0 1 4)
              "Shortest path from A -> E should be A -> B -> E")
(check-equal? (bfs-shortest-path map-matrix 0 14) '(0 1 2 5 8 11 14)
              "Path from A -> O should traverse the grid")
(check-equal? (bfs-shortest-path map-matrix 5 5) '(5)
              "Path from a node to itself should be just that node")
(check-equal? (bfs-shortest-path '((0 1) (1 0) (0 0)) 0 2) #f
              "No path to disconnected node")

;;; Run it
(pretty-print (bfs-shortest-path map-matrix 0 4))
(pretty-print (bfs-shortest-path map-matrix 0 14))

;;; Modify the code in bfs-shortest-path above to create dfs-path.  Add your own design information
;;; in the comments (signature, purpose, template, tests) above your function.
;;;
;;; Questions to answer in addition:
;;; - Will dfs-path always give the same result as bfs-shortest-path? Why or why not?
;;; - How could you ensure dfs-path and bfs-shortest-path always give the same results?
;;; - What changes between a queue (FIFO) and a stack (LIFO) in these functions?

;;; dfs-path : List Number Number -> List
;;;
;;; Given an adjacency matrix, a starting node, and an ending node, use depth-first search to return
;;; a list of nodes that must be traversed.  Produces a discovered path as a list, or #f if there is
;;; no path. NOTE: DFS does NOT guarantee the shortest path.
;;;
;;; (define (dfs-path matrix start goal)
;;;   ;; loop:    named let for the dfs loop
;;;   ;; stack:   each item in the stack is (list node path-to-node)
;;;   ;;          Eg, ((1 (0 1)) (3 (0 3))) means we're exploring nodes 1 and 3
;;;   ;; visited: list of all nodes that we've seen
;;;   (let loop ([stack (list (list start (list start)))]
;;;              [visited (list start)])
;;;     ...))
;;;
;;; matrix : list of lists representing the adjacency matrix
;;; start  : a number representing the node index to start from
;;; goal   : a number representing the node index to end at
;;;
;;; Algorithm:
;;; - Start with a stack containing the start node and its path
;;; - Mark the start node as visited
;;; - While stack is not empty:
;;;    - Pop the first item
;;;    - If it's the goal, return the path
;;;    - Otherwise, get all unvisited neighbours
;;;    - Add neighbours to stack with updated paths
;;;    - Mark newly discovered neighbours as visited
;;; - If stack becomes empty, no path exists
;;;
;;; (check-equal? (dfs-path map-matrix 0 4) '(0 1 4)
;;;               "DFS finds a path from A -> E (may not be shortest)")
;;; (check-equal? (dfs-path map-matrix 5 5) '(5)
;;;               "Path from node to itself should be just that node")
;;; (check-equal? (dfs-path '((0 1) (1 0) (0 0)) 0 2) #f
;;;               "No path to disconnected node")
;;;
(define (dfs-path matrix start goal)
  ;; loop:    named let for the dfs loop
  ;; stack:   each item in the stack is (list node path-to-node)
  ;; visited: list of all nodes that we've seen
  (let loop ([stack (list (list start (list start)))]
             [visited (list start)])
    ;; Base case: if stack is empty, return #f because no path was found
    (if (empty? stack)
        #f
        ;; Otherwise, destructure: current item is (list node path), rest is remaining stack
        (let* ([current-item (car stack)]        ; Eg, '((0 (1 3)) (1 (0 2 4)))  -> '(0 (1 3))
               [current-node (car current-item)] ; Eg, '(0 (1 3))                -> 0
               [path (cadr current-item)]        ; Eg. '(0 (1 3))                -> '(1 3)
               [rest-stack (cdr stack)])         ; Eg, '((0 (1 3)) (1 (0 2 4)))  -> '(1 (0 2 4))
          (if (equal? current-node goal)
              ;; Success: return the path
              path
              ;; Continue exploring: add neighbours to stack
              (let* ([neighbours (get-neighbours matrix current-node)]
                     ;; Filter to only unvisited neighbours (avoid cycles)
                     [unvisited-neighbours
                       (filter-not (lambda (n) (member n visited)) neighbours)]
                     ;; Update visited list with new neighbours
                     [new-visited
                       (append visited unvisited-neighbours)]
                     ;; Create stack items for each unvisited neighbour; each item contains the
                     ;; neighbour and the path to reach it
                     [new-stack-items
                       (map (lambda (n) (list n (append path (list n)))) unvisited-neighbours)]
                     ;; Add new items to FRONT of stack (depth-first); this is the key difference
                     [new-stack
                       (append new-stack-items rest-stack)])
                ;; Recurse with updated stack and visited list
                (loop new-stack new-visited)))))))

;;; Tests
(check-equal? (dfs-path map-matrix 0 4) '(0 1 4)
              "DFS finds a path from A -> E")
(check-equal? (dfs-path map-matrix 5 5) '(5)
              "Path from node to itself should be just that node")
(check-equal? (dfs-path '((0 1) (1 0) (0 0)) 0 2) #f
              "No path to disconnected node")

;;; Run it
(pretty-print (dfs-path map-matrix 0 4))
(pretty-print (dfs-path map-matrix 0 14))

#|
Question 1: Will dfs-path always give the same result as bfs-shortest-path? Why or why not?

Answer: No. BFS explores level-by-level and guarantees the shortest path. DFS explores deeply down one
branch before backtracking, so it may find a longer path first and return it immediately.

Example from A (0) to N (13):
- BFS finds: (0 1 4 7 10 13)         - length 5 edges (A → B → E → H → K → N)
- DFS finds: (0 1 2 5 8 7 6 9 12 13) - length 9 edges (A → B → C → F → I → H → G → J → M → N)

They give the same result when:
- There's only one path to the goal
- The shortest path happens to be explored first by DFS
- Start equals goal


Question 2: How could you ensure dfs-path and bfs-shortest-path always give the same results?

Answer: There are a couple of ways to modify dfs-path:

1. Make dfs explore all paths and return the shortest (defeats the purpose of dfs)
2. Use iterative deepening: run dfs with increasing depth limits (0, 1, 2, ...) until goal is found;
this is essentially bfs using dfs mechanics


Question 3: What changes between a queue (FIFO) and a stack (LIFO) in these functions?

Answer: The key difference is where new items are added. The following change makes all the
difference:

BFS (Queue - FIFO):
  [new-queue (append rest-queue new-queue-items)]  ; Add to END
  Effect: Process oldest items first → explore level-by-level (breadth-first)
  
DFS (Stack - LIFO):
  [new-stack (append new-stack-items rest-stack)]  ; Add to FRONT  
  Effect: Process newest items first → explore deeply down one path (depth-first)

Visual:
  BFS queue: [B D] → [D C E] → [C E G] → ...  (process B, then D, then C...)
  DFS stack: [D B] → [G E D B] → [J H E D B] → ...  (process D, then G, then J...)
              ↑ process front (most recent)
|#

