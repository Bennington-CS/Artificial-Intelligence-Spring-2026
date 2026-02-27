#lang racket
(require rackunit)

;;;;;
;;;;; Chapter 2: Adjacency Matrices and Search
;;;;; Worksheet
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
;;; - Should be a list of lists, where each sublist is a row of the matrix
;;; - Hint: You'll need 15 nodes (A through O) for the visual map above
#;
(define map-matrix
  ;; A B C D E F G H I J K L M N O
  '((0 1 0 1 0 0 0 0 0 0 0 0 0 0 0)   ; A: connects to B, D
    ;; Add 14 more rows here for B through O
    ...
    ))

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
;;;   ... for/list ...)
;;;
;;; matrix  : a list of lists representing an adjacency matrix
;;; node    : a number representing the node index to find neighbours for
;;;
;;; (check-equal? (get-neighbours map-matrix 3)  '(0 4 6)
;;;               "D should connect to A, E, G")
;;; (check-equal? (get-neighbours map-matrix 14) '(11 13)
;;;               "O should connect to L, N")
;;;
#;
(define (get-neighbours matrix node)
  ...)

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
;;;               "Path from A -> E should be via B")
;;; (check-equal? (pretty-print #f) ""
;;;               "No path should produce empty string")
;;;
#;
(define (pretty-print result)
  ...)

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
;;;   ;; loop:    named let for the bfs loop
;;;   ;; queue:   each item in the queue is (list node path-to-node)
;;;   ;;          Eg, ((1 (0 1)) (3 (0 3))) means we're exploring nodes 1 and 3
;;;   ;; visited: list of all nodes that we've seen
;;;   (let loop ([queue (list (list start (list start)))]
;;;              [visited (list start)])
;;;     ...))
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
;;;               "Path from node to itself should be just that node")
;;; (check-equal? (bfs-shortest-path '((0 1) (1 0) (0 0)) 0 2) #f
;;;               "Should be no path to disconnected node")
;;;
#;
(define (bfs-shortest-path matrix start goal)
  ...)
 
;;; Run it and pretty-print the results for A->E and A->O
#;
(pretty-print (bfs-shortest-path map-matrix 0 4))
#;
(pretty-print (bfs-shortest-path map-matrix 0 14))

;;; Modify the code in bfs-shortest-path above to create dfs-path.  Add your own design information
;;; in the comments (signature, purpose, template, tests) above your function.
;;;
;;; Questions to answer in addition:
;;; - Will dfs-path always give the same result as bfs-shortest-path? Why or why not?
;;; - How could you ensure dfs-path and bfs-shortest-path always give the same results?
;;; - What changes between a queue (FIFO) and a stack (LIFO) in these functions?
#;
(define (dfs-path matrix start goal)
  ...)

#|
Question 1: Will dfs-path always give the same result as bfs-shortest-path? Why or why not?

Answer:

Question 2: How could you ensure dfs-path and bfs-shortest-path always give the same results?

Answer:

Question 3: What changes between a queue (FIFO) and a stack (LIFO) in these functions?

Answer:

|#

