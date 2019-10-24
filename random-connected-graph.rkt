#lang racket

(require graph)
(require math)

;; Return true with the input probability and false otherwise
(define (random-event probability)
  (if (< (random) probability)
      #true
      #false))

;; Return a connected renyi random graph with 'size' vertices
;; and a probability 'density' of having an edge between any
;; given pair of vertices.
;; The graphs are slightly biased: to enforce connectedness,
;; the renyi graph is superimposed to a simple path with 'size'
;; vertices.
(define (connected-renyi-erdos size probability)
  (append (trail size)
          (filter (lambda (x) (random-event probability))
                  (combinations
                   (stream->list (in-range 1 (+ size 1)))
                   2))))

(define (trail n)
  (if (equal? n 2)
      (list (list 1 2))
      (append (list (- n 1) n) (trail (- n 1)))))

;; Return a random connected graph with 'size' vertices and at most 'size' edges
;; as a list of vertices
(define (get-random-graph size density)
  (connected-random-graph size size density))

(define (connected-random-graph size n density)
  (if (equal? n 1)
      (if (random-event density)
          (list (list (random-integer 1 (+ size 1)) (random-integer 1 (+ size 1))))
          '())
      (append '() (list (list n (- n 1))) (connected-random-graph size 1 density) (connected-random-graph size (- n 1) density))))

;; ToDo: 
;; Return a random connected graph with 'n-vertices' vertices and 'n-edges' edges
;; distributed randomly between vertices. edges must be larger than 'n-vertices' minus one,
;; since the graph must be connected

;; ToDo:
;; Add generators for graphs with sensible power-laws (for diameter, for instance)


;; Usage sample

(display (graphviz (undirected-graph (connected-renyi-erdos 25 0.5))))