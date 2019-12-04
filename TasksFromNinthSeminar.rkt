#lang racket

(define g '((1 2) (2 3) (3 1 4) (4 3 5) (5 5)))

;task 1

(define (vertices graph)
  (map car graph))

(define (children v graph)
  (cdr (assv v graph)))

(define (edge? u v graph)
  (memv v (children u graph)))

(define (map-children v f graph)
  (map f (children v graph)))

(define (search-child v p graph)
  (filter p (children v graph)))

;task2










