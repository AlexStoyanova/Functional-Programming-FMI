#lang racket

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define the-empty-stream '())

(define head car)

(define (tail s) (force (cdr s)))

(define empty-stream? null?)


;task1

(define (stream-range a b)
  (if (> a b) the-empty-stream
      (cons-stream a (stream-range (+ a 1) b))))

(define someStream (stream-range 1 6))

;task2

(define (stream-ref s n)
  (cond ((empty-stream? s) the-empty-stream)
        ((= n 0) (head s))
        (else (stream-ref (tail s) (- n 1)))))

;task3

(define (stream-map s f)
  (if (empty-stream? s) the-empty-stream
      (cons-stream (f (head s)) (stream-map (tail s) f))))

;task4

(define (stream-filter s p)
  (cond ((empty-stream? s) the-empty-stream)
        ((p (head s)) (cons-stream (head s) (stream-filter (tail s) p)))
        (else (stream-filter (tail s) p))))


;task5

(define (stream-fold s acc f)
  (if (empty-stream? s) acc
      (stream-fold (tail s) (f acc (head s)) f)))


;task6

(define (stream->list s)
  (if (empty-stream? s)
      '()
      (cons (head s) (stream->list (tail s)))))


;task7

(define (stream-take s n)
  (cond ((empty-stream? s) the-empty-stream)
        ((= n 0) the-empty-stream)
        (else (cons-stream (head s) (stream-take (tail s) (- n 1))))))


;task8

(define (stream-drop s n)
  (cond ((empty-stream? s) the-empty-stream)
        ((> n 0) (stream-drop (tail s) (- n 1)))
        (else (cons-stream (head s) (stream-drop (tail s) n)))))

;task9

(define (from n) (cons-stream n (from (+ 1 n))))

(define nat (from 0))

;task10

(define (prime? n)
  (define (pr count)
    (cond ((< n 2) #f)
          ((or (= n 2)(= count n)) #t)
          ((= (remainder n count) 0) #f)
          (else (pr (+ count 1)))))
  (pr 2))

(define (search-el s p)
  (cond ((empty-stream? s) #f)
        ((p (head s)) s)
        (else (search-el (tail s) p))))

(define (prime42)
  (define (helper n s)
    (if (= n 0) (head s)
        (helper (- n 1) (search-el (tail s) prime?))))
  (helper 42 nat))

;task11

(define (fib)
  (define (fibonacci a b)
    (cons-stream a (fibonacci b (+ a b))))
 (fibonacci 1 1))

;task12

(define (stream-append s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (head s1) (stream-append (tail s1) s2)))))


;task13

(define (stream-join s)
  (if (empty-stream? s)
      the-empty-stream
      (stream-append (head s) (stream-join (tail s)))))

;for testing:
(define st1 (stream-range 0 6))
(define st2 (stream-range 7 10))
(define st3 (stream-range 11 15))

(define s4 (cons-stream st1 (cons-stream st2 (cons-stream st3 '()))))

(define allSt (stream-join s4))

;task14

(define (pythagorean-triples)
  (void))

