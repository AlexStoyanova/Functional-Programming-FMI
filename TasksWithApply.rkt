#lang racket
;Task 1

(define (foldr1 op l)
  (if (null? (cdr l))
      (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (maximum x . l)
  (if (null? l) x
      (foldr1 max (cons x l))))


;Task 2

(define (concat . l)
  (apply append l))

;Task 3

(define (removeEl l1 end)
  (define (helper l1 count)
    (if (= count end)
        '()
        (cons (car l1) (helper (cdr l1) (+ count 1)))))
  (helper l1 0))

(define (zip-with f l1 l2)
  (define len1 (length l1))
  (define len2 (length l2))
  (cond ((< len1 len2) (map f l1 (removeEl l2 len1)))
        ((> len1 len2) (map f (removeEl l1 len2) l2))
        (else (map f l1 l2))))


(define (zip-with1 f l1 l2)
  (define len1 (length l1))
  (define len2 (length l2))
  (cond ((null? l1) '())
        ((< len1 len2) (zip-with1 f l1 (removeEl l2 len1)))
        ((> len1 len2) (zip-with1 f (removeEl l1 len2) l2))
        (else (cons (apply (lambda (x y) (f (car x) (car y))) (list l1 l2))
                    (zip-with1 f (cdr l1) (cdr l2))))))      

;Task 4, 5 

(define (zip-with-more f . l)
  (cond ((null? l) '())
        ((null? (cdr l)) (car l)) 
        (else (apply
               zip-with-more
               f
               (apply zip-with1 f (list (car l) (car (cdr l))))
               (cdr (cdr l))))))


;Task 6

(define (transpose m)
  (apply map list m))






  
  