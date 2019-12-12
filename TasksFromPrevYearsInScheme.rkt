#lang racket

;2016/Вариант А
;Зад.2 
(define (removeEl l n)
  (cond ((null? l) '())
        ((equal? (length (car l)) n) (removeEl (cdr l) n))
        (else (cons (car l) (removeEl (cdr l) n)))))

(define (countEx l n)
  (cond ((null? l) 0)
        ((equal? (length (car l)) n) (+ 1 (countEx (cdr l) n)))
        (else (countEx (cdr l) n))))

(define (histogram l)
  (if (null? l) '()
      (cons (list (length (car l)) (countEx l (length (car l))))
            (histogram (removeEl (cdr l) (length (car l)))))))

(define (findMaxElem l)
  (define (helper ll maxEl)
    (cond ((null? ll) maxEl)
          ((< (cadr maxEl) (cadar ll)) (helper (cdr ll) (car ll)))
          (else (helper (cdr ll) maxEl))))
  (helper l (car l)))

(define (remove l x)
  (cond ((null? l) '())
        ((equal? x (car l)) (cdr l))
        (else (cons (car l) (remove (cdr l) x)))))

(define (sort l)
  (define (helper ll resultList)
    (if (null? ll) resultList
        (helper (remove ll (findMaxElem ll))
                (cons (findMaxElem ll) resultList))))
  (helper l '()))

(define (findElem ll n)
  (if (null? ll) '()
      (if (equal? (length (car ll)) n)
          (car ll)
          (findElem (cdr ll) n))))

(define (change his)
  (if (null? his) '()
      (cons (list (caar his) (- (cadar his) 1)) (cdr his))))

(define (lfsortHelper ll)
  (define hist (sort (histogram ll)))
  (define (helper l his)
    (cond ((or (null? l) (null? his)) '())
          ((= (cadar his) 1) (cons (findElem l (caar his))
                                   (helper (remove l (findElem l (caar his))) (cdr his))))
          (else (cons (findElem l (caar his))
                      (helper (remove l (findElem l (caar his))) (change his)))))) 
  (helper ll hist))

(define (lfsort ll)
  (if (null? ll) '()
      (lfsortHelper ll)))



;Зад.4

(define (make-tree root left right)
  (list root left right))

(define (root-tree tree)
  (if (null? tree) '()
      (car tree)))

(define (left-tree tree)
  (if (null? tree) '()
  (cadr tree)))

(define (right-tree tree)
  (if (null? tree) '()
  (caddr tree)))

(define (isLeaf? tree)
  (and (not (null? tree))
        (null? (left-tree tree))
        (null? (right-tree tree))))

(define (prune tree)
  (cond ((null? tree) '())
        ((isLeaf? tree) '())
        (else (make-tree (root-tree tree)
                         (prune (left-tree tree))
                         (prune (right-tree tree))))))


;2016 Вариант Б
;зад.4

(define (bloom tree)
  (cond ((null? tree) '())
        ((isLeaf? tree) (make-tree (root-tree tree)
                                   (make-tree (root-tree tree) '() '())
                                   (make-tree (root-tree tree) '() '())))
        (else (make-tree (root-tree tree)
                         (bloom (left-tree tree))
                         (bloom (right-tree tree))))))



;2017 Вариант А
;зад.3


(define (vertices g)
  (if (null? g) '()
      (map car g)))

(define (children v g)
  (if (null? g) '()
      (cddr (assv v g))))

(define (countChildrens childrens)
  (if (null? childrens) 0
      (+ 1 (countChildrens (cdr childrens)))))

(define (outPower g)
  (if (null? g) '()
      (map (lambda (x) (countChildrens (children x g))) (vertices g))))

(define (inPower g)
  (if (null? g) '()
      (map (lambda (x)
             (length (filter (lambda (z) (member x z))
                               (map (lambda (y) (children y g)) (vertices g)))))
           (vertices g))))

(define (powers g)
  (define out (outPower g))
  (define in (inPower g))
  (map (lambda (x y) (+ x y)) out in))

(define (euleryCycle g)
  (define pws (powers g))
  (define evens (filter even? pws))
  (= (length pws) (length evens))) 
  
(define (eulerCycleCost g)
  (define pws (inPower g))
  (if (not (euleryCycle g)) 0
      (foldr (lambda (x y) (+ x y)) 0 (map (lambda (x y) (* x (cadr y))) pws g))))



;Зад.4

(define (sumSubtree root)
  (cond ((null? root) 0)
        ((isLeaf? root) (car root))
        (else (+ (car root) (sumSubtree (left-tree root)) (sumSubtree (right-tree root))))))


(define (transformSum tree)
  (if (null? tree) '()
      (make-tree (sumSubtree tree)
                 (transformSum (left-tree tree))
                 (transformSum (right-tree tree)))))


;2018/Вариант А
;Зад. 2
(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define (grow t x)
  (cond ((null? t) '())
        ((isLeaf? t) (make-tree (root-tree t)
                                (make-tree x '() '())
                                (make-tree x '() '())))
        (else (make-tree (root-tree t)
                         (grow (left-tree t) x)
                         (grow (right-tree t) x)))))

(define (from n) (cons-stream n (from (+ n 1))))

(define nums (from 1))

(define (growingTreesHelper lastTree numbers)
  (cons-stream lastTree (growingTreesHelper (grow lastTree (head (tail numbers))) (tail numbers))))

(define (growingTrees) (growingTreesHelper (make-tree (head nums) '() '()) nums))
  


;Вариант Б
;Зад. 2

(define (newTree t y)
  (if (null? t) '()
      (make-tree (+ y (root-tree t))
                 (newTree (left-tree t) y)
                 (newTree (right-tree t) y))))

(define (clone t x y)
  (if (null? t) '()
      (make-tree x (newTree t y) (newTree t y))))


(define (cloninTrees)
  (define lastTree (make-tree 1 '() '()))
  (define (helper lastT)
    (cons-stream lastT (helper (clone lastT 1 1))))
  (helper lastTree))






















