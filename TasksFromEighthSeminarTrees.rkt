;Task 1

(define (make-tree root left right)
  (cons root (list left right)))

(define (root-tree tree)
  (if (null? tree) '()
      (car tree)))

(define (left-tree tree)
  (if (null? tree)
      '()
      (cadr tree)))

(define (right-tree tree)
  (if (null? tree)
      '()
      (caddr tree)))

(define (empty-tree? tree)
  (null? tree))

(define (leaf-tree? tree)
  (if (null? tree) #f
      (and (null? (left-tree tree))
               (null? (right-tree tree)))))


(define (tree? tree)
  (cond ((null? tree) #t)
        (else (and (<= (length (cdr tree)) 2)
                   (tree? (left-tree tree))
                   (tree? (right-tree tree) )))))
      

;Task 2

(define (pre-order tree)
  (cond ((null? tree) '())
        ((leaf-tree? tree) (list (car tree)))
        (else (append (list (root-tree tree))
                      (pre-order (left-tree tree))
                      (pre-order (right-tree tree))))))

(define (in-order tree)
  (cond ((null? tree) '())
        ((leaf-tree? tree) (list (car tree)))
        (else (append (in-order (left-tree tree))
                      (list (root-tree tree))
                      (in-order (right-tree tree))))))
      
(define (post-order tree)
  (cond ((null? tree) '())
        ((leaf-tree? tree) (list (car tree)))
        (else (append (post-order (left-tree tree))
                      (post-order (right-tree tree))
                      (list (root-tree tree))))))      


;Task 3

(define (level n tree)
  (cond ((null? tree) '())
        ((= n 0) (list (root-tree tree)))
        (else (append (level (- n 1) (left-tree tree))
                      (level (- n 1) (right-tree tree))))))


;Task 4

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((leaf-tree? tree) 1)
        (else (+ (count-leaves (left-tree tree))
                 (count-leaves (right-tree tree))))))


;Task 5

(define (map-tree f tree)
  (cond ((null? tree) '())
        ((leaf-tree? tree) (list (f (root-tree tree)) '() '()))
        (else (cons (f (root-tree tree))
                    (cons (map-tree f (left-tree tree))
                    (list (map-tree f (right-tree tree))))))))

;Task 6

(define (height tree)
  (define (height-help tree curr)
    (cond ((null? tree) curr)
          ((leaf-tree? tree) (+ 1 curr))  
          (else (max (height-help (left-tree tree) (+ 1 curr))
                (height-help (right-tree tree) (+ 1 curr))))))
  (height-help tree 0))


;Task 7

(define (sum-tree tree)
  (cond ((null? tree) 0)
        ((leaf-tree? tree) (root-tree tree))
        (else (+ (root-tree tree)
                 (sum-tree (left-tree tree))
                 (sum-tree (right-tree tree))))))

;Task 8

(define (max-tree tree)
  (cond ((null? tree) 0)
        ((leaf-tree? tree) (root-tree tree))
        (else (max (root-tree tree)
                   (max-tree (left-tree tree))
                   (max-tree (right-tree tree))))))


;Task 9

(define (invert tree)
  (if (null? tree) '()
      (cons (root-tree tree)
            (list (invert (right-tree tree))
                  (invert (left-tree tree))))))

;Task 10

(define (binary-heap? tree)
  (if (null? tree) #t
      (and (if (not (null? (root-tree (left-tree tree))))
               (>= (root-tree tree) (root-tree (left-tree tree)))
               #t)
           (if (not (null? (root-tree (right-tree tree))))
               (>= (root-tree tree) (root-tree (right-tree tree)))
               #t)
           (binary-heap? (left-tree tree))
           (binary-heap? (right-tree tree)))))

;Task 11

(define (myAbs x) (if (< x 0) (- x) x))

(define (balanced? tree)
  (cond ((null? tree) #t)
        ((> (myAbs (- (height (left-tree tree))
                      (height (right-tree tree)))) 1) #f)
        (else (and (balanced? (left-tree tree))
                   (balanced? (right-tree tree))))))


;Task 12

(define (binary-search-tree? tree)
  (if (null? tree) #t
      (and (if (not (null? (root-tree (left-tree tree))))
               (> (root-tree tree) (root-tree (left-tree tree)))
               #t)
           (if (not (null? (root-tree (right-tree tree))))
               (<= (root-tree tree) (root-tree (right-tree tree)))
               #t)
           (binary-search-tree? (left-tree tree))
           (binary-search-tree? (right-tree tree)))))

  
;Task 13

(define (binary-search-tree-insert tree v)
  (if (null? tree)
      (list v '() '())
      (if (< v (root-tree tree))
          (make-tree (root-tree tree)
                     (binary-search-tree-insert (left-tree tree) v)
                     (right-tree tree))
          (make-tree (root-tree tree)
                     (left-tree tree)
                     (binary-search-tree-insert (right-tree tree) v)))))


;Task 14

(define (tree-sort l)
  (define (helper l tree)
    (if (null? l)
        (in-order tree)
        (helper (cdr l) (binary-search-tree-insert tree (car l)))))
  (helper l '()))


;Task 15

(define (nth-el l n)
  (cond ((> n (length l)) '())
        ((= n 1) (car l))
        ((null? l) '())
        (else (nth-el (cdr l) (- n 1)))))


(define (to-balanced-tree l)
  (define (helper l start end tree)
    (cond ((= start end) (binary-search-tree-insert tree (nth-el l start)))
          ((< start end) (make-tree (nth-el l (quotient (+ start end) 2))
                                    (helper l start (- (quotient (+ start end) 2) 1) tree)
                                    (helper l (+ (quotient (+ start end) 2) 1) end tree)))
          (else tree)))
  (helper l 1 (length l) '()))

















