;Task 1

(define (isAlreadyIn? x l)
  (cond ((null? l) #f)
        ((equal? (car l) x) #t)
        (else (isAlreadyIn? x (cdr l)))))
      
(define (snoc x l) (append l (list x)))      

(define (union a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else (if (not (isAlreadyIn? (car a) b))
                  (union (cdr a) (snoc (car a) b))
                  (union (cdr a) b)))))

(define (interaction a b)
  (cond ((null? a) '())
        ((null? b) a)
        (else (if (not (isAlreadyIn? (car a) b))
                  (cons (car a) (interaction (cdr a) b))
                  (interaction (cdr a) b)))))

               
(define (substraction a b)
  (cond ((null? a) '())
        ((null? b) '())
        (else (if (isAlreadyIn? (car a) b)
                  (cons (car a) (substraction (cdr a) b))
                  (substraction (cdr a) b)))))
                       

;Task 2

(define (fOfElementInList f l)
  (define (helper f l iterL)
    (if (null? iterL)
        '()
        (if (isAlreadyIn? (f (car iterL)) l)
            (cons (car iterL) (helper f l (cdr iterL)))
            (helper f l (cdr iterL)))))
  (helper f l l))
          

;Task 3

(define (countDig num)
  (if (= (remainder num 10) num)
      1
      (+ 1 (countDig (quotient num 10)))))


(define (middle-digit n)
  (define (middle num pos)
    (if (= (countDig num) pos)
        (remainder num 10)
        (middle (quotient num 10) pos)))
  (if (even? (countDig n)) -1
      (middle n (+ 1 (quotient (countDig n) 2)))))
  
;Task 4

(define (countElem l)
  (if (null? l) 0
      (+ 1 (countElem (cdr l)))))

(define (cols m)
  (countElem (car m)))

(define (rols m)
  (countElem m))

(define (nth-el l n)
  (cond ((null? l) '())
        ((= n 1) (car l))
        (else (nth-el (cdr l) (- n 1)))))

(define (nth-col m n)
  (if (null? m) '()
      (cons (nth-el (car m) n) (nth-col (cdr m) n))))

(define (transpose m)
  (define (helper m n)
    (if (= n 0) '()
        (snoc (nth-col m n) (helper m (- n 1)))))
  (helper m (cols m)))
  
(define (sum x l)
  (if (null? l) (- x)
      (+ (car l) (sum x (cdr l)))))

(define (sumEl? l)
  (define (helper iterL)
    (cond ((null? iterL) #f)
          ((equal? (car iterL) (sum (car iterL) l)) #t)
          (else (helper (cdr iterL)))))
  (helper l))

(define (count-cols m)
  (define transMatrix (transpose m))
  (define (helper trans res)
    (cond ((null? trans) res)
          ((sumEl? (car trans))
           (helper (cdr trans) (+ 1 res)))
          (else (helper (cdr trans) res))))
  (helper transMatrix 0))
        

;Task 6

(define (isMember x l)
  (cond ((null? l) #f)
        ((equal? (car l) x) #t)
        (else (isMember x (cdr l)))))
      
(define (isInListAfterF? f l)
  (define (helper l1)
    (cond ((null? l1) #t)
          ((not (isMember (f (car l1)) l)) #f)
          (else (helper (cdr l1)))))
  (helper l))

(define (checkOperationWithElem el op f l)
  (cond ((null? l) #t)
        ((not (= (op (f el) (f (car l))) (f (op el (car l))))) #f)
        (else (checkOperationWithElem el op f (cdr l)))))

(define (saveOperation op f l)
  (define (helper l1)
    (cond ((null? l1) #t)
          ((not (checkOperationWithElem (car l1) op f l)) #f)
          (else (helper (cdr l1)))))
  (helper l))

(define (is-em? l op f)
  (and (isInListAfterF? f l) (saveOperation op f l)))


;Task 7

(define (isMember x l)
  (cond ((null? l) #f)
        ((equal? (car l) x) #t)
        (else (isMember x (cdr l)))))
      
(define (isInListAfterF? f l1 l2)
   (cond ((null? l1) #t)
         ((not (isMember (f (car l1)) l2)) #f)
         (else (isInListAfterF? f (cdr l1) l2))))

(define (helper x f l1)
  (cond ((null? l1) #f)
        ((equal? (f (car l1)) x) #t)
        (else (helper x f (cdr l1)))))

(define (isInFirstList? f l1 l2)
  (cond ((null? l2) #t)
        ((not (helper (car l2) f l1)) #f)
        (else (isInFirstList? f l1 (cdr l2)))))

(define (is-sur? l1 l2 f)
  (and (isInListAfterF? f l1 l2) (isInFirstList? f l1 l2)))

;Task 11

(define (reverseDigits num)
  (define (rev-h num res)
    (if (= (remainder num 10) num)
        (+ (remainder num 10) (* 10 res))
        (rev-h (quotient num 10) (+ (* res 10) (remainder num 10)))))
  (rev-h num 0))

(define (diff-reverse num)
  (- num (reverseDigits num)))


;Task 12

(define (makeListOfDigits num)
  (if (= (remainder num 10) num)
      (list num)
      (cons (remainder num 10) (makeListOfDigits (quotient num 10)))))


(define (maxEl l)
  (define (helper l1 max)
    (if (null? l1) max
        (if (> (car l1) max)
            (helper (cdr l1) (car l1))
            (helper (cdr l1) max))))
  (helper l (car l)))

(define (remove x l)
  (if (null? l) '()
      (if (not (equal? x (car l)))
          (cons (car l) (remove x (cdr l)))
          (cdr l))))

(define (sortList l)
  (define (helper l res)
    (if (null? l) res
        (helper (remove (maxEl l) l) (cons (maxEl l) res))))
  (helper l '()))


(define (makeNumFromList l)
  (if (null? l) 0
      (+ (car l) (* 10 (makeNumFromList (cdr l))))))

(define (sort-digits n)
  (makeNumFromList (sortList (makeListOfDigits n))))


;Task 13

(define (firstFuncthenSecFunc x f g)
  (define (helper x f g iter)
    (if (= iter 0) x
        (f (g (helper x f g (- iter 1))))))
  (helper x f g (quotient x 2)))


(define (permutable? a b f g)
  (if (> a b) #t
      (if (even? a)
          (and (= (firstFuncthenSecFunc a f g)
                  (firstFuncthenSecFunc a g f))
               (permutable? (+ 2 a) b f g))
          (permutable? (+ 1 a) b f g))))
          

;Task 14

(define (maxSubstract p)
  (- (cdr p) (car p)))

(define (biggestInterval l)
  (define (helper l max p)
    (if (null? l) p
        (if (< max (maxSubstract (car l)))
            (helper (cdr l) (maxSubstract (car l)) (car l))
            (helper (cdr l) max p))))
  (helper l (maxSubstract (car l)) (car l)))

(define (subsetOfBiggestInterval interval l)
  (if (null? l) (list interval)
      (if (and (> (caar l) (car interval))
               (< (cdar l) (cdr interval)))
          (cons (car l) (subsetOfBiggestInterval interval (cdr l)))
          (subsetOfBiggestInterval interval (cdr l)))))

(define (longest-interval-subsets il)
  (if (null? il) '()
      (subsetOfBiggestInterval (biggestInterval il) il)))

(define (minEl l)
  (define (helper l min res)
    (if (null? l) res
        (if (> min (caar l))
            (helper (cdr l) (caar l) (car l))
            (helper (cdr l) min res))))
  (helper l (caar l) (car l)))

(define (remove p l)
  (if (null? l) '()
      (if (and (= (caar l) (car p)) (= (cdar l) (cdr p)))
          (cdr l)
          (cons (car l) (remove p (cdr l))))))

(define (sortPairs l)
  (if (null? l) '()
      (cons (minEl l) (sortPairs (remove (minEl l) l)))))

(define (longest-interval-subsets il)
  (if (null? il) '()
      (sortPairs (subsetOfBiggestInterval (biggestInterval il) il))))


;Task 15

(define (countDigits n)
  (if (= n 0) 0
      (+ 1 (countDigits (quotient n 10)))))

(define (sumDigExp n digits)
  (if (= (remainder n 10) n)
      (expt n digits)
      (+ (expt (remainder n 10) digits)
         (sumDigExp (quotient n 10) digits))))

(define (narcissistic? n)
  (define numberOfDig (countDigits n))
  (= n (sumDigExp n numberOfDig)))


;Task 16

(define (d x)
  (define (helper x sum iter)
    (if (= iter x) sum
        (if (= (remainder x iter) 0)
            (helper x (+ sum iter) (+ iter 1))
            (helper x sum (+ iter 1)))))
  (helper x 0 1))

(define (friendly? a b)
  (and (= (d a) b) (= (d b) a)))



;Task 17

(define (fixStart f start end)
  (if (= start (- end 1))
      (f start end)
      (f start (fixStart f start (- end 1)))))

(define (iteration f a b max i j)
  (cond ((> i b) max)
        ((> j b) (iteration f a b max (+ 1 i) (+ 2 i)))
        (else (if (< max (fixStart f i j))
              (iteration f a b (fixStart f i j) i (+ j 1))
              (iteration f a b max i (+ j 1))))))
              
(define (find-max f a b)
  (iteration f a b (f a (+ 1 a)) a (+ a 1)))


      

               