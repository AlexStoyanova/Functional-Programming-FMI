(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (lenght l)
  (if (null? l) 0
      (+ 1 (lenght (cdr l)))))

(define (myAppend l1 l2)
  (if (null? l1) l2
      (cons (car l1) (myAppend (cdr l1) l2))))

(define (snoc x l) (myAppend l (list x)))
  
;Task 1

(define (rows m)
  (if (null? m)
      0
      (+ 1 (rows (cdr m)))))

(define (cols m)
  (if (null? m)
      0
      (+ 1 (cols (cdr m)))))
        
(define (dimensions m)
  (cons (rows m) (cols (car m))))

;Task 2

(define (myReverse l)
  (define (rev l res)
    (if (null? l) res
        (rev (cdr l) (cons (car l) res))))
  (rev l '()))

(define (reverse-columns m)
  (if (null? m) '()
      (cons (myReverse (car m)) (reverse-columns (cdr m)))))

;Task 3

(define (nth-el l n)
  (cond ((> n (lenght l)) '())
        ((= n 1) (car l))
        ((null? l) '())
        (else (nth-el (cdr l) (- n 1)))))

(define (nth-column m n)
  (if (null? m) '()
      (cons (nth-el (car m) n) (nth-column (cdr m) n))))

;Task 4

(define (main-diagonal m)
  (define (helper m index)
    (if(null? m) '()
       (cons (nth-el (car m) index) (helper (cdr m) (+ 1 index)))))
  (helper m 1))

;Task 5

(define (transpose m)
  (define (helper m col)
  (if (= col 0) '()
      (snoc (nth-column m col) (helper m (- col 1)))))
 (helper m (cols (car m))))

;Task 6

(define (myEven? l)
  (if (null? l) #t
      (if (= (remainder (car l) 2) 0)
          (myEven? (cdr l))
          #f)))

(define (for-all-columns? m p)
  (define newMatrix (transpose m))
  (define (helper newMatrix p)
    (if (null? newMatrix) #t
       (if (p (car newMatrix))
           (helper (cdr newMatrix) p)
           #f)))
  (helper newMatrix p))

;Task 7

(define (primeNum? n)
  (define (helper n counter end)
    (cond ((<= n 1) #f)
          ((= counter end) #t)
          (else (if (= (remainder n counter) 0) #f
            (helper n (+ 1 counter) end)))))
  (helper n 2 n))       

(define (hasPrimeNumInList? l)
  (if (null? l) #f
      (if (primeNum? (car l)) #t
          (hasPrimeNumInList? (cdr l)))))

(define (prime-in-each-column? m)
  (for-all-columns? m hasPrimeNumInList?))


;Task 8

(define (multiplyTwoLists l1 l2)
  (if (null? l1) 0
      (+ (* (car l1) (car l2)) (multiplyTwoLists (cdr l1) (cdr l2)))))

(define (multiplyListAndMatix l m)
  (if (null? m) '()
      (cons (multiplyTwoLists l (car m)) (multiplyListAndMatix l (cdr m)))))

(define (multiply a b)
  (define newMatrix (transpose b))
  (define (helper a newMatrix)
    (if (null? a) '()
    (cons (multiplyListAndMatix (car a) newMatrix) (helper (cdr a) newMatrix))))
  (helper a newMatrix))
  
;Task 9

(define (firstInSecond? l1 l2)
  (define (myMember x l)
  (if (null? l) #f
      (if (equal? (car l) x) #t
          (myMember x (cdr l)))))
  (if (null? l1) #t
      (if (myMember (car l1) l2)
          (firstInSecond? (cdr l1) l2)
          #f)))

(define (numOfFindings? l m)
  (if (null? m) #f
      (if (firstInSecond? l (car m))
          #t
          (numOfFindings? l (cdr m)))))
          

(define (find-columns m)
  (define newMatrix (transpose m))
  (define (helper m newMatrix)
    (if (null? newMatrix) 0
        (if (numOfFindings? (car newMatrix) m)
            (+ (helper m (cdr newMatrix)) 1)
            (helper m (cdr newMatrix)))))
  (helper m newMatrix))





 
                  
                  