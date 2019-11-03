;Task 1

(define (countSeqOfNum x l)
  (cond ((null? l) 0)
        ((not (equal? x (car l))) 0)
        (else (+ 1 (countSeqOfNum x (cdr l))))))

(define (removeFirstEl l count)
  (cond ((null? l) '())
        ((= count 1) (cdr l))
        (else (removeFirstEl (cdr l) (- count 1)))))

(define (run-length-encode l)
  (if (null? l) '()
      (cons (cons (car l) (countSeqOfNum (car l) l))
            (run-length-encode (removeFirstEl l (countSeqOfNum (car l) l))))))

;Task 2

(define (run-length-decode code)
  (define (helper l counter)
  (if (= counter 0) '()
      (cons (car l) (helper l (- counter 1)))))
 (if (null? code) '()
     (append (helper (car code) (cdar code)) (run-length-decode (cdr code)))))

;Task 3

(define (helper l num)
  (if (null? l) 0
      (if (equal? num (car l))
          (+ 1 (helper (cdr l) num))
          (helper (cdr l) num))))

  
(define (remove l x)
  (if (null? l) '()
      (if (not (equal? (car l) x))
          (cons (car l) (remove (cdr l) x))
          (remove (cdr l) x))))

(define (histogram l)
  (if (null? l) '()
      (cons (cons (car l) (helper l (car l)))
            (histogram (remove l (car l))))))
  

;Task 4

(define (snoc l x) (append l (list x)))

(define (hasElement el l)
  (if (null? l) #f
      (if (equal? el (car (car l))) #t
          (hasElement el (cdr l)))))

(define (addEl el l)
  (if (null? l) '()
      (if (equal? el (car (car l)))
          (snoc (car l) el)
          (addEl el (cdr l)))))

(define (findAndAdd fEl el l res)
  (if (null? l) res
      (if (not (equal? fEl (car (car l))))
          (findAndAdd fEl el (cdr l) (cons (car l) res))
          (findAndAdd fEl el (cdr l) (cons (snoc (car l) el) res)))))

(define (makeListOfListAndEl x l)
  (if (null? l) (list (list x))
      (cons (list x) l)))

(define (makeKeys f l)
  (define (makeKeys-help f l res)
    (if (null? l) res
      (if (hasElement (f (car l)) res)
          (makeKeys-help f (cdr l) res)
          (makeKeys-help f (cdr l) (makeListOfListAndEl (f (car l)) res)))))
  (makeKeys-help f l '()))

(define (group-by f l)
  (define myList (makeKeys f l))
  (define (helpME f l myList)
    (if (null? l)
         myList
        (helpME f (cdr l) (findAndAdd (f (car l)) (car l) myList '()))))
  (helpME f l myList))

      



