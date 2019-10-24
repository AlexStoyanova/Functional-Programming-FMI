(define (add x y) (+ x y))

(define (even x) (= 0 (remainder x 2)))

(define (odd x) (not(even x)))

(define (signum x) 
    (cond ((< x 0) -1) 
          ((> x 0) 1) 
          (else 0)))

(define (factorial x) 
     (if(= x 1) 1 (* (factorial(- x 1)) x)))

(define (sum start end) 
        (cond ((> start end) 0)
              ((= start end) start) 
              (else (+ start (sum (+ start 1) end)))))

(define (exptt x n) 
        (cond ((< n 0) (/ 1 (exptt x (* -1 n))))
              ((= n 0) 1)
              (else (* x (exptt x (- n 1))))))

			  (define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (fact n)
  (accumulate-i * 1 1 n (lambda (x) x) (lambda (x) (+ x 1))))


(define (myExpt x n)
  (accumulate-i * 1 1 n (lambda (i) x) (lambda (i) (+ i 1))))

(define (myExp x n)
  (accumulate + 0. 0 n (lambda (i) (/ (myExpt x i) (fact i)))(lambda (i) (+ i 1))))

(define (func x) (lambda (sum y) (sum y x)))

(define (square x) (* x x))

(define (fast-expt x n)
  (define (fast-expt-h x n res)
    (cond ((= n 1) (* x res))
          ((even? n) (fast-expt-h x (quotient n 2) (square res)))
          (else (fast-expt-h x (- n 1) (* res x)))))
  (fast-expt-h x n 1))