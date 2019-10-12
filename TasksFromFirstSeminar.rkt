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
