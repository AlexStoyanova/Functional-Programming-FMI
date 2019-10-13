(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (fib-iter n)
  (define (for-fib-iter first second counter)
    (if (= counter n)
        second
        (for-fib-iter second (+ first second) (+ counter 1))))
  (if (= n 0)
      0
      (for-fib-iter 0 1 0)))

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-iter n)
  (define (f-for first second third counter)
    (if (= counter n)
        third
        (f-for second third (+ (* 3 first) (* 2 second) third) (+ 1 counter))))
  (f-for 0 1 2 2))


(define (binomial-coefficient row index)
  (cond ((= index 1) 1)
        ((= index row) 1)
        (else (+ (binomial-coefficient (- row 1) (- index 1))
                 (binomial-coefficient (- row 1) index)))))
      






  