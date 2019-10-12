(define (for iter end product) (if (= iter end) (* product end) 
(for (+ iter 1) end (* product iter) )))

(define (fact-iter n) (for 1 n 1))

(define (sum-iter start end) 
      (define (for-sum helper) 
              (if (= helper end)
              helper 
              (+ helper (for-sum (+ helper 1)))))
                                       (for-sum start))

(define (expt-iter x n) 
        (define (for-expt iter product) 
          (if (>= iter n) 
               product  
              (for-expt (+ iter 1) (* x product)))) 
   (cond((= n 0) 1)
        ((< n 0) -1)
        (else (for-expt 1 x))))

(define (count-digits n) 
        (if (= (remainder n 10) n) 
                    1 
                    (+ (count-digits (/ n 10) 1))))


(define (count-digits-iter n sum) 
  (if (= (remainder n 10) n) 
      (+ sum 1) 
      (count-digits-iter (/ n 10) (+ sum 1))))

(define (for-prime n counter) 
  (cond ((= (quotient n 2) counter) #t)
        ((= (remainder n counter) 0) #f) 
        (else (for-prime n (+ counter 1)))))

(define (prime n) (if (< n 2) #f (for-prime n 2)))

(define (sum-digits n sum) 
  (if (= (remainder n 10) n) 
       (+ n sum) 
       (sum-digits (quotient n 10) (+ sum (reminder n 10)))))

(define (sum-dig n) (sum-digits n 0))

(define (reverse-digits n result)
  (if (= (remainder n 10) n) 
      (+ (* result 10) (remainder n 10))
      (reverse-digits (quotient n 10) (+ (* result 10) (remainder n 10)))))

(define (rev-dig n) (reverse-digits n 0))
