(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-iter op nv a b term next)
  (if (> a b) nv
      (accumulate-iter op (op nv (term a)) (next a) b term next)))

(define (next x) (+ x 1))

(define (count pred a b)
  (accumulate + 0 a b (lambda (x) (if (pred x) 1 0)) next))


(define (count-palind a b)
  (define (rev-dig n res)
    (if (= (remainder n 10) n)
        (+ (* res 10) n)
        (rev-dig (quotient n 10) (+ (* res 10) (remainder n 10)))))
  (accumulate + 0 a b (lambda (x) (if (= x (rev-dig x 0)) 1 0)) next))

(define (double f)
  (lambda (x) (f (f x))))


(define (rep f n)
      (lambda (x) (if (= n 0)
                  x
                  (f ((rep f (- n 1))x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (sq x) (* x x))

(define (exists? pred a b)
  (accumulate (lambda (x y) (or x y)) #f a b pred next))

(define (for-all? pred a b)
  (not (exists? (lambda (x) (if (pred x) #f #t)) a b)))


