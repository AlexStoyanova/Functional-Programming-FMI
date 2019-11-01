(define (len l)
(define (myLength l n)
  (if (null? l) n
      (myLength (cdr l) (+ n 1))))
  (myLength l 0))

(define (sum l)
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))))

(define (memberR l x)
  (cond ((null? l) #f)
        ((equal? (car l) x) #t)
        (else (memberR (cdr l) x))))
   
(define (last l)
  (if (null? (cdr l)) (car l)
      (last (cdr l))))

(define (nth l n)
  (if (= n 0)
      (car l)
      (nth (cdr l) (- n 1))))

(define (scale l x)
  (map  (lambda (i) (* i x)) l))

(define (myReverse l)
  (define (rev l res)
    (if (null? l) res
        (rev (cdr l) (cons (car l) res))))
  (rev l '()))

(define (add-last l x)
  (myReverse (cons x (myReverse l))))

(define (myMap l f)
  (if (null? l) '()
  (cons (f (car l)) (myMap (cdr l) f))))

(define (filter l p)
  (if (null? l) '()
      (if (p (car l)) (cons (car l) (filter (cdr l) p))
          (filter (cdr l) p))))

(define (myAppend l1 l2)
  (if (null? l1) l2
        (cons (car l1) (myAppend (cdr l1) l2))))

(define (accumulate op nv l)
  (if (null? l) nv
      (op (car l) (accumulate op  nv (cdr l)) )))

(define (accumulate-iter op nv l)
  (if (null? l) nv
      (accumulate-iter op (op nv (car l)) (cdr l))))

(define (maximum l)
   (foldr max l))

(define (minimum l)
  (foldr min l))

(define (foldr op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr op (cdr l)))))

  
(define (remove l x)
  (if (null? l) '()
      (if (not (equal? (car l) x))
          (cons (car l) (remove (cdr l) x))
          (cdr l))))

(define (maximum2 l maxEl)
  (cond ((null? l) maxEl)
        ((> (car l) maxEl) (maximum2 (cdr l) (car l)))
        (else (maximum2 (cdr l) maxEl))))


(define (selection-sort-max l)
  (if (null? l) '()
      (cons (maximum l) (selection-sort-max (remove l (maximum l))))))

(define (selection-sort-min l)
  (if(null? l) l
     (cons (minimum l)(selection-sort-min (remove l(minimum l))))))

(define (selection-sort l comp)
  (if (equal? comp >)
      (selection-sort-max l)
      (selection-sort-min l)))

(define (consr l x) (cons x l))

(define (myPartition p l)
(define (partition p l list1 list2)
  (if (null? l) (list list1 list2)
      (if (p (car l))
          (partition p (cdr l) (append  list1 (list(car l))) list2)
          (partition p (cdr l) list1 (append list2 (list (car l)))))))
      (partition p l '() '()))


(define (flatten l)
  (if(null? l) '()
     (if(list? (car l))
        (append (car l) (flatten (cdr l)))
        (append (list (car l)) (flatten (cdr l))))))

(define (atom? x) (and (not (list? x)) (not (pair? x)))) 

(define (map-deep f l)
  (cond ((null? l) '())
        ((atom? (car l)) (cons (f (car l)) (map-deep f (cdr l))))
        (else (cons (map-deep f (car l)) (map-deep f (cdr l))))))
             
(define (zip a b)
  (if (or (null? a) (null? b))
      '()
      (cons (list (car a) (car b)) (zip (cdr a) (cdr b)))))

(define (remove l x)
  (if (null? l) '()
      (if (not (equal? (car l) x))
          (cons (car l) (remove (cdr l) x))
          (remove (cdr l) x))))

(define (remove-duplicates l)
  (if (null? l) '()
      (cons (car l) (remove-duplicates (remove l (car l))))))


(define (myReverse l)
  (define (rev l res)
    (if (null? l) res
        (rev (cdr l) (cons (car l) res))))
  (rev l '()))

(define (chunk l n)
  (define (help l n res)
    (if (or (null? l) (= n 0)) res
        (help (cdr l) (- 1 n) (cons (car l) res))))
  (cond ((null? l) '())
        ((<= (len l) n) l)
        (else (cons (myReverse (help l n '())) (chunk (cadr (myReverse (help l n '()))) n)))))

(define (myPartition p l)
(define (partition p l list1 list2)
  (if (null? l) (list list1 list2)
      (if (p (car l))
          (partition p (cdr l) (append  list1 (list(car l))) list2)
          (partition p (cdr l) list1 (append list2 (list (car l)))))))
      (partition p l '() '()))


(define (split l n)
  (define (iter l1 l2 counter)
    (if(= n counter)
       (list (myReverse l2) l1)
       (iter  (cdr l1) (cons (car l1) l2) (+ 1 counter))))
  (iter l '() 0))

  
(define (chunk l n)
  (if (< (len l) n) (list l)
      (cons (car (split l n))
       (chunk (cadr (split l n)) n))))


(define (snoc x l) (append l (list x)))

(define (split l n)
  (define (iter l1 l2 counter)
    (if (= n counter)
        (list l2 l1)
        (iter (cdr l1) (snoc (car l1) l2) (+ counter 1))))
  (iter l '() 0))

(define (chunk l n)
  (if (< (len l) n)
      (list l)
      (cons (car (split l n))
            (chunk (cadr (split l n)) n))))







