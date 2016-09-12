#lang racket
(define (atom? a)
  (not (or (pair? a) (null? a))))
(define (add1 a)
  (+ a 1))
(define (sub1 a)
  (- a 1))

(define (add-iter a b)
  (cond ((zero? b) a)
        (else (add-iter (add1 a) (sub1 b)))))
(define (sub-iter a b)
  (cond ((zero? b) a)
        (else (sub-iter (sub1 a) (sub1 b)))))
(define (add-rec a b)
  (cond ((zero? b) a)
        (else(add1 (add-rec a (sub1 b))))))
(define (sub-rec a b)
  (cond ((zero? b) a)
        (else (sub1 (sub-rec a (sub1 b))))))

(define (addtup tup)
  (if (null? tup)
      0
      (+ (car tup) (addtup (cdr tup)))))

(define (addtup-iter tup)
  (define (iter sum tup)
    (if (null? tup)
        sum
        (iter (+ sum (car tup)) (cdr tup))))
  (iter 0 tup))


(define (mul a b)
  (if (zero? b)
      0
      (+ a (mul a (sub1 b)))))

(define (mul-iter a b)
  (define (iter prod b)
    (if (zero? b)
        prod
        (iter (+ prod a) (sub1 b))))
  (iter 0 b))

(define (tup-op op tup1 tup2)
  (if (or (null? tup1) (null? tup2))
      '()
      (cons (op (car tup1) (car tup2)) (tup-op op (cdr tup1) (cdr tup2)))))

(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else
         (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(define (o> a b)
  (cond ((zero? a) #f)
        ((zero? b) #t)
        (else (o> (sub1 a) (sub1 b)))))

(define (o< a b)
  (cond ((zero? b) #f)
        ((zero? a) #t)
        (else (o< (sub1 a) (sub1 b)))))

(define (o= a b)
  (cond ((and (zero? a) (zero? b)) #t)
        ((or (zero? a) (zero? b)) #f)
        (else (o= (sub1 a) (sub1 b)))))

(define (o2= a b)
  (not (or (< a b) (> a b))))

(define (expt-rec b n)
  (if (zero? n)
      1
      (* b (expt-rec b (sub1 n)))))

(define (div-rec a b)
  (cond ((< a b) 0)
        (else (add1 (div-rec (- a b) b)))))

(define (div-iter a b)
  (define (iter ac a)
    (cond ((< a b) ac)
          (else (iter (add1 ac) (- a b)))))
  (iter 0 a))

(define (length-rec lat)
  (cond ((null? lat) 0)
        (else (add1 (length-rec (cdr lat))))))

(define (length-iter lat)
  (define (iter ac lat)
    (cond ((null? lat) ac)
          (else (iter (add1 ac) (cdr lat)))))
  (iter 0 lat))

(define (pick n lat)
  (cond ((= 1 n) (car lat))
        (else (pick (sub1 n) (cdr lat)))))

(define (no-nums lat)
  (cond ((null? lat) '())
        ((number? (car lat)) (no-nums (cdr lat)))
        (else (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
  (cond ((null? lat) '())
        ((not (number? (car lat))) (all-nums (cdr lat)))
        (else (cons (car lat) (all-nums (cdr lat))))))

(define (eqan? a b)
  (if (number? a)
      (if (number? b)
          (= a b)
          #f)
      (if (number? b)
          #f
          (eq? a b))))

(define (occur a lat)
  (cond ((null? lat) 0)
        ((eq? a (car lat)) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat)))))

(define (occur-iter a lat)
  (define (iter ac lat)
    (if (null? lat)
        ac(iter (if (eq? a (car lat))
                    (add1 ac)
                    ac)
                (cdr lat))))
  (iter 0 lat))

(define (one? a) 
  (= a 1))
                           
(define (rempick n lat)
  (cond ((null? lat) '())
        ((one? n) (cdr lat))
        (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))
