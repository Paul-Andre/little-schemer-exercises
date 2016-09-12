#lang racket
;   9.
;   ...and Again, and Again, and Again,...


(define atom?
  (λ (a)
    (not (or (pair? a) (null? a)))))


(define pick
  (λ (a lat)
    (cond ((null? lat) #f)
          ((= 1 a) (car lat))
          (else (pick (- a 1) (cdr lat))))))

(define looking
  (λ (a lat)
    (define keep-looking
      (λ (n)
        (cond ((eq? a n) #t) 
              ((number? n) (keep-looking (pick n lat)))
              (else #f))))
    (keep-looking 1)))


(define build-pair
  (λ (a b)
    (cons a (cons b '()))))
(define first (λ (p) (car p)))
(define second (λ (p) (car (cdr p))))
(define a-pair?
  (λ (p)
    (and (pair? p)
         (pair? (cdr p))
         (null? (cdr (cdr p))))))


(define shift
  (λ (p)
    (build-pair (first (first p))
           (build-pair (second (first p))
                  (second p)))))

            
(define align
  (λ (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora)) (align (shift pora)))
          (else
           (build-pair (first pora)
                       (align (second pora)))))))

(define align-weight*
  (λ (pora)
    (cond ((atom? pora) 1)
          (else (+ (* 2 (align-weight* (first pora)))
                   (align-weight* (second pora)))))))

(define revpair
  (λ (p)
    (build-pair (second p)
                (first p))))

(define shuffle
  (λ (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora)) (shuffle (revpair pora)))
          (else
           (build-pair (first pora)
                       (shuffle (second pora)))))))

;mind boggling starts here:

(define length-1
  ((λ (f) (f f))
   (λ (self)
     (λ (l)
       (cond ((null? l) 0)
             (else (+ 1 ((self self) (cdr l)))))))))

(define length-2
  ((λ (f) (f f))
   (λ (mk-length)
     ((λ (length)
        (λ (l)
          (cond ((null? l) 0)
                (else (+ 1 (length (cdr l)))))))
      (λ (x) ((mk-length mk-length) x))))))

(define length-3 ; a guess
  ((λ (f l) (f f l))
   (λ (mk-length length)
     (length
      (λ (x) ((mk-length mk-length length) x))))
   (λ (length)
        (λ (l)
          (cond ((null? l) 0)
                (else (+ 1 (length (cdr l)))))))))

(define length-4
  ((λ (le)
     ((λ (f) (f f))
      (λ (self)
        (le
         (λ (x) ((self self) x))))))
   
   (λ (length)
     (λ (lat)
       (cond ((null? lat) 0)
             (else (+ 1 (length (cdr lat)))))))   ))

(define Y
  (λ (func)
    ((λ (f) (f f))
     (λ (f)
       (func
        (λ (x) ((f f) x)))))))
  
(length-4 '( 1 2 3))