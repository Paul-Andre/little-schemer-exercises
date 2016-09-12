#lang racket
(define (atom? a)
  (not (or (null? a) (pair? a))))

(define (insertL-f test?)
  (λ (new old l)
    (cond ((null? l) '())
          ((test? old (car l)) (cons old
                                     (cons new
                                           ((insertL-f test?) new old (cdr l)))))
          (else (cons (car l)
                      ((insertL-f test?) new old (cdr l)))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? old (car l)) (cons new
                                       (cons old
                                             ((insertR-f test?) new old (cdr l)))))
            (else (cons (car l)
                        ((insertR-f test?) new old (cdr l))))))))

(define insert-g-1
  (λ (test? right?)
    (λ (new old l)
      (cond ((null? l) '())
            ((test? old (car l)) ((λ (a)
                                     (if right?
                                         (cons new
                                               (cons old
                                                     a))
                                         (cons old
                                               (cons new
                                                     a))))
                                    ((insert-g-1 test? right?) new old (cdr l))))
            (else (cons (car l)
                        ((insert-g-1 test? right?) new old (cdr l))))))))
   
(define find-do
  (λ (found? do)
    (λ (l)
      (cond ((null? l) '())
            ((found? (car l)) (do (car l) ((find-do found? do) (cdr l))))
            (else (cons (car l)
                        ((find-do found? do) (cdr l))))))))
(define multirember&co
  (lambda ( a lat col)
    (cond
      ( ( null? lat)
        ( col ( quote ()) (quote ())))
      ( ( eq? ( car lat) a)
        ( multirember&co a
                         ( cdr lat)
                         (lambda ( newlat seen)
                           ( col newlat
                                 ( cons ( car lat) seen)))))
      ( else
        ( multirember&co a
                         ( cdr lat)
                         (lambda ( newlat seen)
                           ( col ( cons ( car lat) newlat)
                                 seen) ) ) ) ) ) )



;((find-do (λ (a) (< a 5)) (λ (head tail) (cons 5 tail))) '(3 4 5 67 1 3 5 1 23 5 24 5 4))
                         
(define multiinsertLR
  (λ (new oldL oldR l)
    (cond ((null? l) '())
          ((eq? oldL (car l)) (cons new
                                    (cons oldL
                                          (multiinsertLR new oldL oldR (cdr l)))))
          ((eq? oldR (car l)) (cons oldR
                                    (cons oldL
                                          (multiinsertLR new oldL oldR (cdr l)))))
          (else (cons (car l)
                      (multiinsertLR new oldL oldR (cdr l)))))))

(define multiinsertLR&co
  (λ (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat) (λ (newlat leftInserts rightInserts)
                                                                            (col (cons new
                                                                                       (cons oldL
                                                                                             newlat))
                                                                                 (+ 1 leftInserts)
                                                                                 rightInserts))))
          ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat) (λ (newlat leftInserts rightInserts)
                                                                            (col (cons oldR
                                                                                       (cons new
                                                                                             newlat))
                                                                                 leftInserts
                                                                                 (+ 1 rightInserts)))))
          (else (multiinsertLR&co new oldL oldR (cdr lat) (λ (newlat leftInserts rightInserts)
                                                            (col (cons (car lat)
                                                                       newlat)
                                                                 leftInserts
                                                                 rightInserts)))))))
                                                                                      
(define multiinsertLR&co1
  (λ (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          (else (multiinsertLR&co new oldL oldR (cdr lat) (λ (newlat leftInserts rightInserts)
                                                            (cond ((eq? oldL (car lat))
                                                                   (col (cons new (cons oldL newlat))
                                                                        (+ 1 leftInserts)
                                                                        rightInserts))
                                                                  ((eq? oldR (car lat))
                                                                   (col (cons oldR (cons new newlat))
                                                                        leftInserts
                                                                        (1 + rightInserts)))
                                                                  (else
                                                                   (col (cons (car lat) newlat)
                                                                        leftInserts
                                                                        rightInserts)))))))))

(define even?
  (λ (a)
    (= 0 (remainder a 2))))

(define evens-only*
  (λ (l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))))))

(define evens-only*&co 
  ;This function takes a nested list of numbers and
  ;builds a nested list of the even numbers
  ;and simultaneously multiplies the even numbers and sums
  ;up the odd numbers.
  ;The collector function "col" recieves three arguments:
  ; -the evens-only list,
  ; -the product of the even numbers
  ; -the sum of the odd numbers.
  (λ (l col)
    
    (define self
      (λ (l col)
        (cond ((null? l) (col '() 1 0))
              ((atom? (car l))
               (cond ((even? (car l))
                      (self (cdr l) (λ (evens-list evens-prod odds-sum)
                                      (col (cons (car l) evens-list)
                                           (* (car l) evens-prod)
                                           odds-sum))))
                     (else
                      (self (cdr l) (λ (evens-list evens-prod odds-sum)
                                      (col evens-list
                                           evens-prod
                                           (+ (car l) odds-sum)))))))
              (else (self (car l) (λ (evens-list-car evens-prod-car odds-sum-car)
                                    (self (cdr l) (λ (evens-list-cdr evens-prod-cdr odds-sum-cdr)
                                                    (col (cons evens-list-car
                                                               evens-list-cdr)
                                                         (+ evens-prod-car evens-prod-cdr)
                                                         (* odds-sum-car odds-sum-cdr))))))))))
    (self l col)))
                  
                  

(evens-only*&co '(1 2 3 4 5 6 ( 1 2 3 4( 1 2 3) (1)) 1 2 3) (λ a a))