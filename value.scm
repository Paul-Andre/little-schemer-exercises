#lang racket
(define atom?
  (λ (a)
    (not (or (null? a) (pair? a)))))

(define add1
  (λ (a) (+  1 a)))
(define sub1
  (λ (a) ( - a 1)))
(define zero?
  (lambda (a) (= 0 a)))




(define (member? a lat)
  (cond ((null? lat) #f)
        (else (if (eq? a (car lat))
                  #t
                  (member? a (cdr lat))))))

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



(define (entry? l)
  (define (both-lists-have-the-same-length-and-the-first-is-a-set? f s)
    (cond ((and (null? f) (null? s)) #t)
          ((or (null? f) (null? s)) #f)
          ((member? (car f) (cdr f)) #f)
          (else (both-lists-have-the-same-length-and-the-first-is-a-set?
                 (cdr f)
                 (cdr s)))))
  (and (a-pair? l)
       (both-lists-have-the-same-length-and-the-first-is-a-set?
        (first l)
        (second l))))

(define new-entry build-pair)

(define lookup-in-entry
  (λ (key entry error-callback)
    (define (iter keys values)
      (cond ((null? keys) (error-callback key))
            ((eq? key (car keys)) (car values))
            (else (iter (cdr keys) (cdr values)))))
    (iter (first entry) (second entry))))
    
(define lookup-in-table
  (λ (key table error-callback)
    (if (null? table)
        (error-callback key)
        (lookup-in-entry key (car table) (λ (not-used-value)
                                           (lookup-in-table key (cdr table) error-callback))))))


(define expression-to-action
  (λ (e)
    (cond ((atom? e) atom-to-action)
          (else list-to-action))))

(define atom-to-action
  (λ (e)
    (cond 
          (( number? e) *const)
          (( eq? e #t ) *const)
          (( eq? e #f ) *const)
          (( eq? e (quote cons )) *const)
          (( eq? e (quote car )) *const)
          (( eq? e (quote cdr )) *const)
          (( eq? e (quote null?)) *const)
          (( eq? e (quote eq?)) *const)
          (( eq? e (quote atom?)) *const)
          (( eq? e (quote zero?) ) *const)
          (( eq? e (quote add1 ) ) *const)
          (( eq? e (quote sub1)) *const)
          (( eq? e (quote number?)) *const)
          (else *identifier))))

(define list-to-action
  (λ (l)
    (cond ((atom? (car l))
           (cond ((eq? (car l) (quote quote)) *quote)
                 ((or (eq? (car l) (quote lambda)) (eq? (car l) (quote λ))) *lambda)
                 ((eq? (car l) (quote cond)) *cond)
                 (else *application))
           (else *application)))))
      
(define value
  (λ (e)
    (meaning e '())))

(define meaning
  (λ (e table)
    ((expression-to-action e) e table)))

(define *const
  (λ (e table)
    (cond ((number? e) e)
          ((eq? #t e) #t)
          ((eq? #f e) #f)
          (else (build-pair (quote primitive) e)))))
(define *quote
  (λ (e table)
    (second e)))
(define initial-table
  (λ (name)
    (car (quote ()))))
(define *identifier
  (λ (e table)
    (lookup-in-table e table initial-table)))
(define *lambda
  (λ (e table)
    (build-pair 'non-primitive
                (cons table (cdr e)))))
(