#lang racket
(define (atom? a)
  (not (or (pair? a) (null? a))))

(define (lat? l)
  (if (null? l) 
      #t
      (and (atom? (car l))
           (lat? (cdr l)))))

(define (member? a lat)
  (if (null? lat)
      #f
      (or (eq? a (car lat)) (member? a (cdr lat)))))

(define (rember a lat)
  (if (null? lat)
      '()
      (if (eq? a (car lat))
          (cdr lat)
          (cons (car lat) (rember a (cdr lat))))))

(define (firsts l)
  (cond ((null? l) '())
        (else (cons (car (car l)) (firsts (cdr l))))))

(define (insertR new old lat)
  (if (null? lat) 
      '()
      (if (eq? old (car lat))
          (cons old
                (cons new (cdr lat)))
          (cons (car lat) (insertR new old (cdr lat))))))

(define (insertL new old lat)
  (if (null? lat) 
      '()
      (if (eq? old (car lat))
          (cons new lat)
          (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat )
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons new (cdr lat))
          (cons (car lat) (subst new old (cdr lat))))))


(define (subst2 new old1 old2 lat)
  (if (null? lat)
      '()
      (if (or (eq? old1 (car lat)) (eq? old2 (car lat)))
          (cons new (cdr lat))
          (cons (car lat) (subst new old1 old2 (cdr lat))))))
(define (multirember a lat)
  (if (null? lat)
      '()
      (if (eq? a (car lat))
          (multirember a (cdr lat))
          (cons (car lat) (multirember a (cdr lat))))))

(define (multiinsertR new old lat)
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons old 
                (cons new
                      (multiinsertR new old (cdr lat))))
          (cons (car lat) (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (if (null? lat)
      '()
      (if (eq? old (car lat))
          (cons new 
                (cons old
                      (multiinsertL new old (cdr lat))))
          (cons (car lat) (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond ((null? lat) '())
        ((eq? (car lat) old)
         (cons new (multisubst new old (cdr lat))))
        (else
         (cons (car lat) (multisubst new old (cdr lat))))))


(multiinsertL 'a 'b '(b b b b b c b))
          
      
(multirember 1 (quote (1 1 2 1 32 43 1 0)))
               
(multisubst 'a 'b '( b b g b a g b ))