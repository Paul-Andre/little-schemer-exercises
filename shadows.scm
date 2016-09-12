#lang racket
(define (atom? a)
  (not (or (pair? a) (null? a))))
(define (add1 a)
  (+ a 1))
(define (sub1 a)
  (- a 1))

(define (numbered? l) ; my numbered accepts a format like (3 + 3 + 3)
  (cond ((null? l) #f)
        ((atom? l) (number? l))
        ((numbered? (car l))
         
         (cond ((null? (cdr l)) #t)
               ((or (eq? (car (cdr l)) (quote +))
                    (eq? (car (cdr l)) (quote *))
                    (eq? (car (cdr l)) (quote ^)))
                (numbered? (cdr (cdr l))))
               (else #f)))
        (else #f)))

#|
(define (value-backward l) ; it works, but evaluates in the backwards order. this isn't a problem when only two numbers are permitted
  (if (and (atom? l) (number? l)) 
      l
      (let ((f (value (car l))))
        (if (null? (cdr l))
            f
            (let ((n (value (cdr (cdr l)))))
              (cond
                ((eq? (car (cdr l)) (quote +)) (+ f n))
                ((eq? (car (cdr l)) (quote *)) (* f n))
                ((eq? (car (cdr l)) (quote ^)) (expt f n))))))))

(define (value-forward l)
  (define (get-op rep)
    (cond ((eq? rep (quote +)) +)
          ((eq? rep (quote *)) *)
          ((eq? rep (quote ^)) expt)))
  (define (iter so-far l)
    (cond ((null? l) so-far)
          (else (iter ((get-op (car l))
                       so-far
                       (value-forward (car (cdr l)))) 
                      (cdr (cdr l))))))
  (cond ((number? l) l)
        (else (iter (value-forward (car l)) (cdr l)))))
|#

(define (value l)
  (define (operand1 l)
    (car l))
  (define (operand2 l)
    (car (cdr (cdr l))))
  (define (operator l)
    (car (cdr l)))
  (define (do-operation op a b)
    ((cond ((eq? op (quote +)) +)
           ((eq? op (quote -)) -)
           ((eq? op (quote *)) *)
           ((eq? op (quote ^)) expt))
     a
     b))
  (cond ((number? l) l)
        (else (do-operation (operator l)
                           (value (operand1 l))
                           (value (operand2 l))))))



(value '(1 + (2 * 2)))