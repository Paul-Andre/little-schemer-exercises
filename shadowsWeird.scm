#lang racket
;;; weird number representation

(begin
  (define (zero? a)
    (null? a))
  (define (add1 a)
    (cons '() a))
  (define (sub1 a)
    (cdr a))
  
  (define (+ a b)
    (cond ((zero? b) a)
          (else(add1 (+ a (sub1 b))))))
  (define (- a b)
    (cond ((zero? b) a)
          (else(sub1 (- a (sub1 b))))))
  
  (- '(() () ()) '(() ()))
  
  )