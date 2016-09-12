#lang racket
(require compatibility/mlist)

(define (multirember-rec a lat)
  (if (null? lat)
      '()
      (if (eq? a (car lat))
          (multirember-rec a (cdr lat))
          (cons (car lat) (multirember-rec a (cdr lat))))))

(define (multirember-iter a lat)
  (define (iter last-element lat)
    (if (not (null? lat))
        (if (eq? a (car lat))
            (iter last-element (cdr lat))
            (let ((next-last-element (mcons (car lat) '())))
              (set-mcdr! last-element next-last-element)
              
              (iter next-last-element (cdr lat))))
        #f))
  (let ((dummy-node (mcons '() '()))) ;dummy node is needed in case the first element is to be removed
    (iter dummy-node lat)
    (mcdr dummy-node)))
            
(multirember-iter 2 '(1 2 3 4 5 2 3 4))