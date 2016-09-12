#lang racket
(define (atom? a)
  (not (or (pair? a) (null? a))))
(define (add1 a)
  (+ a 1))
(define (sub1 a)
  (- a 1))

(define (rember* a l)
  (cond ((null? l) '())
        ((eq? (car l) a) (rember* a (cdr l)))
        (else (cons (if (list? (car l))
                        (rember* a (car l))
                        (car l))
                    (rember* a (cdr l))))))



(define (insertR* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond 
           ((eq? (car l) old) (cons old
                                    (cons new 
                                          ( insertR* new old (cdr l)))))
           (else (cons (car l) (insertR* new old (cdr l))))))
        ((list? (car l)) (cons (insertR* new old (car l))
                               (insertR* new old (cdr l))))))


(define (rember** a l) ; the ** means that the procedure can also manage badly formatted lists (but it can give some wierd results)
  (cond ((and (equal? a (car l)) (eq? a (cdr l))) '())
        ((eq? a (car l)) (if (pair? (cdr l))
                             (rember** a (cdr l))
                             (cdr l)))
        ((eq? a (cdr l)) (if (pair? (car l))
                             (rember** a (car l))
                             (car l)))
        (else (cons (if (pair? (car l))
                        (rember** a (car l))
                        (car l))
                    (if (pair? (cdr l))
                        (rember** a (cdr l))
                        (cdr l))))))             

(define (occur** a l)
  (cond ((pair? l) (+ (occur** a (car l))
                      (occur** a (cdr l))))
        ((equal? a l) 1)
        (else 0)))

(define (subst** new old l)
  (cond ((equal? old l) new)
        ((pair? l) (cons (subst** new old (car l))
                         (subst** new old (cdr l))))
        (else l)))

(define (insertL* new old l)
  (cond ((null? l) '())
        ((atom? (car l))
         (cond ((eq? (car l) old) (cons new
                                        (cons old
                                              (insertL* new old (cdr l)))))
               (else (cons (car l)
                           (insertL* new old (cdr l))))))
        (else (cons (insertL* new old (car l))
                    (insertL* new old (cdr l))))))

                
(define (insertL*-2 new old l) ;this way, old can be a list, provided we change "eq?" for "equal?"
  (cond ((pair? l)
         (cond ((equal? (car l) old) (cons new
                                        (cons old
                                              (insertL*-2 new old (cdr l)))))
               (else (cons (insertL*-2 new old (car l))
                       (insertL*-2 new old (cdr l))))))
        (else l)))
                
(define (member** a l)
  (cond ((equal? a l) #t)
        ((pair? l) (or (member** a (car l))
                       (member** a (cdr l))))
        (else #f)))
                       
(define (leftmost l)
  (cond ((atom? l) l)
        ((pair? l) (leftmost (car l)))))

(define (eqlist? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        ((and (atom? (car a)) (atom? (car b))) (and (eq? (car a) (car b))
                                                    (eqlist? (cdr a) (cdr b))))
        ((or (atom? (car a)) (atom? (car b))) #f)
        (else (and (eqlist? (car a) (car b))
                   (eqlist? (car a) (car b))))))

(define (equal?-1 a b)
  (cond ((and (pair? a) (pair? b)) (and (equal?-1 (car a) (car b))
                                        (equal?-1 (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f)
        (else (eq? a b))))

(define (equal?-2 a b)
  (if (pair? a) 
      
      (if (pair? b)
          (and (equal?-2 (car a) (car b))
               (equal?-2 (cdr a) (cdr b)))
          #f)
         
      (if (pair? b)
          #f
          (eq? a b))))

(define (rember s l)
  (if 

(equal?-1 '((3 4 d a vb (sd s))) '((3 3 d a vb (sd s))))