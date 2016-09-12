#lang racket
(define (atom? a)
  (not (or (pair? a) (null? a))))

(define (member? a lat)
  (if (null? lat)
      #f
      (or (eq? a (car lat)) (member? a (cdr lat)))))

(define (multirember a lat)
  (if (null? lat)
      '()
      (if (eq? a (car lat))
          (multirember a (cdr lat))
          (cons (car lat) (multirember a (cdr lat))))))

(define (firsts l)
  (cond ((null? l) '())
        (else (cons (car (car l)) (firsts (cdr l))))))


(define (set? l)
  (cond ((null? l) #t)
        ((member? (car l) (cdr l)) #f)
        (else (set? (cdr l)))))

(define (makeset1 l)
  (cond ((null? l) '())
        ((member? (car l) (cdr l)) (makeset (cdr l)))
        (else (cons (car l)
                    (makeset (cdr l))))))
(define (makeset l)
  (cond ((null? l) '())
        (else (cons (car l)
                    (makeset (multirember (car l) (cdr l)))))))

(define (subset? sub super)
  (cond ((null? sub) #t)
        (else (and (member? (car sub) super)
                   (subset? (cdr sub) super)))))

(define (eqset? a b)
  (and (subset? a b)
       (subset? b a)))

(define (intersect? a b)
  (cond ((null? a) #f)
        (else (or (member? (car a) b)
                  (intersect? (cdr a) b)))))

(define (intersect a b)
  (cond ((null? a) '())
        ((member? (car a) b) (cons (car a)
                                   (intersect (cdr a) b)))
        (else (intersect (cdr a) b))))
(define (union a b)
  (cond ((null? a) b)
        (else (cond ((member? (car a) b) (union (cdr a) b))
                    (else (cons (car a)
                                (union (cdr a) b))))))) 
(define (difference a b)
  (cond ((null? a) '())
        (else (cond ((member? (car a) b) (difference (cdr a) b))
                    (else (cons (car a)
                                (difference (cdr a) b))))))) 

(define (intersectall l)
  (cond ((null? (cdr l)) (car l))
        (else (intersect (car l) (intersectall (cdr l))))))


(define (a-pair? l)
  (and (pair? l)
       (pair? (cdr l))
       (null? (cdr (cdr l)))))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build a b)
  (cons a
        (cons b
              '())))
(define (rel? l)
  (cond ((null? l) #t)
        (else (and (a-pair? (car l))
             (rel? (cdr l))))))

(define (fun?1 rel) ; this function assumes rel is a rel.
  (set? (firsts rel)))

(define (fun?2 l)
  (define (member? a l)
    (cond ((null? l) #f)
          ((eq? a (car (car l))) #t)
          (else (member? a (cdr l)))))
  
  (define (fun? l)
    (cond ((null? l) #t)
          (else (cond ((not (a-pair? (car l))) #f )
                      ((member? (car (car l)) (cdr l)) #f)
                      (else (fun? (cdr l)))))))
  (fun? l))


(define (revrel l)
  (define (rev p)
    (build (second p) (first p)))
  
  (cond ((null? l) '())
        (else (cons (rev (car l))
                    (revrel (cdr l))))))

(define (fullfun? l)
  (define (checkall l fs)
    
    (define (checkmember m f)
      (cond ((null? f) #f)
            (else (or ((car f) m)
                      (checkmember m (cdr f))))))
    (cond ((null? l) #f)
          (else (or (checkmember (car l) fs)
                    (checkall (cdr l) fs)))))
  
          
  (cond ((null? l) #t)
        (else (cond  ((checkall (cdr l) (list
                                          (λ(m) (eq? (first m) (first (car l))))
                                          (λ(m) (eq? (second m) (second (car l))))
                                          (λ(m) (not (a-pair? m))))
                       ) #f)
                      (else (fullfun? (cdr l)))))))
  

(fullfun? '((a b) (b a) (c w)))