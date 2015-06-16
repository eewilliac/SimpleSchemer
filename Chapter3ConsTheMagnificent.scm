#lang racket

(define alonglist (list 'peppers 'and 'tomato 'and 'potato 'and 'lettuce 'and 'celery 'and 'kibble 'and 'bits 'and 'and 'codfish 'and 'ackee))
(define firstlist (list (list 'apple 'peach 'pumpkin)
                        (list 'plum 'pear 'cherry)
                        (list 'grape 'raisin 'pea)
                        (list 'bean 'carrot 'eggplant)))
                             

(define atom?
  (lambda(expression)
    (cond
      ((not(null? expression))(not(pair? expression)))
      (else #f))))


(define member?
  (lambda(atom lat)
    (cond
      ((null? lat)#f)
      ((or(eq? atom (car lat))(member? atom (cdr lat))))
      (else #f))))
       
(define rember
  (lambda(atom lat)
    (cond
      ((null? lat)(quote()))
      ((eq? atom (car lat))(cdr lat))
      (else
       (cons (car lat)(rember atom (cdr lat)))))))


(define firsts
  (lambda(lat)
    (cond
      ((null? lat)(quote()))
      (else
       (cons (car (car lat))(firsts (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat)(quote()))
      ((eq? (car lat) old)(cons old(cons new (insertR new old (cdr lat)))))
      (else
       (cons (car lat)(insertR new old (cdr lat)))))))


(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat)(quote()))
      ((eq? (car lat) old)(cons old(cons new(insertR new old (cdr lat))))) ;this is what you need to memorize (cons old (cons new (function (cdr list)))
      (else
       (cons (car lat)(insertR new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat)(quote()))
      ((eq? (car lat)old)(cons new (subst new old (cdr lat))))
      (else
       (cons (car lat)(subst new old (cdr lat)))))))


(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat)(quote()))
      ((or(eq?(car lat)old1)(eq?(car lat)old2))(cons new (subst2 new old1 old2 (cdr lat))))
      (else
       (cons (car lat)(subst2 new old1 old2 (cdr lat)))))))

;work on this one because my mental model is kinda shakey...
(define multirember
  (lambda (anAtom alat)
    (cond
      ((null? alat)(quote()))
      ((eq? (car alat) anAtom) (multirember anAtom (cdr alat)))
      (else
       (cons (car alat)(multirember anAtom(cdr alat)))))))
     