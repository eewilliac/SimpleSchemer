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
      ((eq? (car lat) old)(cons old(cons new (cdr lat))))
      (else
       (cons (car lat)(insertR new old (cdr lat)))))))

      
     