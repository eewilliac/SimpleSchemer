#lang racket

(define alonglist (list 'peppers 'and 'tomato 'and 'potato 'and 'lettuce 'and 'celery 'and 'kibble 'and 'bits 'and 'and 'codfish 'and 'ackee))

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
       
