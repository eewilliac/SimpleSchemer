#lang racket

(define atom?
  (lambda(expression)
    (cond
      ((not(null? expression))(not(pair? expression)))
      (else #f))))