#lang racket
(define atom?
  (lambda(x)
    (and(not(pair? x))(not(null? x)))))

(define lat?
  (lambda(x)
    (cond
      ((null? x)#f)
      (atom? (car x)#t)
      (lat? (cdr x)))))

(define lat (list 'ewc 'aoq 'apd 'lw 'ef 'olw))

  

