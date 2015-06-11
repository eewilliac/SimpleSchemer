#lang racket
(define atom?
  (lambda(x)
    (and(not(pair? x))(not(null? x)))))

(define lat (list 'ewc 'aoq 'apd 'lw 'ef 'olw))

(define lat?
  (lambda(x)
    (cond
      ((null? x)#f)
      (atom? (car x) #t)
      (else
       (lat?(cdr x))))))

;;this is the problem with my programming.
;; the line with null? is fine.
;;but then atom? is a problem as well.

