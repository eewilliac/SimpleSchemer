#lang racket
(define sub1
  (lambda(num)
    (- num 1)))

 (define add1
   (lambda(num)
     (+ num 1)))

(define recursiveSubtract
  (lambda (a b)
    (cond
      ((zero? b)a)
      (else
       (sub1(recursiveSubtract a(sub1 b)))))))

(define recursiveAdd
  (lambda(a b)
    (cond
      ((zero? b)a)
      (else
       (add1 (recursiveAdd a (sub1 b)))))))

(define tupadd
  (lambda(atuple)
    (cond
      ((null? atuple)0)
      (else
       (recursiveAdd (car atuple)(tupadd (cdr atuple)))))))

(define recursiveMult
  (lambda(n m)
    (cond
      ((zero? m)0)
      (else
       (recursiveAdd n(recursiveMult n (sub1 m)))))))

(define etup1 (list 3 6 9 11 4))
(define etup2 (list 8 5 2 0 7))
(define tupAdd
  (lambda(tup1 tup2)
    (cond
      ((or (null? tup1)(null? tup2))(quote()))
      (else
       (cons(recursiveAdd (car tup1)(car tup2)) (tupAdd (cdr tup1)(cdr tup2)))))))