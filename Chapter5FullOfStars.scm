#lang racket
(define atom?
  (lambda (dataStructure)
    (cond
      ((null? dataStructure)#f)
      ((not(pair? dataStructure))#t)
      (else #f))))
(define alist '(a b c))
(define longlist '(hotdogs and hamburger and cheezeburger and tomato))

(define multiRember
  (lambda (anAtom currentList)
    (cond
      ((null? currentList)(quote()))
      ((eq? (car currentList)anAtom)(multiRember anAtom (cdr currentList)))
      (else
       (cons (car currentList)(multiRember anAtom (cdr currentList)))))))


(define knottylist '((hotdogs and)((hamburgers)and)(cheezburger and)and (tomato)and))
;now lets see multiRember*
(define multiRember*
  (lambda (anAtom dataStructure)
    (cond
      ((null? dataStructure)(quote()))
      ((atom? (car dataStructure))(cond
                                    ((eq? (car dataStructure) anAtom)(multiRember* anAtom (cdr dataStructure)))
                                    (else
                                     (cons (car dataStructure)(multiRember* anAtom (cdr dataStructure))))))
      (else
       (cons (multiRember* anAtom (car dataStructure))(multiRember* anAtom(cdr dataStructure)))))))
       
(define atomCount*
  (lambda (anAtom dataStructure)
    (cond
      ((null? dataStructure)0)
      ((atom? (car dataStructure))(+ 1 (atomCount* anAtom (cdr dataStructure))))
      (else
       (+ 1 (atomCount* anAtom (cdr dataStructure)))