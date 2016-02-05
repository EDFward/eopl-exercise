#lang racket

#|

S-list ::= ({S-exp}*)
S-exp  ::= Symbol | S-list

Equivalently,

S-list ::= ()
       ::= (S-exp . S-list)
S-exp  ::= Symbol | S-list

|#

(define (subst old new s-list)
  (match s-list
    ['() '()]
    [(cons s-exp s-list-remain)
     (cons
      (subst-in-s-exp old new s-exp)
      (subst old new s-list-remain))]))

(define (subst-in-s-exp old new s-exp)
  (if (symbol? s-exp)
      (if (eqv? old s-exp) new s-exp)
      ; Must be a s-list.
      (subst old new s-exp)))

; Exercise 1.12: Inline subst.
(define (subst-inline old new s-list)
  (match s-list
    ['() '()]
    [(cons s-exp s-list-remain)
     (cons
      (if (symbol? s-exp)
          (if (eqv? old s-exp) new s-exp)
          (subst old new s-exp))
      (subst old new s-list-remain))]))

; Exercise 1.13: Write subst using map,
; conforming to original kleene star grammar.
(define (subst-map old new s-list)
  (map ((curry subst-in-s-exp) old new) s-list))

; Exercise 1.15: Return a list of n copies of x.
; duple: Int Val -> Listof(Val)
(define (duple n x)
  (if (eqv? n 0)
      '()
      (cons x (duple (- n 1) x))))

; Exercise 1.16: Invert list of 2-lists.
; invert: List -> List
; usage: (invert '((a 1) (a 2) (1 b))) = ((1 a) (2 a) (b 1))
(define (invert lst)
  (map (λ (2-list) (cons (cadr 2-list) (car 2-list))) lst))

; Exercise 1.17: Wrap parentheses around each top-level element of lst.
; down: List -> List
; usage: (down '(1 2 3)) = ((1) (2) (3))
(define (down lst)
  map (list) lst)
      
; Exercise 1.18: Swap occurrences of s1 and s2.
; swapper: Val Val S-list -> S-list
; usage: (swapper 'a 'b '(a b c d)) = (b a c d)
(define (swapper s1 s2 s-list)
  (map (λ (s-exp)
         (if (symbol? s-exp)
             (cond [(eqv? s-exp s1) s2]
                   [(eqv? s-exp s2) s1]
                   [else s-exp])
             (swapper s1 s2 s-exp)))
       s-list))