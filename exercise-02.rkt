#lang racket
(require racket/block)

; Exercise 2.5: Implement 'env' in association-list representation.
; Env is a 2-list containing saved variable/value pair and remaining env.
; empty-env: () -> Env
(define (empty-env) '())
; extend-env: Var Val Env -> Env
(define (extend-env var val env)
  (list (list var val) env))
; apply-env: Env Var -> Val
(define (apply-env env var)
  (if (null? env)
      (error "no such variable")
      (let ([saved-var (caar env)]
            [saved-val (cadar env)]
            [remain-env (last env)])
        (if (eqv? var saved-var)
            saved-val
            (apply-env remain-env var)))))

; Exercise 2.8: Add an observer to test whether an env is empty.
; empty-env?: Env -> Bool
(define (empty-env? env)
  (null? env))

; Exercise 2.9: Add an observer to test whether a variable has binding.
; has-binding?: Env Var -> Bool
(define (has-binding? env var)
  (if (empty-env? env)
      #f
      (let ([saved-var (caar env)]
            [remain-env (last env)])
        (if (eqv? var saved-var)
            #t
            (has-binding? remain-env var)))))

; Exercise 2.10: Bind multiple variable/value bindings. Assume variable
; list and value list are of the same length.
; extend-env*: Listof(Sym) Listof(Val) Env -> Env
(define (extend-env* vars vals env)
  (if (or (null? vars) (null? vals))
      env
      (extend-env (car vars) (car vals)
                  (extend-env* (cdr vars) (cdr vals) env))))

; Exercise 2.11: Implement 'env' in ribcage representation.
(define (extend-envr var val env)
  (list (list (list var) (list val)) env))
(define (extend-envr* vars vals env)
  (list (list vars vals) env))
(define (apply-envr env var)
  (letrec ([index 0]  ; Only for 'find-index', will be shadowed.
           [find-index (lambda (lst v)
                         (cond
                           [(null? lst) #f]
                           [(eqv? (car lst) v) index]
                           [else (block
                                  (set! index (1 . + . index))
                                  (find-index (cdr lst) v))]))])
    (if (empty-env? env)
        (error "no such variable")
        (let* ([vars (caar env)]
               [vals (cadar env)]
               [index (find-index vars var)])
          (if index
              (list-ref vals index)
              (apply-envr (last env) var))))))

; Example environment for testing.
(define e1
  (extend-env
   'd 6 (extend-env
         'y 8 (extend-env
               'x 7 (extend-env
                     'y 14 (empty-env))))))
(define e2 (extend-env* (list 'z 'm 'n) (list 100 101 102 103) e1))
(define er
  (extend-envr*
   '(d y x y) '(6 8 7 14)
   (extend-envr
    'z 100 (extend-envr 'm 101 (extend-envr 'n 102 (empty-env))))))


