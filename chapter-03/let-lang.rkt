#lang eopl

(require eopl/tests/chapter3/let-lang/environments)
(require eopl/tests/chapter3/let-lang/lang)
(require eopl/tests/private/utils)

(require racket/block)

; Exercise 3.6: Extend the language by adding a new operator `minus`.
; Exercise 3.7: Extend the language to have addition, multiplication and
; integer quotient.
; Exercise 3.8: Add numeric equality predicate and numeric order predicates.
; Exercise 3.9: Add list processing operations like `car`, `cdr`, etc.
; Exercise 3.10: Add `list` operation to build lists directly.
; Exercise 3.12: Support `cond` expression.
; Exercise 3.15: Add `print` operation.
; Exercise 3.16: Extend `let` expression to support arbitrary number of variables.
; Exercise 3.17: Extend the language with `let*`.
; Exercise 3.18: Add `unpack` expression.


; ExpVal definition.

(define-datatype expval expval?
  (num-val [val number?])
  (bool-val [val boolean?])
  (list-val [val list?]))

(define (expval->num v)
  (cases expval v
    (num-val [num] num)
    (else (eopl:error "ExpVal not a number"))))

(define (expval->bool v)
  (cases expval v
    (bool-val [bool] bool)
    (else (eopl:error "ExpVal not a boolean"))))

(define (expval->list v)
  (cases expval v
    (list-val [lst] lst)
    (else (eopl:error "ExpVal not a list"))))


; Grammatical specification.

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") quo-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("emptylist") empty-list-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("print" "(" expression ")") print-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    ))


; Boilerplate of sllgen.

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


; Eval.

; run: String -> ExpVal
(define (run s) (value-of-program (scan&parse s)))
; value-of-program: Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    (a-program [exp1] (value-of exp1 (init-env)))))
; value-of: Exp Env -> ExpVal
(define (value-of exp env)
  (cases expression exp
    (const-exp [num] (num-val num))
    (var-exp [var] (apply-env env var))
    (diff-exp
     [exp1 exp2]
     (let ([num1 (expval->num (value-of exp1 env))]
           [num2 (expval->num (value-of exp2 env))])
       (num-val (- num1 num2))))
    (zero?-exp
     [exp1]
     (let ([num1 (expval->num (value-of exp1 env))])
       (bool-val (zero? num1))))
    (if-exp
     [exp1 exp2 exp3]
     (if (expval->bool (value-of exp1 env))
         (value-of exp2 env)
         (value-of exp3 env)))
    (let-exp
     [vars exps body]
     (value-of body (let-extend-env vars exps env)))
    (minus-exp
     [exp1]
     (num-val (- 0 (expval->num (value-of exp1 env)))))
    (add-exp
     [exp1 exp2]
     (let ([num1 (expval->num (value-of exp1 env))]
           [num2 (expval->num (value-of exp2 env))])
       (num-val (+ num1 num2))))
    (mul-exp
     [exp1 exp2]
     (let ([num1 (expval->num (value-of exp1 env))]
           [num2 (expval->num (value-of exp2 env))])
       (num-val (* num1 num2))))
    (quo-exp
     [exp1 exp2]
     (let ([num1 (expval->num (value-of exp1 env))]
           [num2 (expval->num (value-of exp2 env))])
       (num-val (quotient num1 num2))))
    (equal?-exp
     [exp1 exp2]
     (let ([num1 (expval->num (value-of exp1 env))]
           [num2 (expval->num (value-of exp2 env))])
       (bool-val (equal? num1 num2))))
    (greater?-exp
     [exp1 exp2]
     (let ([num1 (expval->num (value-of exp1 env))]
           [num2 (expval->num (value-of exp2 env))])
       (bool-val (> num1 num2))))
    (less?-exp
     [exp1 exp2]
     (let ([num1 (expval->num (value-of exp1 env))]
           [num2 (expval->num (value-of exp2 env))])
       (bool-val (< num1 num2))))
    (empty-list-exp [] (list-val '()))
    (cons-exp
     [exp1 exp2]
     (let ([head (value-of exp1 env)]
           [tail (expval->list (value-of exp2 env))])
       (list-val (cons head tail))))
    (car-exp [exp1] (car (expval->list (value-of exp1 env))))
    (cdr-exp [exp1] (list-val (cdr (expval->list (value-of exp1 env)))))
    (null?-exp [exp1] (null? (expval->list (value-of exp1 env))))
    (list-exp [exps] (list-val (map (lambda (e) (value-of e env)) exps)))
    (cond-exp [exps1 exps2] (eval-cond exps1 exps2 env))
    (print-exp [exp1]
               (let ([val1 (value-of exp1 env)])
                 (block
                  (write val1)
                  (num-val 1))))
    (let*-exp
     [vars exps body]
     (value-of body (let*-extend-env vars exps env)))
    (unpack-exp
     [vars exp1 body]
     (letrec ([vals (expval->list (value-of exp1 env))]
              [extend-env-multiple
               (lambda (vars vals env)
                 (if (null? vars) env
                     (extend-env-multiple (cdr vars) (cdr vals)
                                          (extend-env (car vars) (car vals) env))))])
       (if (= (length vals) (length vars))
           (value-of body (extend-env-multiple vars vals env))
           (eopl:error "Variable list size doesn't match given expression"))))
    ))

; eval-cond: Listof(Exp) List(Exp) Env -> ExpVal
; Evaluate cond expressions. The first argument is a list of conditional
; expressions and the second is the corresponding result expressions. After
; parsing those lists guarantee to be of the same size.
(define (eval-cond conds res env)
  (cond
    [(null? conds) (eopl:error "No tests succeeded in cond expression")]
    [(expval->bool (value-of (car conds) env)) (value-of (car res) env)]
    [else (eval-cond (cdr conds) (cdr res) env)]))

; let-extend-env: List(Var) List(Exp) Env -> Env
; Extend env with multiple variables. Variable list and expression list are
; guaranteed to be of the same size.
(define (let-extend-env vars exps env)
  (if (null? vars)
      env
      (extend-env (car vars) (value-of (car exps) env)
                  (let-extend-env (cdr vars) (cdr exps) env))))

; let*-extend-env: List(Var) List(Exp) Env -> Env
; Similar as `let-extend-env` but works like `let*`.
(define (let*-extend-env vars exps env)
  (if (null? vars)
      env
      (let*-extend-env (cdr vars) (cdr exps)
                       (extend-env (car vars) (value-of (car exps) env) env))))

; Tests.

; sugar-list: List -> ExpVal
; Note: Wrap a normal scheme list to the format of list-val in let-lang.
(define (sugar-list lst)
  (list-val (map
             (lambda (e)
               (cond
                 [(number? e) (num-val e)]
                 [(boolean? e) (bool-val e)]
                 [(list? e) (sugar-list e)]))
             lst)))

(define (equal-answer? ans correct-ans)
  (equal? ans (sloppy->expval correct-ans)))

(define (sloppy->expval val)
  (cond
    [(number? val) (num-val val)]
    [(boolean? val) (bool-val val)]
    [(list? val) (sugar-list val)]
    [else (eopl:error 'sloppy-expval "Can't convert value to expval")]))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

(check-run
 (minus-arith-1 "minus(11)" -11)
 (minus-arith-2 "minus(-(12, minus(-2)))" -10)
 
 (simple-addition "+(55, -(22,11))" 66)
 (simple-multiplication "*(2, +(8, 2))" 20)
 (simple-quotient "minus(/(9,2))" -4)
 
 (equality "if equal?(11,11) then 111 else 0" 111)
 (greater "if greater?(1,2) then 111 else 0" 0)
 (less "if less?(1,2) then 111 else 0" 111)
 
 (cons-list "let x=4 in cons(x,cons(cons(-(x,1), emptylist),emptylist))" (4 (3)))
 (simple-list "cons(1,cons(2,emptylist))" (1 2))
 (simple-car-1 "car(cons(1,emptylist))" 1)
 (simple-car-2 "car(cons(2, cons(1, emptylist)))" 2)
 (simple-cdr-1 "cdr(cons(1, emptylist))" ())
 (simple-cdr-2 "cdr(cons(2,cons(1,emptylist)))" (1))
 
 (build-list-1 "let x=4 in list(x,-(x,1),+(x,3))" (4 3 7))
 (build-list-2 "car(cdr(list(1, 2, 3)))" 2)
 (nested-lists "list(list(1),2,list(list(3)))" ((1) 2 ((3))))
 
 (cond "cond zero?(11) ==> 0 less?(3,2) ==> 1 greater?(10,9) ==> 2 end" 2)
 (print "print(*(2,16))" 1)
 
 (multiple-let "let x = 30 in let x = -(x,1) y = -(x,2) in -(x,y)" 1)
 (multiple-let* "let x = 30 in let* x = -(x,1) y = -(x,2) in -(x,y)" 2)
 (unpack "let u = 7 in unpack x y = cons(u,cons(3,emptylist)) in -(x,y)" 4)
 )
