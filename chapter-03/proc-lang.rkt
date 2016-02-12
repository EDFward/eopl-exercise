#lang eopl

(require eopl/tests/chapter3/proc-lang/proc-rep/data-structures)
(require eopl/tests/chapter3/proc-lang/proc-rep/environments)
(require eopl/tests/private/utils)


; Exercise 3.20: Write a Curried procedure to sum two arguments.
; Exercise 3.21: Extend the language to support multiple parameters and arguments
; in procedures.
; Exercise 3.23: Write a procedure for factorial.
; Exercise 3.24 ~ 3.29: Omitted.

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
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
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
     [var exp body]
     (value-of body (extend-env var (value-of exp env) env)))
    (proc-exp [vars body] (proc-val (procedure-mul vars body env)))
    (call-exp [rator rands]
              (let ([proc (expval->proc (value-of rator env))]
                    [args (map (lambda (e) (value-of e env)) rands)])
                (apply-procedure-mul proc args)))
    ))

; Procedural representation for procedure and its application.
(define (procedure var body env)
  ; Return the lambda for observer `apply-procedure`.
  (lambda (val) (value-of body (extend-env var val env))))
(define (apply-procedure proc val) (proc val))

; Extend the env with multiple variables and values, both of which are
; represented as lists.
(define (extend-env-mul vars vals env)
  (if (null? vars) env
      (extend-env-mul (cdr vars) (cdr vals)
                      (extend-env (car vars) (car vals) env))))

; Procedures with multiple parameters and arguments.
(define (procedure-mul vars body env)
  (lambda (vals) (value-of body (extend-env-mul vars vals env))))
(define (apply-procedure-mul proc vals) (proc vals))


; Tests.

(define (equal-answer? ans correct-ans)
  (equal? ans (sloppy->expval correct-ans)))

(define (sloppy->expval val)
  (cond
    [(number? val) (num-val val)]
    [(boolean? val) (bool-val val)]
    [else (eopl:error 'sloppy-expval "Can't convert value to expval")]))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

(check-run
 ;; simple applications
 (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
 (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
 (let-to-proc "(proc(f)(f 30)  proc(x)-(x,1))" 29)¬
 
 (nested-procs-1 "((proc (x) proc (y) -(x,y)  5) 6)" -1)
 (nested-procs-2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
                 -1)
 
 (y-combinator "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
   t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
 
 (curried "let f = proc (x) proc (y) -(x,-(0,y)) in ((f 3) 4)" 7)
 
 (multi-arguments "let f = proc(x,y) -(x,-(0,y)) in  (f 3 4)" 7)
 
 (factorial "
let maketimes = proc (maker)
                 proc (x, y)
                  if zero?(-(x,1)) then y else -(((maker maker) -(x,1) y), -(0, y))
in let
    times = (maketimes maketimes)
in let
    makefact = proc (maker)
                proc(x)
                 if zero?(-(x,1)) then 1 else (times ((maker maker) -(x,1)) x)
in let
    fact = (makefact makefact)
in (fact 6)" 720)
 )
