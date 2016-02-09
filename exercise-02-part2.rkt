#lang racket

(require eopl/datatype)
(require eopl/eopl)
(require racket/block)

(define (value? v) #t)

; Exercise 2.21: Implement the data type of environments.
; All interfaces are the same as in exercise-02-part1.rkt
(define-datatype env env?
  (empty-env)
  (extend-env
   [saved-var symbol?]
   [saved-val value?]
   [saved-env env?]))

(define (apply-env e var)
  (cases env e
    (empty-env [] (error "variable not found"))
    (extend-env
     [saved-var saved-val saved-env]
     (if (eqv? saved-var var)
         saved-val
         (apply-env saved-env var)))))

(define (empty-env? e)
  (cases env e
    (empty-env [] #t)
    (else #f)))

(define (has-binding? e var)
  (cases env e
    (empty-env [] #f)
    (extend-env
     [saved-var saved-val saved-env]
     (if (eqv? saved-var var)
         #t
         (has-binding? saved-env var)))))

; Exercise 2.22: Implement the data type of stack.
(define-datatype stack stack?
  (empty-stack)
  (push
   [v value?]
   [st stack?]))

(define (top st)
  (cases stack st
    (empty-stack [] (error "top on empty stack"))
    (push [top-val _] top-val)))

(define (empty-stack? st)
  (cases stack st
    (empty-stack [] #t)
    (else #f)))

(define (pop st)
  (cases stack st
    (empty-stack [] (error "pop on empty stack"))
    (push [_ saved-stack] saved-stack)))

; Exercise 2.24: Convert a binary tree to a list.
(define-datatype bintree bintree?
  (leaf-node [num integer?])
  (interior-node
   [key symbol?]
   [left bintree?]
   [right bintree?]))

; bintree-to-list: Bintree -> List
(define (bintree-to-list tree)
  (cases bintree tree
    (leaf-node [num] '(leaf-node num))
    (interior-node
     [key left right]
     (list 'interior-node key (bintree-to-list left) (bintree-to-list right)))))

; Exercise 2.25: Find the interior node with maximum leaf sum, return its symbol.
; Note that there should be at least one interior node, otherwise return false.
; max-interior: Bintree -> Symbol
(define (max-interior tree)
  (letrec
      ([max-sum 0]
       [max-symbol #f]
       [leaf-sum
        (lambda (tree)
          (cases bintree tree
            (leaf-node [num] num)
            (interior-node
             [key left right]
             (let*
                 ([left-sum (leaf-sum left)]
                  [right-sum (leaf-sum right)]
                  [curr-sum (left-sum . + . right-sum)])
               (if (or
                    (not max-symbol)
                    (>= curr-sum (max right-sum left-sum)))
                   ; Imperative. Update the maximum records.
                   (block
                    (set! max-sum curr-sum)
                    (set! max-symbol key)
                    curr-sum)
                   curr-sum)))))])
    (block
     (leaf-sum tree)
     max-symbol)))

; Example binary tree for testing.
(define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))

; Exercise 2.26: Implement red-blue tree and mark procedure as in exercise 1.33.
(define-datatype red-blue-tree red-blue-tree?
  (red-node
   [left red-blue-tree?]
   [right red-blue-tree?])
  (blue-node [children (list-of red-blue-tree?)])
  (leaf-rb-node [num integer?]))

; mark-helper: RBTree Int -> RBTree
(define (mark-helper tree red-count)
  (cases red-blue-tree tree
    (leaf-rb-node [_] (leaf-rb-node red-count))
    (red-node
     [left right]
     ; Increase count and continue traversing.
     (let ([new-count (red-count . + . 1)])
       (red-node (mark-helper left new-count) (mark-helper right new-count))))
    (blue-node
     [children]
     ; Keep the current count and map to all children.
     (blue-node (map (lambda (t) (mark-helper t red-count)) children)))))
; mark-leaves-with-red-depth: RBTree -> RBTree
(define (mark-leaves-with-red-depth tree)
  (mark-helper tree 0))

; Example red blue tree for testing.
(define rb
  (red-node
   (blue-node (list (leaf-rb-node 26) (leaf-rb-node 12)))
   (red-node
    (leaf-rb-node 11)
    (blue-node (list (leaf-rb-node 117) (leaf-rb-node 14))))))

; Exercise: 2.29: Suppose the grammar for lambda-calculus expressions is as follow:
; Lc-exp = identifier | (lambda ({identifier}*) Lc-exp | (Lc-exp {Lc-exp}*)
; Write the new data type and a parser.
(define-datatype lc-exp lc-exp?
  (var-exp [var symbol?])
  (lambda-exp
   [bounded-vars (list-of symbol?)]
   [body lc-exp?])
  (app-exp
   [rator lc-exp?]
   [rands (list-of lc-exp?)]))

; parse-lc-exp: Val -> LcExp
; Assume input is always valid.
(define (parse-lc-exp datum)
  (cond
    [(symbol? datum) (var-exp datum)]
    [(pair? datum)
     (if (eqv? (car datum) 'lambda)
         (lambda-exp (cadr datum)
                     (parse-lc-exp (last datum)))
         (app-exp (parse-lc-exp (car datum))
                  (map (curry parse-lc-exp) (cdr datum))))]))

; Exercise 2.31: Write a parser to convert a prefix-list to the abstract syntax.
(define-datatype prefix-exp prefix-exp?
  (const-exp [num integer?])
  (diff-exp
   [operand1 prefix-exp?]
   [operand2 prefix-exp?]))

; parse-prefix-lst: Listof(Int|-) -> PrefixExp
(define (parse-prefix-lst lst)
  (letrec
      ; parse-first-exp: List(Int|-) -> PrefixExp List
      ; usage: Parse the first parsable expression and return the remaining.
      ([parse-first-exp
        (lambda (lst)
          (cond
            [(null? lst) (error "malformed prefix list")]
            ; Const expression.
            [(integer? (car lst)) (values (const-exp (car lst)) (cdr lst))]
            ; Diff expression.
            [(eqv? '- (car lst))
             (let*-values
                 ([{operand1 remains1} (parse-first-exp (cdr lst))]
                  [{operand2 remains2} (parse-first-exp remains1)])
               (values (diff-exp operand1 operand2) remains2))]
            [else (error "unrecognizable symbol")]))])
    ; Only parse the first legitimate expression, ignore the rest.
    (let-values ([{res-exp _} (parse-first-exp lst)])
      res-exp)))
