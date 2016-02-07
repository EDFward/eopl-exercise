#lang racket
(require racket/block)

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
  (map (λ (2-list) (list (cadr 2-list) (car 2-list))) lst))

; Exercise 1.17: Wrap parentheses around each top-level element of lst.
; down: List -> List
; usage: (down '(1 2 3)) = ((1) (2) (3))
(define (down lst)
  (map list lst))

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

; Exercise 1.19: Set n-th element of a list.
; list-set: List Int Val -> List
(define (list-set lst n x)
  (match lst
    ['() '()]
    [(cons hd tl) (if (= n 0)
                      (cons x tl)
                      (cons hd (list-set tl (- n 1) x)))]))

; Exercise 1.20: Count occurrences.
; count-occurrences: Val S-list -> Int
(define (count-occurrences s s-list)
  (for/sum ([s-exp s-list])
    (if (symbol? s-exp)
        (if (eqv? s-exp s) 1 0)
        (count-occurrences s s-exp))))

; Exercise 1.21: Cartesian prodcut of two lists.
; product: List List -> List
(define (product los1 los2)
  (for*/list ([i los1] [j los2])
    (list i j)))

; Exercise 1.22: Filter with predicates.
; filter-in: Pred List -> List
(define (filter-in pred lst)
  (match lst
    ['() '()]
    [(cons hd tl)
     (if (pred hd)
         (cons hd (filter-in pred tl))
         (filter-in pred tl))]))

; Exercise 1.23: Find index of first element satisfying the predicate.
; list-index: Pred List -> Int
(define (list-index pred lst)
  (letrec ([list-index-helper
            (lambda (lst i)
              (match lst
                ['() #f]
                [(cons hd tl)
                 (if (pred hd) i (list-index-helper tl (+ i 1)))]))])
    (list-index-helper lst 0)))

; Exercise 1.24: Return true if every element in the list satisfies
; the predicate.
; every?: Pred List -> Bool
; Note: andmap
(define (every? pred lst)
  (match lst
    ['() #t]
    [(cons hd tl)
     (if (pred hd)
         (every? pred tl)
         #f)]))

; Exercise 1.25: Return true if any element in the list satisfies
; the predicate.
; exists?: Pred List -> Bool
; Note: ormap
(define (exists? pred lst)
  (match lst
    ['() #f]
    [(cons hd tl)
     (if (pred hd)
         #t
         (exists? pred tl))]))

; Exercise 1.26: Remove pairs of brackets for top-level elements.
; up: List -> List
; usage:(up (down lst)) = lst
(define (up lst)
  (append*
   (map (lambda (v) (if (list? v) v (list v))) lst)))

; Exercise 1.27: Remove all parentheses.
; flatten: S-list -> List
(define (flatten s-list)
  (append*
   (map (lambda (v) (if (list? v) (flatten v) (list v))) s-list)))

; Exercise 1.28: Merge 2 sorted list.
; merge: Listof(Int) Listof(Int) -> Listof(Int)
(define (merge loi1 loi2)
  (cond
    [(empty? loi1) loi2]
    [(empty? loi2) loi1]
    [(< (car loi1) (car loi2))
     (cons (car loi1) (merge (cdr loi1) loi2))]
    [else (cons (car loi2) (merge loi1 (cdr loi2)))]))

; Exercise 1.29: Sort a list of integers.
; sort: Listof(Int) -> Lisfof(Int)
(define (sort loi)
  (if ((length loi) . <= . 1)
      loi
      (let*-values
          ([{pivot} (car loi)]
           [{smaller greater} (partition ((curry >) pivot) (cdr loi))])
        (append (sort smaller) (list pivot) (sort greater)))))

; Exercise 1.30: Sort a list of integers by a predicate.
; sort/predicate: Pred Listof(Int) -> Listof(Int)
(define (sort/predicate pred loi)
  (if ((length loi) . <= . 1)
      loi
      (let*-values
          ([{pivot} (car loi)]
           [{left right}
            (partition (lambda (v) (pred v pivot)) (cdr loi))])
        (append (sort/predicate pred left)
                (list pivot)
                (sort/predicate pred right)))))

; Exercise 1.31: On calculating on bintree.
; leaf: Int -> Node
(define (leaf i) i)
; interior-node: Symbol Node Node -> Node
(define (interior-node s left right)
  (list s left right))
; leaf?: Node -> Bool
(define (leaf? node)
  (integer? node))
; lson: Node -> Node
(define (lson node)
  (cond
    [(leaf? node) (error "cannot get left tree on a leaf")]
    [else (cadr node)]))
; rson: Node -> Node
(define (rson node)
  (cond
    [(leaf? node) (error "cannot get right tree on a leaf")]
    [else (last node)]))
; contents-of: Node -> Int | Symbol
(define (contents-of node)
  (if (leaf? node) node (car node)))

; Exercise 1.32: Double every integers in the leaves.
; double-tree: Node -> Node
(define (double-tree tree)
  (let ([contents (contents-of tree)])
    (if (leaf? tree)
        (* 2 contents)
        (list contents
              (double-tree (lson tree))
              (double-tree (rson tree))))))

; Exercise 1.33: Now every leaf should contain an integer of nodes
; between it and the root that contain the symbol 'red'.
; mark-helper: Node Int -> Node
(define (mark-helper tree red-count)
  (let ([contents (contents-of tree)])
    (if (leaf? tree)
        red-count
        (let ([new-red-count
               (+ red-count (if (eqv? contents 'red) 1 0))])
          (list contents
                (mark-helper (lson tree) new-red-count)
                (mark-helper (rson tree) new-red-count))))))
; mark-leaves-with-red-depth: Node -> Node
(define (mark-leaves-with-red-depth tree)
  (mark-helper tree 0))

; Exercise 1.34: Find path to an integer in a BST (guaranteed to exist).
; path: Int BST -> Listof(left|right)
(define (path n bst)
  (match bst
    [(list val left right)
     (cond
       [(= n val) '()]
       [else (let-values
                 ([{action subtree}
                   (if (< n val)
                       {values 'left left}
                       {values 'right right})])
               (cons action (path n subtree)))])]
    [else (error "malformed BST, or value not found")]))

; Exercise 1.35: Number each leaf in bintree start from 0, in pre-order.
; number-leaves: Node -> Node
(define (number-leaves tree)
  (letrec
      ([n 0]
       [number-leaves-helper
        (lambda (tree)
          (let ([contents (contents-of tree)])
            (cond
              ; Imperative.
              [(leaf? tree) (block
                             (set! n (+ 1 n))
                             (- n 1))]
              [else (list contents
                          (number-leaves-helper (lson tree))
                          (number-leaves-helper (rson tree)))])))])
    (number-leaves-helper tree)))

; Exercise 1.36: Write helper procedure g to help implement
; number-elements (zipping with indexes) in following way.
; number-elements: List -> Listof(2-List)
(define (number-elements lst)
  (if (null? lst) '()
      (g (list 0 (car lst)) (number-elements (cdr lst)))))
; g: 2-List Listof(2-List) -> Listof(2-List)
(define (g indexed-pair remaining-indexed-pairs)
  (cons
   indexed-pair
   (map
    (lambda (pair)
      (match pair
        [(list index ele)
         (list (+ 1 index) ele)]))
    remaining-indexed-pairs)))

; Example tree for testing.
(define example-bintree
  (interior-node
   'red
   (interior-node 'bar
                  (leaf 26)
                  (leaf 12))
   (interior-node 'red
                  (leaf 11)
                  (interior-node 'quux
                                 (leaf 117)
                                 (leaf 14)))))
(define example-bst
  '(14
    (7 () (12 () ()))
    (26 (20 (17 () ())
            ())
        (31 () ()))))