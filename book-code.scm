;;;; Code from the SICP text

(define square
  (lambda (x)
    (* x x)))

(define sum-squares
  (lambda (x y)
    (+ (square x)
       (square y))))

(define abs
  (lambda (x)
    (if (< x 0)
        (- x)
        x)))

(define average
  (lambda (x y)
    (/ (+ x y)
       2)))

(define sqrt
  (lambda (x)
    (define good-enough?
      (lambda (guess)
        (< (abs (- (square guess) x)) 0.000001)))
    (define improve
      (lambda (guess)
        (average guess (/ x guess))))
    (define sqrt-iter
      (lambda (guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess)))))
    (sqrt-iter 1.0)))

(define factorial
  (lambda (n)
    (define fact-iter
      (lambda (product counter)
        (if (> counter n)
            product
            (fact-iter (* counter product)
                       (+ counter 1)))))
    (fact-iter 1 1)))

(define fibonacci
  (lambda (n)
    (define fibonacci-iter
      (lambda (a b count)
        (if (= count 0)
            b
            (fibonacci-iter (+ a b)
                            a
                            (- count 1)))))
    (fibonacci-iter 1 0 n)))

(define exponent
  (lambda (base index)
    (exponent-iter base index 1)))

(define exponent-iter
  (lambda (base index a)
    (cond ((= 0 index) a)
          ((even? index) (exponent-iter a
                                        (square base)
                                        (/ index 2)))
          (else (exponent-iter (* a base)
                               base
                               (- index 1))))))

(define double
  (lambda (n)
    (+ n n)))

(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))

(define find-divisor
  (lambda (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1))))))

(define divides?
  (lambda (a b)
    (= (remainder b a) 0)))

(define prime?
  (lambda (n)
    (= (smallest-divisor n) n)))

(define exponent-mod
  (lambda (base index modulus)
    (cond ((= index 0) 1)
          ((even? index)
           (remainder (square (exponent-mod base (/ index 2) modulus))
                      modulus))
          (else
           (remainder (* base (exponent-mod base (- index 1) modulus))
                      modulus)))))

(define fermat-test
  (lambda (n)
    (define try-it
      (lambda (a)
        (= (exponent-mod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

(define fast-prime?
  (lambda (n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f))))

(define cube
  (lambda (x)
    (* x x x)))

(define inc
  (lambda (x)
    (+ x 1)))

(define dec
  (lambda (x)
    (- x 1)))

(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b)))))

;;; Chapter 2
;;; 2.11 Example: Arithmetic Operations for Rational Numbers

(define make-rational
  (lambda (n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g)))))

(define numerator
  (lambda (rational)
    (car rational)))

(define denominator
  (lambda (rational)
    (cdr rational)))

(define add-rational
  (lambda (x y)
    (make-rational (+ (* (numerator x) (denominator y))
                      (* (numerator y) (denominator x)))
                   (* (denominator x) (denominator y)))))

(define subtract-rational
  (lambda (x y)
    (make-rational (- (* (numerator x) (denominator y))
                      (* (numerator y) (denominator x)))
                   (* (denominator x) (denominator y)))))

(define multiply-rational
  (lambda (x y)
    (make-rational (* (numerator x) (numerator y))
                   (* (denominator x) (denominator y)))))

(define divide-rational
  (lambda (x y)
    (make-rational (* (numerator x) (denominator y))
                   (* (denominator y) (numerator x)))))

(define equal-rational?
  (lambda (x y)
    (= (* (numerator x) (denominator y))
       (* (numerator y) (denominator x)))))

(define print-rational
  (lambda (x)
    (newline)
    (display (numerator x))
    (display "/")
    (display (denominator x))))

(define f
  (lambda (x y)
    ((lambda (a b)
       (+ (* x (square a))
          (* y b)
          (* a b)))
     (+ 1 (* x y))
     (- 1 ))))

(define f2
  (lambda (x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
      (+ (* x (square a))
         (* y b)
         (* a b)))))

(define add-interval
  (lambda (x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y)))))

(define mul-interval
  (lambda (x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4)))))

(define div-interval
  (lambda (x y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(define make-interval
  (lambda (a b)
    (cons a b)))

(define upper-bound
  (lambda (interval)
    (cdr interval)))

(define lower-bound
  (lambda (interval)
    (car interval)))

(define make-center-width
  (lambda (c w)
    (make-interval (- c w) (+ c w))))

(define center-interval
  (lambda (interval)
    (/ (+ (lower-bound interval) (upper-bound interval)) 2)))

;; Closure: an operation for combining data objects satisfies the
;; closure property if the results of combining things with that
;; operation can themselves be combined using the same operation.

;; (define list-ref
;;   (lambda (items n)
;;     (if (= n 0)
;;         (car items)
;;         (list-ref (cdr items) (- n 1)))))

;; (define map
;;   (lambda (proc items)
;;     (if (null? items)
;;         '()
;;         (cons (proc (car items))
;;               (map proc (cdr items))))))

(define count-leaves
  (lambda (tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else (+ (count-leaves (car tree))
                   (count-leaves (cdr tree)))))))

(define scale-tree
  (lambda (tree factor)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (scale-tree sub-tree factor)
               (* sub-tree factor)))
         tree)))

(define enumerate-tree
  (lambda (tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree)))))))

(define enumerate-interval
  (lambda (a b)
    (if (> a b)
        '()
        (cons a (enumerate-interval (+ a 1) b)))))

(accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 6)))

(define flatmap
  (lambda (proc seq)
    (accumulate append '() (map proc seq))))

(define prime-sum?
  (lambda (pair)
    (prime? (+ (car pair) (cadr pair)))))

(define make-pair-sum
  (lambda (pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))))

(define prime-sum-pairs
  (lambda (n)
    (map make-pair-sum
         (filter prime-sum?
                 (flatmap (lambda (i)
                            (map (lambda (j) (list i j))
                                 (enumerate-interval 1 (- i 1))))
                          (enumerate-interval 1 n))))))

(prime-sum-pairs 10)

(define permutations
  (lambda (set)
    (if (null? set)
        (list '())
        (flatmap (lambda (x)
                   (map (lambda (sub) (cons x sub))
                        (permutations (remove x set))))
                 set))))

(permutations '(1 2 3 4))

(define deriv
  (lambda (exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
          (else
           (error "unknown expression type -- DERIV" exp)))))

(define variable?
  (lambda (x)
    (symbol? x)))

(define same-variable?
  (lambda (v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))))

(define =number?
  (lambda (exp num)
    (and (number? exp) (= exp num))))

(define make-sum
  (lambda (a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2)))))

(define make-product
  (lambda (m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2)))))

(define sum?
  (lambda (x)
    (and (pair? x) (eq? (car x) '+))))

(define addend
  (lambda (s)
    (cadr s)))

(define augend
  (lambda (s)
    (caddr s)))

(define product?
  (lambda (x)
    (and (pair? x) (eq? (car x) '*))))

(define multiplier
  (lambda (p)
    (cadr p)))

(define multiplicand
  (lambda (p)
    (caddr p)))

(define element-of-set?
  (lambda (x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set))))))

(define adjoin-set
  (lambda (x set)
    (if (element-of-set? x set)
        set
        (cons x set))))

(define intersection-set
  (lambda (set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2)))))

;; sets as trees
;; The set {1,3,5,7,9,11} may be represented by a tree in a number of
;; different ways.  The only thing we require for a valid representation
;; is that all elements in the left subtree be smaller than the node
;; entry and that all elements in the right subtree be larger

;; Checking for membership then becomes an O(log n) time operation.

;; We represent trees by using lists.  Each node will be a list of three
;; items: the entry at the node, the left subtree and the right subtree.
;; A left or a right subtree of the empty list will idnicate that there
;; is no subtree connected there.  We can describe this representation
;; by the following procedures:

(define entry
  (lambda (tree)
    (car tree)))

(define left-branch
  (lambda (tree)
    (cadr tree)))

(define right-branch
  (lambda (tree)
    (caddr tree)))

(define make-tree
  (lambda (entry left right)
    (list entry left right)))

(define element-of-set?
  (lambda (x set)
    (cond ((null? set) false)
          ((= x (entry set)) true)
          ((< x (entry set))
           (element-of-set? x (left-branch set)))
          (else
           (element-of-set? x (right-branch set))))))

(define adjoin-set
  (lambda (x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          ((> x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set)))))))

;; Generating a Huffman tree
;; Idea is to arrange the tree so that symbols with least frequency
;; appear farthest away from root

;; Start with set of leaf nodes, containing symbols and relative
;; frequencies, determined from initial data.
;; Find two leaves with lowest weights and merge them, forming a tree
;; whoses left and right branches are those nodes, and adding
;; frequencies.
;; Remove two original nodes and replace with new tree.
;; Repeat
(define make-leaf
  (lambda (symbol weight)
    (list 'leaf symbol weight)))

(define leaf?
  (lambda (object)
    (eq? (car object) 'leaf)))

(define symbol-leaf
  (lambda (leaf)
    (cadr leaf)))

(define weight-leaf
  (lambda (leaf)
    (caddr leaf)))

(define make-code-tree
  (lambda (left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (+ weight right)))))

(define left-branch
  (lambda (tree)
    (car tree)))

(define right-branch
  (lambda (tree)
    (cadr tree)))

(define symbols
  (lambda (tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree))))

(define weight
  (lambda (tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree))))

(define decode
  (lambda (bits tree)
    (define decode-1
      (lambda (bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch
                   (choose-branch (car bits) current-branch)))
              (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree))
                  (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(define choose-branch
  (lambda (bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE-BRANCH" bit)))))

(define adjoin-set
  (lambda (x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set)))))))

(define make-leaf-set
  (lambda (pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
          (adjoin-set (make-leaf (car-pair) (cadr pair))
                      (make-leaf-set (cdr pairs)))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
