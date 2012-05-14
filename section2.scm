;;;; SICP - Chapter 2

;; 2.1
(define signature
  (lambda (n)
    (if (< n 0)
        -1
        1)))

(define make-rational
  (lambda (n d)
    (let ((g (gcd n d))
          (s (signature d)))
      (cons (* s (/ n g))
            (* s (/ d g))))))
;; 2.2
(define make-segment
  (lambda (a b)
    (cons a b)))

(define start-segment
  (lambda (line-segment)
    (car line-segment)))

(define end-segment
  (lambda (line-segment)
    (cdr line-segment)))

(define make-point
  (lambda (x y)
    (cons x y)))

(define x-point
  (lambda (point)
    (car point)))

(define y-point
  (lambda (point)
    (cdr point)))

(define midpoint-segment
  (lambda (line-segment)
    (make-point (average (x-point (start-segment line-segment))
                         (x-point (end-segment line-segment)))
                (average (y-point (start-segment line-segment))
                         (y-point (end-segment line-segment))))))

;; 2.3
(define make-rectangle ; Implementation 1
  (lambda (base-point width height)
    (cons base-point (cons width height))))

(define rectangle-width
  (lambda (rectangle)
    (car (cdr rectangle))))

(define rectangle-height
  (lambda (rectangle)
    (cdr (cdr rectangle))))

(define make-rectangle ; Implementation 2
  (lambda (point1 point2)
    (cons point1 point2)))

(define rectangle-width
  (lambda (rectangle)
    (abs (- (x-point (cdr rectangle))
            (x-point (car rectangle))))))

(define rectangle-height
  (lambda (rectangle)
    (abs (- (y-point (cdr rectangle))
            (y-point (car rectangle))))))

(define rectangle-perimeter
  (lambda (rectangle)
    (* 2 (+ (rectangle-width rectangle)
            (rectangle-height rectangle)))))

(define rectangle-area
  (lambda (rectangle)
    (* (rectangle-width rectangle)
       (rectangle-height rectangle))))

;; 2.4
(define cons-alt ; Returns a procedure that expects a procedure
  (lambda (x y)
    (lambda (m) (m x y))))

((cons-alt 2 3) +) ; 5
(define car-alt
  (lambda (z)
    (z (lambda (p q) p))))

;; (car-alt (cons-alt 1 2))
;; (car-alt (lambda (m) (m 1 2)))
;; ((lambda (m) (m 1 2)) (lambda (p q) p))

(define cdr-alt
  (lambda (z)
    (z (lambda (p q) q))))

;; 2.5
(define cons-nat
  (lambda (a b)
    (* (exponent 2 a) (exponent 3 b))))

(define index
  (lambda (n a)
    (if (not (= (remainder n a) 0))
        0
        (+ 1 (index (/ n a) a)))))

(define car-nat
  (lambda (product)
    (index product 2)))

(define cdr-nat
  (lambda (product)
    (index product 3)))

(cons-nat 2 3) ; 108
(car-nat (cons-nat 2 3)) ; 2
(cdr-nat (cons-nat 2 3)) ; 3

;; 2.6
(define zero ; [ x -> f(x) ] -> [ x -> x ]
  (lambda (f)
    (lambda (x) x)))

((zero inc) 2) ; 2

(define add-one ; [ x -> f^n(x) ] -> [ x -> f^(n+1)(x) ]
  (lambda (n)
    (lambda (f)
      (lambda (x) (f ((n f) x))))))

(((add-one zero) inc) 2) ; 3

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(define plus
  (lambda (n m)
    (lambda (f)
      (lambda (x) ((n f) ((m f) x))))))

(((plus zero zero) inc) 1) ; 1
(((plus zero one) inc) 1) ; 2
(((plus one zero) inc) 1) ; 2
(((plus one one) inc) 1) ; 3
(((plus two two) inc) 1) ; 5

;; Exercise 2.7

(define make-interval
  (lambda (a b)
    (cons a b)))

(define upper-bound
  (lambda (interval)
    (cdr interval)))

(define lower-bound
  (lambda (interval)
    (car interval)))

;; 2.8
(define sub-interval
  (lambda (x y)
    (add-interval x
                  (make-interval (- (upper-bound y))
                                 (- (lower-bound y))))))

;; 2.9
;; x = (a, b), y = (c, d) => x + y = (a + c, b + d), so that
;; width(x + y) = (1/2) * (b + d - (a + c))
;;              = (1/2) * (b - a) + (1/2) * (d - c)
;;              = width(x) + width(y)
(define width-interval
  (lambda (interval)
    (/ (- (upper-bound interval) (lower-bound interval)) 2.0)))

(width-interval (mul-interval (make-interval 0 1)
                              (make-interval 4 10))) ; 5.0

(* (width-interval (make-interval 0 1))
   (width-interval (make-interval 4 10))) ; 1.5

;; 2.10
(define make-interval
  (lambda (a b)
    (if (= a b)
        (error 'make-interval "must have distinct end-points.")
            (cons a b))))

;; 2.11
;; Draw a picture.

;; 2.12
(define make-center-percent
  (lambda (center percentage)
    (make-interval (* center (- 1 percentage))
                   (* center (+ 1 percentage)))))

(define percent-interval
  (lambda (interval)
    (- 1 (/ (lower-bound interval)
            (center-interval interval)))))

;; 2.13
;; Let A have center c, and tolerance s.  Let B have center d, and
;; tolerance t.  The tolerance of the product AB is given by the
;; formula
;; 1 - (2 * (1 - s) * (1 - t)) / ((1 - s) * (1 - t) + (1 + s) * (1 + t))
;; and when s and t are small, this formula is roughly equal to
;; 1 - (1 - s) * (1 - t)

; 0.00399998800003587
(percent-interval (mul-interval (make-center-percent 37 0.001)
                                (make-center-percent 82 0.003)))

; 0.003997000000000028
(- 1 (* (- 1 0.001) (- 1 0.003)))

;; 2.17
(define last-pair
  (lambda (list)
    (if (null? (cdr list))
        (car list)
        (last-pair (cdr list)))))

;; 2.18
(define reverse-list
  (lambda (items)
    (if (null? items)
        items
        (append (reverse (cdr items)) (list (car items))))))

;; 2.20
(define same-parity
  (lambda (x . w)
    (cond ((null? w) (list x))
          ((= 0 (remainder (- x (car w)) 2))
           (cons x (apply same-parity (car w) (cdr w))))
          (else
           (apply same-parity x (cdr w))))))

;; (apply proc v ... lst) applies proc using the content of 
;; (list* v ... lst)

;; 2.21
(define square-list
  (lambda (items)
    (if (null? items)
        '()
        (cons (square (car items))
              (square-list (cdr items))))))

(define square-list
  (lambda (items)
    (map square items)))

;; 2.22
(define square-list-iter
  (lambda (items)
    (define iter
      (lambda (things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer
                        (square (car things)))))))
    (iter items '())))

;; (square-list-iter '(1 2 3 4))
;; (iter '(1 2 3 4) '())
;; (iter '(2 3 4) (cons '() 1))
;; (iter '(3 4) (cons (cons '() 1) 4))
;; (iter '(3) (cons (cons (cons '() 1) 4) 9))
;; (iter '() (cons (cons (cons (cons '() 1) 4) 9) 16))
;; (cons (cons (cons (cons '() 1) 4) 9) 16)

;; 2.23
(define new-for-each
  (lambda (proc items)
    (cond ((not (null? items))
           (proc (car items))
           (new-for-each proc (cdr items))))))

;; 2.24
(list 1 (list 2 (list 3 4)))

;; (1 (2 (3 4)))  +---+---+  (2 (3 4))  +---+---+  (3 4)  +---+---+     +---+---+
;;           ---->| * | *-|------------>| * | *-|-------->| * | *-|---->| * | / |
;;                +-|-+---+             +-|-+---+         +-|-+---+     +-|-+---+
;;                  |                     |                 |             |
;;                  V                     V                 V             V
;;                +---+                 +---+             +---+         +---+
;;                | 1 |                 | 2 |             | 3 |         | 4 |
;;                +---+                 +---+             +---+         +---+

;; (1 (2 (3 4)))
;;      /\
;;     /  \
;;    1 (2 (3 4))
;;        /\
;;       /  \
;;      2  (3 4)
;;          /\
;;         /  \
;;         3  4

;; 2.25
(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7))))))))))))

;; 2.26
(define x '(1 2 3))
(define y '(4 5 6))

(append x y)
(cons x y)
(list x y)

;; 2.27
(define deep-reverse
  (lambda (items)
    (cond ((null? items) items)
          ((not (pair? (car items)))
           (append (deep-reverse (cdr items))
                   (list (car items))))
          (else
           (append (deep-reverse (cdr items))
                   (list (deep-reverse (car items))))))))

x
(deep-reverse x)

;; 2.28
(define fringe
  (lambda (tree)
    (cond ((null? tree) tree)
          ((not (pair? tree)) (list tree))
          (else (append (fringe (car tree))
                        (fringe (cdr tree)))))))

(define x '((1 2) (3 4)))
(fringe x)
(fringe (list x x))

;; 2.29
(define make-mobile
  (lambda (left right)
    (list left right)))

(define make-branch
  (lambda (length structure)
    (list length structure)))

(define left-branch
  (lambda (mobile)
    (car mobile)))

(define right-branch
  (lambda (mobile)
    (cadr mobile)))

(define branch-length
  (lambda (branch)
    (car branch)))

(define branch-structure
  (lambda (branch)
    (cadr branch)))

(define weight-branch
  (lambda (branch)
    (if (not (pair? branch))
        branch
        (foldl + (fringe branch)))))

(define balanced?
  (lambda (mobile)
    (if (not (pair? mobile))
        #t
        (let ((left (left-branch mobile))
              (right (right-branch mobile)))
          (and (= (torque left) (torque right))
               (balanced? (branch-structure left))
               (balanced? (branch-structure right)))))))

(define torque
  (lambda (branch)
    (* (branch-length branch)
       (mass branch))))

(define mass
  (lambda (branch)
    (let ((structure (branch-structure branch)))
      (if (not (pair? structure))
          structure
          (+ (mass (left-branch structure))
             (mass (right-branch structure)))))))

(define simple-mobile
  (make-mobile (make-branch 1 3)
               (make-branch 3 1)))

(define unbalanced-mobile
  (make-mobile (make-branch 1 1)
               (make-branch 1 2)))

(define test-mobile
  (make-mobile (make-branch 1
                            (make-mobile (make-branch 2 3)
                                         (make-branch 3 2)))
               (make-branch 1
                            (make-mobile (make-branch 3 2)
                                         (make-branch 2 3)))))

(balanced? simple-mobile)
(balanced? test-mobile)
(balanced? unbalanced-mobile)

;; 2.30
(define square-tree
  (lambda (tree)
    (map (lambda (subtree)
           (if (pair? subtree)
               (square-tree subtree)
               (square subtree)))
         tree)))

(define tree-map
  (lambda (proc tree)
    (map (lambda (subtree)
           (if (pair? subtree)
               (tree-map proc subtree)
               (proc subtree)))
         tree)))

(define square-tree
  (lambda (tree)
    (tree-map square tree)))

;; 2.32
(define subsets
  (lambda (set)
    (if (null? set)
        '(())
        (let ((rest (subsets (cdr set))))
          (append rest
                  (map (lambda (subset) (cons (car set) subset)) rest))))))

;; If S = {a_1, a_2, ... , a_n} then the set of subsets of S is
;; precisely the disjoint union of the set of subsets that a_1 and
;; the set of subsets that do not.

;; 2.32
(define map-new
  (lambda (proc sequence)
    (accumulate (lambda (x y) (cons (proc x) y)) '() sequence)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define append-new
  (lambda (seq1 seq2)
    (accumulate cons seq2 seq1)))

(define length-new
  (lambda (sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence)))

;; 2.34
(define horner-eval
  (lambda (x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                  (+ this-coeff
                     (* x higher-terms)))
                0
                coefficient-sequence)))

(horner-eval 2 '(1 3 0 5 0 1))

;; 2.35
(define count-leaves
  (lambda (tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else (+ (count-leaves (car tree))
                   (count-leaves (cdr tree)))))))

(define count-leaves
  (lambda (tree)
    (accumulate (lambda (x y) (+ 1 y))
                0
                (map enumerate-tree tree))))

(define count-leaves
  (lambda (tree)
    (accumulate +
                0
                (map (lambda (x) 1) (enumerate-tree tree)))))

;; 2.36
(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define accumulate-n
  (lambda (op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op
                          init
                          (map car seqs))
              (accumulate-n op
                            init
                            (map cdr seqs))))))

;; 2.37
(define test-matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define test-vector-1 '(1 2 3 4))
(define test-vector-2 '(4 5 6 6))

(define make-vector
  (lambda entries entries))

(define dot-product
  (lambda (v w)
    (accumulate +
                0
                (map * v w))))

(define matrix-*-vector
  (lambda (matrix vector)
    (map (lambda (row) (dot-product row vector)) matrix)))

(define transpose
  (lambda (matrix)
    (accumulate-n cons '() matrix)))

(define matrix-*-matrix
  (lambda (matrix-1 matrix-2)
    (map (lambda (column) (matrix-*-vector matrix-1 column))
         (transpose matrix-2))))

(define fold-left
  (lambda (op initial sequence)
    (define iter
      (lambda (result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest)))))
    (iter initial sequence)))

;; left and right folds give the same output when the operation is
;; commutative.

;; 2.39
(foldl cons '() '(1 2 3 4))
(define reverse
  (lambda (sequence)
    (foldr (lambda (x y) (cons y x)) '() sequence)))

;; 2.40
(define unique-pairs
  (lambda (n)
    (flatmap (lambda (i)
               (map (lambda (j) (list i j))
                    (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n))))

(define prime-sum-pairs
  (lambda (n)
    (map make-pair-sum (filter prime-sum? (unique-pairs n)))))

;; 2.41
(enumerate-interval 1 5)
(enumerate-interval 1 4)
(enumerate-interval 1 3)

(map (lambda (i)
       (map (lambda (j)
              (map (lambda (k) (list i j k))
                   (enumerate-interval 1 (- j 1))))
            (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 5))

(define test-triples
  (flatmap (lambda (i)
             (map (lambda (j)
                    (map (lambda (k) (permutations (list i j k)))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 3 5)))

(flatmap identity test-triples)

(permutations '(1 2 3))


;; 2.42
;; Assume we have generated the sequence of all possible ways to place
;; k-1 queens in the first k-1 columns of the board.  For each of these
;; ways, generate an extended set of positions by placing a queen in
;; each row of the kth column.  Now filter these, keeping only the
;; positions for which the queen in the kth column is safe wrt the other
;; queens.  This produces the sequence of all ways to place k queens in
;; the first k columns.
;; We implement this solution as a procedure `queens', which returns a
;; sequence of all solutions to the problem of placing n queens on an
;; n*n board.  `Queens' has an internal procedure `queen-cols' that
;; returns the sequence of all ways to place queens in the first k
;; columns of the board.

(define queens
  (lambda (board-size)
    (define queen-cols
      (lambda (k)
        (if (= k 0)
            (list empty-board)
            (filter
             (lambda (positions) (safe? k positions))
             (flatmap
              (lambda (rest-of-queens)
                (map (lambda (new-row)
                       (adjoin-position new-row k rest-of-queens))
                     (enumerate-interval 1 board-size)))
              (queen-cols (- k 1)))))))
    (queen-cols board-size)))

(define queen-cols ; return all ways to place queens in first k cols
  (lambda (k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 8)))
          (queen-cols (- k 1)))))))

(define column-test
  (lambda (k)
    (if (= k 0)
        '(())
        (flatmap
         (lambda (rest)
           (map (lambda (new-row)
                  (adjoin-position new-row k rest))
                (enumerate-interval 1 4)))
         (column-test (- k 1))))))

(define make-position
  (lambda (row col)
    (list row col)))

(define position-row
  (lambda (position)
    (car position)))

(define position-column
  (lambda (position)
    (cadr position)))

(define empty-board '())

(define adjoin-position
  (lambda (row column positions)
    (append positions (list (make-position row column)))))

(define queens-test
  (lambda (board-size)
    (define queen-cols
      (lambda (k)
        (if (= k 0)
            (list empty-board)
            (flatmap
             (lambda (rest-of-queens)
               (map (lambda (new-row)
                      (adjoin-position new-row k rest-of-queens))
                    (enumerate-interval 1 board-size)))
             (queen-cols (- k 1))))))
    (queen-cols board-size)))

(queen-cols 1)





;; Exercise 2.54
(define equal-test?
  (lambda (list1 list2)
     (cond ((and (empty? list1) (empty? list2)) true)
           ((not (eq? (car list1) (car list2))) false)
           (else (equal-test? (cdr list1) (cdr list2))))))


;; Exercise 2.56
(define make-ratio
  (lambda (num denom)
    (cond ((=number? num 0) 0)
          (else (list '/ num denom)))))

(define numerator
  (lambda (ratio)
    (cadr ratio)))

(define denominator
  (lambda (ratio)
    (caddr ratio)))

(define equal-ratio?
  (lambda (r1 r2)
    (equal? (make-product (numerator r1) (denominator r2))
            (make-product (numerator r2) (denominator r1)))))

(define ratio?
  (lambda (exp)
    (eq? (car exp) '/)))

(define make-diff
  (lambda (a1 a2)
    (cond ((=number? a1 0) (- a2))
          ((=number? a2 0) a1)
          (else (list '- a1 a2)))))

(define fast-expt
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

(define make-exponentiation
  (lambda (base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent))
           (fast-expt base exponent))
          (else (list '** base exponent)))))

(define base
  (lambda (exponentiation)
    (cadr exponentiation)))

(define exponent
  (lambda (exponentiation)
    (caddr exponentiation)))

(define exponentiation?
  (lambda (exp)
    (eq? (car exp) '**)))

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
          ((ratio? exp)
           (make-ratio (make-diff
                        (make-product (denominator exp)
                                      (deriv (numerator exp) var))
                        (make-product (numerator exp)
                                      (deriv (denominator exp) var)))
                       (make-exponentiation (denominator exp) 2)))
          ((exponentiation? exp)
           (make-product (exponent exp)
                         (make-exponentiation (base exp)
                                              (- (exponent exp) 1))))
          (else
           (error "unknown expression type -- DERIV" exp)))))

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

;; TODO 2.57, 2.58

