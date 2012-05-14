;;;; SICP - Chapter 1

;;; TODO
;;;   1, 4-9, 13, 14, 17, 22-28

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; 1.3
(define sum-max-squares
  (lambda (a b c)
    (cond ((and (<= a b) (<= a c)) (sum-squares b c))
          ((and (<= b a) (<= b c)) (sum-squares a c))
          ((and (<= c a) (<= c b)) (sum-squares a b)))))

;; 1.10
(define ackermann
  (lambda (x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (ackermann (- x 1)
                           (ackermann x (- y 1)))))))

(ackermann 1 10) ; 1024
(ackermann 2 4) ; 65536
(ackermann 3 3) ; 65536

(define f
  (lambda (n)
    (ackermann 0 n)))

(define g
  (lambda (n)
    (ackermann 1 n)))

(define h
  (lambda (n)
    (ackermann 2 n)))

;; By definition, f doubles a.
;; We have the recurrence g(n) = 2 * g(n-1) i.e. g raises 2 to the nth
;; power.
;; The equality A(2,n) = A(1,A(2,n-1)) shows that h(n) = g(h(n-1)), i.e.
;; h raises 2 to the power of h(n-1).

;; 1.11
(define f-rec
  (lambda (n)
    (if (< n 3)
        n
        (+ (f-rec (- n 1))
           (* 2 (f-rec (- n 2)))
           (* 3 (f-rec (- n 3)))))))

(define f
  (lambda (n)
    (define f-iter
      (lambda (a b c count)
        (if (= count 0)
            c
            (f-iter (+ a (* 2 b) (* 3 c))
                    a
                    b
                    (- count 1)))))
    (f-iter 2 1 0 n)))

;; 1.12
(define pascal
  (lambda (row column)
    (cond ((= row 0) 1)
          ((or (= column 0) (= column row)) 1)
          (else (+ (pascal (- row 1) (- column 1))
                   (pascal (- row 1) column))))))

;; 1.16
(define exponent
  (lambda (base index)
    (exponent-iter base index 1)))

(define exponent-iter
  (lambda (base index a)
    (cond ((= 0 index) a)
          ((even? index) (exponent-iter (square base)
                                        (/ index 2)
                                        a))
          (else (exponent-iter base
                               (- index 1)
                               (* base a))))))

;; 1.18
(define multiply
  (lambda (base k)
    (multiply-iter base k 0)))

(define multiply-iter
  (lambda (base k a)
    (cond ((= 0 k) base)
          ((even? k) (multiply-iter (double base)
                                    (/ k 2)
                                    a))
          (else (multiply-iter base
                               (- k 1)
                               (+ base a))))))

;; 1.19
(define fib
  (lambda (n)
    (fib-iter 1 0 0 1 n)))

(define fib-iter
  (lambda (a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))))))

;; 1.21
(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

;; 1.29
(define simpson
  (lambda (f a b n)
    (define h (/ (- b a) n))
    (define y
      (lambda (k)
        (define val (f (+ a (* k h))))
        (cond ((or (= k 0) (= k n)) val)
              ((odd? k) (* 4 val))
              (else (* 2 val)))))
    (* (/ h 3.0)
       (sum y 0 inc n))))

;; 1.30
(define sum
  (lambda (term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result)))))
    (iter a 0)))

;; 1.31
(define product
  (lambda (term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result)))))
    (iter a 1)))

;; 1.32
(define accumulate
  (lambda (combiner null-value term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result)))))
    (iter a null-value)))

;; 1.33
(define filtered-accumulate
  (lambda (filter combiner null-value term a next b)
    (define iter
      (lambda (a result)
        (cond ((> a b) result)
              ((not (filter a))
               (iter (next a) result))
              (else
               (iter (next a) (combiner (term a) result))))))
    (iter a null-value)))

(filtered-accumulate (lambda (x) #t) + 0 identity 0 inc 10)
(filtered-accumulate prime? * 1 identity 1 inc 10)

(define sum-prime-squares
  (lambda (a b)
    (filtered-accumulate prime? + 0 square a inc b)))

(define gcd
  (lambda (a b)
    (if (= b 0)
        (abs a)
        (gcd b (remainder a b)))))

(define coprime?
  (lambda (a b)
    (= (gcd a b) 1)))

(define coprime-product
  (lambda (n)
    (filtered-accumulate (lambda (i) (coprime? i n))
                         *
                         1
                         identity
                         1
                         inc
                         (- n 1))))
