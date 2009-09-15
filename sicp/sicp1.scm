;;; SICP
;;; CHapter 1
; What are we studying?
; processes manipulate data
; programs direct processes
; experts organize, visualize, structure, and debug

; Why Lisp?
; procedures modeled as data
; fun!

;;; Chapter 1.1 Elements of Programming
; Languages organize our thoughts about processes
;   Mechanisms:
;      primitive expressions, means of combination and abstraction
;   compound expressions: application of procedure to data
;      (operators operands) -

; Names identify a variable, whose value is some item
; Environmnt is a pairing of names to values

; Evaluate subexpressions and apply value of operator to operands
;   special forms do not follow this rule

; Procedure definition
(define (square x)
  (* x x))

; Substitution models:
;   applicative order: evaluate args first (Scheme)
;   normal order: expand procedure applications first

; Conditional expressions
(define (abs x)
  (if (< x 0)
      (- x)
      x))

; Ex 1.3
(define (square-of-largest-two a b c)
  (let ([sum-of-squares (lambda (x y) (+ (square x) (square y)))])
    (cond [(and (> a c) (> b c)) (sum-of-squares a b)]
          [(and (> a b) (> c b)) (sum-of-squares a c)]
          [(and (> b a) (> c a)) (sum-of-squares b c)])))

; Ex 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Ex 1.5
; Applicative-order: p evaluates cyclically
; Normal-order: p never evaluated, returns 0
(define (avg a b)
  (/ (+ a b) 2))

; Newton's method for square roots
(define (sqrt number)
  (define (improve guess)
    (avg guess (/ number guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) number)) 1/1000))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 2))

; Ex 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt number)
  (define (improve guess)
    (avg guess (/ number guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) number)) 1/1000))
  (define (iter guess)
    (new-if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 2))

; Applicative order evaluation evaluates both then and else
; clauses regardless of the logical status of the if clause
; In this case, this causes the recursive step iter to be
; called infinitely, and the process never exits.

; Ex 1.7
; (sqrt (square 0.01)) -> 0.03... because this is good-enough?
; (sqrt (square 10000) -> never terminates because the precision
;   of changes to the guess of the root are too large to result
;   in a change of only 1/1000 in the guessed square

(define (new-sqrt number)
  (define (improve guess)
    (avg guess (/ number guess)))
  (define (good-enough? guess)
    (< (abs (- 1 (/ (square guess) number))) 1/1000))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 2))

; (new-sqrt (square 0.01)) -> 0.01000...
; (new-sqrt (square 10000)) -> 1000...

; Ex 1.8
(define (cube n)
  (* n n n))

(define (cube-root n)
  (define (improve guess)
    (/ (+ (/ n (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess)
    (< (abs (- 1 (/ (cube guess) n))) 1/1000))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.5))

; Black box procedures: lexical scope
; procedures bind their formal parameters as bound variables
; scope: set of expressions in which a binding holds
; free variable: not bound in current scope

; internal definitions and block structure

;;; 1.2 Procedures and the Processes they Generate
; Only by visualizing a process can we reliably design one
; Process shapes in time and space
(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

; describe compution in steps:
; product <- counter * product
; counter <- counter + 1
(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter) (+ counter 1))))
  (iter 1 1))

; recursive definitions and deferred operations (linear recursion)
; iterative definitions and explicit state (iterative recursion)
; tail recursion: recursive procedure with iterative process

; 1.9 The first is linear recursion, because the inc is returned
; with some computation left to do.
; The second is iterative: all state contained in the arguments to +

; 1.10 Ackerman function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 0 n) -> (* 2 n)
; (A 1 n) -> (expt 2 n)
; (A 2 n) ->

(define (n-squares-of-2 n)
  (if (< n 2)
      2
      (expt (n-squares-of-2 (- n 1)) 2)))

(define (A2N n)
  (expt 2 (n-squares-of-2 (- n 1))))

;;; !.2.2. Tree recursion
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

; shape: time proportional to number of nodes in tree,
;    space proportional to maximum depth of tree.

(define (fib-iter n)
  (local ((define (iter a b step)
             (if (= step n)
                 b
                 (iter b (+ a b) (+ step 1)))))
         (iter 0 1 1)))

(define us-coins '(50 25 10 5 1))

(define (ways-to-change amount coins)
  (cond ((< amount 0) 0)
        ((= amount 0) 1)
        ((null? coins) 0)
        (else (+ (ways-to-change (- amount (car coins))
                                 coins)
                 (ways-to-change amount
                                 (cdr coins))))))

; 1.11
(define (weird-f n)
  (if (< n 3)
      n
      (+ (weird-f (- n 1))
         (* 2 (weird-f (- n 2)))
         (* 3 (weird-f (- n 3))))))

(define (weird-f-iter n)
  (local ((define (iter a b c step)
            (if (= step n)
                c
                (iter b c (+ c (* 2 b) (* 3 a)) (+ step 1)))))
         (if (< n 3)
             n
             (iter 0 1 2 2))))

(define (pascal row col)
  (cond ((= row 1) 1)
        ((= col 1) 1)
        ((= row col) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))

; 1.13
; Fib(n) = round phi^n / (sqrt 5)
;  where phi = (1 + (sqrt 5)) / 2
;          u = (1 - (sqrt 5)) / 2
;
; base case:
;  Fib(1) = round phi / (sqrt 5) = round .7235 = 1
; Iductive case:
;  Fib(n) = Fib(n-1) + Fib(n-2)
;  Fib(n) = phi^n-1 + u^n-1 + phi^n-2 + u^n-2 / sqrt 5
;  Fib(n) = (phi + 1)*phi^n-2 + (u + 1)* u^n-2
;  Fib(n) = (phi^2)(phi^n-2) + (u^2)(u^n-2)
;  Fib(n) = phi^n + u^n / sqrt5

; 1.2.3 Orders of growth

; 1.14 Order of growth of make-change
; 1.15
(define (cube x) (* x x x))
(define times 0)
(define (p x)
  (set! times (+ times 1))
  (- (* 3 x) (* 4 (cube x))))
(define (p-times) times)
(define (p-times-0!) (set! times 0))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; a. applied five times
; b. log3 n

; 1.2.4 Exponentiation
; b ^ 0 = 1
; b ^ 1 = b
; b ^ n*2 = (square (b^n))
; b ^ n*2+1 = b * b^n
(define (expt n p)
  (cond ((eq? p 0) 1)
        ((eq? p 1) n)
        ((odd? p) (* n (expt n (- p 1))))
        (else (square (expt n (/ p 2))))))

; 1.16
(define (expt-iter n p)
  (local ((define (iter a n p)
            (cond ((eq? p 0) a)
                  ((odd? p) (iter (* n a)
                                  n
                                  (- p 1)))
                  (else (iter a (square n) (/ p 2))))))
         (iter 1 n p)))

; 1.17, 1.18
(define (double a)
  (* 2 a))
(define (halve a)
  (/ a 2))
(define (mult a b)
  (local ((define (iter a b c)
            (cond ((eq? a 0) c)
                  ((even? a) (iter (halve a) (double b) c))
                  (else (iter (- a 1) b (+ c b))))))
         (iter a b 0)))

; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 q p) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.2.6 Testing for primality
(define (divides? d n)
  (= 0 (remainder n d)))

(define (smallest-divisor n)
  (local ((define (iter test)
            (cond ((> (square test) n) n)
                  ((divides? test n) test)
                  (else (iter (+ test 1))))))
         (iter 2)))

; Simple prime test: O(root n)
(define (prime?-simple n)
  (= n (smallest-divisor n)))

; Fermat test: O(log n)
(define (exp-mod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (exp-mod base (/ exp 2) m)) m))
        (else
         (remainder (* base (exp-mod base (- exp 1) m))
                    m))))

(define (square-mod b m)
  (remainder (square (remainder b m)) m))

(define (times-mod a b m)
  (remainder (* (remainder a m) (remainder b m)) m))

; returns true if possibly prime, else false
(define (fermat-test n a)
  (= a (exp-mod a n n)))

(define (fermat-test-once n)
  (fermat-test n (+ 1 (random (- n 1)))))

(define (fermat-test-times n t)
  (cond ((= t 0) #t)
        ((fermat-test-once n) (fermat-test-times n (- t 1)))
        (else false)))

(define (prime?-fermat n)
  (fermat-test-times n (round (/ n 2))))
; Ex. 1.21 : 199, 1999, 7
; Ex. 1.22

(define prime? prime?-simple)
(define (search-for-primes min num)
  (cond ((= num 0) '())
        ((prime? min)
         (cons min
               (search-for-primes (+ min 2) (- num 1))))
        (else
         (search-for-primes (+ min 2) num))))

(define (exact-time f)
  (let* ((start (current-milliseconds))
         (value (f)))
    (print (- (current-milliseconds) start))
    (newline)
    value))

; The current speed of my computer makes it infeasible to test
; at any less than 10^11. At this level, we note the following
; millisecond times:
; 10^8  > 6ms
; 10^9  > 17ms
; 10^10 > 309ms
; 10^11 > 967ms
; For 8>9 and 10>11 we find a difference of roughly 3, close to
; (sqrt 10). Between 9 and 10, we note that:
;  (> (expt 10 9)  (expt 2 32)) > #f
;  (> (expt 10 10) (expt 2 32)) > #t
; So it seems that math operations on integers outside of the
; native system word length cost much more, undermining the
; idea of speed determined by pure mathematical O(n).

; Ex. 1.23
(define (smallest-divisor n)
  (local ((define (next-divisor n)
            (if (<= n 2)
                3
                (+ n 2)))
          (define (iter test)
            (cond ((> (square test) n) n)
                  ((divides? test n) test)
                  (else (iter (next-divisor test))))))
         (iter 2)))
; actually, the difference using this smallest-divisor
; seems to be about 1/2

; Ex. 1.24


(define prime? prime?-simple)
(define (search-for-primes min num)
  (cond ((= num 0) '())
        ((prime? min)
         (cons min
               (search-for-primes (+ min 2) (- num 1))))
        (else
         (search-for-primes (+ min 2) num))))

(define (exact-time f)
  (let* ((start (current-milliseconds))
         (value (f)))
    (print (- (current-milliseconds) start))
    (newline)
    value))

;; not finished: something wrong with the prime?-fast method

; Ex 1.25
; This is no longer o(log n) and adds extra time because we very
; quickly get out of machine-native integers and into bug nums.

; Ex 1.26
; By calling the recursive case twice for each step, he has turned
; the O(log n) process into O(n). A simple let expression would do

; Ex 1.27 Test Carmichael Numbers

(define (carmichael-number? n)
  (and (not (prime? n))
       (prime?-fermat n)))

(define (range->list min max)
  (build-list (+ (- max min) 1)
              (lambda (n) (+ n min))))

(define (carmichael-numbers-in-range min max)
  (filter carmichael-number?
          (range->list min max)))
; slighly more general than what is asked for...

; all values are non-false
(define (all vals)
  (foldl (lambda (v1 v2)
           (and v1 v2))
         #t
         vals))

(define (prime?-mr-test n)
  (local ((define (nontrivial-root? a n)
            (and (not (or (= a 1)
                          (= a (- n 1))))
                 (divides? n (- (square a) 1))))
          (define (exp-mod b e m)
            (cond ((= e 0) 1)
                  ((even? e)
                   (let ((res (remainder
                               (square (exp-mod b (/ e 2) m)) m)))
                     (if (nontrivial-root? res n)
                         0
                         res)))
                  (else
                   (remainder (* b (exp-mod b (- e 1) m)) m))))
          (define (test a)
            (= 1 (exp-mod a (- n 1) n))))
         (let ((tries (range->list 2 (round (+ 1 (/ n 2))))))
           (all (map test tries)))))

; results:
; > (carmichael-numbers-in-range 1 10000)
; (561 1105 1729 2465 2821 6601 8911)
; > (filter prime?-mr-test (carmichael-numbers-in-range 1 10000))
; ()

;;; 1.3 Higher-order procedures
(define (cube x) (* x x x))
(define (1+ x) (+ 1 x))
(define (identity x) x)

(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a) (sum f (next a) next b))))

(define (sum-cubes min max)
  (sum cube min 1+ max))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f
          (+ a (/ dx 2))
          add-dx
          b)
     dx))

; Ex 1.29
; > (simpsons-rule cube 0 1 6)
; 1/4
; very precise even with low n!
(define (simpsons-rule f a b n)
  (let* ((h (/ (- b a) n))
         (y (lambda (k)
              (f (+ a (* k h)))))
         (coeff (lambda (k)
                  (cond ((= k 0) 1)
                        ((= k n) 1)
                        ((odd? k) 4)
                        ((even? k) 2))))
         (term (lambda (k)
                 (* (y k) (coeff k)))))
    (* h 1/3 (apply + (map term (range->list 0 n))))))

; Ex 1.30 Iterative sum
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; Ex 1.31 Product procedure
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 1+ n))



(define (2+ n)
  (+ 2 n))

(define (pi-estimate terms)
  (let ((term (lambda (n)
                (if (even? n)
                    (/ n (1+ n))
                    (/ (1+ n) n)))))
    (* 4 (product term 2 1+ (+ terms 1)))))

; b: it's iterative, transformatin is simple

; Ex 1.32
; a.
(define (accumulate combiner null-value term a next b)
  (let ((iter (lambda (a res)
                (if (> a b)
                    res
                    (iter (next a)
                          (combiner (term a) res))))))
    (iter a null-value)))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))
(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

(define (filtered-accumulate filter combiner null-value term a next b)
  (letrec ((iter (lambda (a res)
                   (cond ((> a b) res)
                         ((filter a) (iter (next a)
                                           (combiner (term a) res)))
                         (else (iter (next a) res))))))
    (iter a null-value)))

(define (filter-sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))
(define (filter-prod filter term a next b)
  (filtered-accumulate filter * 1 term a next b))

(define (sum-prime-squares a b)
  (filter-sum prime? square a 1+ b))

; b.
(define (prod-relative-primes n)
  (filter-prod (lambda (i) (= (gcd i n) 1)) identity 1 1+ n))

; Ex 1.34
(define (f g)
  (g 2))
; (f f)
; (f 2)
; (2 2) - 2 is not a procedure so it cannot be applied

(define (close-enough? a b)
  (< (abs (- a b)) .01))
(define (average a b)
  (/ (+ a b) 2))

(define (half-interval-search-helper f a b)
  (let* ((midpoint (average a b))
         (midvalue (f midpoint)))
    (cond ((close-enough? a b) midpoint)

          ((= midvalue 0) midpoint)
          ((> midvalue 0) (half-interval-search f a midpoint))
          (else (half-interval-search f midpoint b)))))

(define (-/f f)
  (lambda (x)
    (- (f x))))

(define (half-interval-search f a b)
  (cond ((and (> (f a) 0)
              (< (f b) 0))
         (half-interval-search-helper (-/f f) a b))
        ((and (< (f a) 0)
              (> (f b) 0))
         (half-interval-search-helper f a b))
        (else (error "Values aren't opposite sign: " a b))))

; Ex 1.35
(define (fixed-point f guess)
  (letrec ((iter (lambda (guess)
                   (if (close-enough? guess)
                       guess
                       (iter (f guess)))))
           (close-enough? (lambda (guess)
                            (> .001 (abs (- guess (f guess)))))))
    (iter guess)))

(define phi-guess
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))



; Ex 1.36
(define (fixed-point-times f guess)
  (letrec ((iter (lambda (guess times)
                   (if (close-enough? guess)
                       (values guess times)
                       (iter (f guess) (+ times 1)))))
           (close-enough? (lambda (guess)
                            (> .001 (abs (- guess (f guess)))))))
    (iter guess 1)))

(define (fixed-point-damp-times f guess)
  (letrec ((iter (lambda (guess times)
                   (if (close-enough? guess)
                       (values guess times)
                       (iter (average guess (f guess)) (+ times 1)))))
           (close-enough? (lambda (guess)
                            (> .001 (abs (- guess (f guess)))))))
    (iter guess 1)))

; > (fixed-point-damp-times (lambda (x) (/ (log 1000) (log x))) 2)
; 4.55...
; 7
; without dampening, takes 23 times

; 1.37 a
(define (count-frac n d k)
  (letrec ((iter (lambda (i res)
                   (if (< i 1)
                       res
                       (iter (- i 1)
                             (/ (n i) (+ (d i) res)))))))
    (iter (- k 1) (/ (n k) (d k)))))

; 11 steps until 1.6180
; b: trivial

; Ex 1.38
(define (constant a)
  (lambda (i) a))
(define (estimate-e n)
  (+ 2 (count-frac (constant 1.0)
                   (lambda (k) (if (= (remainder k 3) 2)
                                   (expt 2 (+ 1 (/ (- k 2) 3)))
                                   1))
                   n)))

; Ex 1.39
(define (tan-cf x k)
  (letrec ((iter (lambda (i res)
                   (if (= i 1)
                       (/ x (- 1 res))
                       (iter (- i 1)
                             (/ (square x)
                                (- (* i 2) 1 res)))))))
    (iter (- k 1) k)))

; this is quite beautiful!
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (derivative f)
  (let ((dx 0.0001))
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx))))

(define (newtons-method g guess)
  (letrec ((Dg (derivative g))
           (f (lambda (x)
                (- x (/ (g x) (Dg x))))))
    (fixed-point f guess)))

; Ex 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

; Ex 1.41
(define (double a)
  (lambda (b)
    (a (a b))))

; Ex 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; Ex 1.43
; ((repeated f n) 3)
; ((repeated f n-1) (f 3))
(define (repeated f n)
  (lambda (x)
    (if (< n 1)
        x
        ((repeated f (- n 1)) (f x)))))

(define (repeated f n)
  (letrec ((iter (lambda (composed n)
                   (if (< n 1)
                       (lambda (x) (composed x))
                       (iter (compose f composed) (- n 1))))))
    (iter f (- n 1))))

; Ex 1.44
(define (average . xs)
  (/ (apply + xs) (length xs)))

(define (smooth f)
  (let ((dx 0.01))
    (lambda (x)
      (average (f (- x dx))
               (f x)
               (f (+ x dx))))))

(define (smooth-n f n)
  ((repeated smooth n) f))

; Ex 1.45
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (4th-root x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x (expt y 3))))
               1.0))

(define (18th-root x)
  (fixed-point ((repeated average-damp 4)
                (lambda (y) (/ x (expt y 17))))
               1.0))

; Ex 1.46
