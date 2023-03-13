;; Exercise 1.1: Do this interactively at a scheme REPL.

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (foo a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
	((and (< b a) (< b c)) (sum-of-squares a c))
	(else (sum-of-squares a b))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; The procedure will do what its name suggests: it will always compute the sum
;; of a and the absolute value of b.  The "operator" of its compound expression
;; is a conditional, which will select the appropriate operator to use (+ or -)
;; to evaluate the final expression.

;; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(test 0 (p))
;; If the interpreter uses applicative-order evaluation, the last call to
;; `test' will return 0 without evaluating the call to `p'.  Whereas a
;; normal-order evaluation based interpreter will get stuck in an infinite
;; recursion/expansion expanding the recursively defined `p'.
;;
;; Even though the `if' special form is evaluated the same way in both
;; interpreters, the one using normal-order evaluation will never get the
;; chance to start evaluating the `if': it needs to finish expanding the body
;; of `test' to atoms first before it can start evaluating it.

;; This is the square-root procedure from SICP 1.1.7
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))
;; Here too the interpreter will get into an infinite recursion.  The `if'
;; special form *ensures* that only *one* of the consequent/alternate
;; expressions is evaluated, and *only once*.  Whereas the function `new-if'
;; evaluates all its parameters out of necessity: that's the evaluation rule
;; for functions.  So using this `new-if' in `sqrt-iter' causes the recursive
;; call to be made for ever.  The `if' special form would make sure that the
;; recursive call doesn't happen if `good-enough?' returned true, thereby
;; avoiding the infinite recursion.
