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

;; Exercise 1.7
;;
;; Since we are using an absolute delta value (say d, which is 0.001 here) to
;; determine if a guess is a good enough square-root of a given number, say x,
;; it's possible that x is so small that the (absolute value of the) difference
;; between the square of guess and x is less than d even when guess is "way
;; off".  For example, the square-root of 0.000001 should be 0.001, but the
;; good-enough?  procedure above returns #t even for the guess of 0.01, which
;; is an order of magnitude off compared to the actual square-root.
;;
;; So we get wrong answers for very small numbers, but at least we do get
;; *some* answer.  For very large numbers on the other hand, the sqrt procedure
;; will get into an infinite recursion, and we will never get an answer back!
;; To understand why, we need some understanding of how floating point numbers
;; are represented in a computer.
;;
;; IEEE floating point numbers have three parts: 1. the sign; 2. the base, or
;; what is more formally called the "significand"; and 3. the exponent.  Couple
;; this with the fact that since we are using a finite amount of storage to
;; represent real numbers, *every* real number in any range can never be
;; represented exactly, because there are an infinite number of real numbers in
;; any range (of real numbers).  So when we compute with real, or
;; floating-point (FP), numbers, there are often "round off" errors, when the
;; result of a FP operation doesn't have an exact representation.
;;
;; Now consider that the exponent of very large numbers will be large.  These
;; large exponents, coupled with round-off errors will cause the good-enough?
;; test to fail when dealing with very large x's: the (absolute value of the)
;; difference between x and the square of guess will always be greater than the
;; threshold (0.001), so sqrt-iter will get into an infinite recursion.
;;
;; To work around these problems, we will need to change the way we check for a
;; "good enough" guess.  Here's one way suggested in the exercise:
(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.0001))

;; So instead of squaring guess and checking how much it differs from x, we get
;; an improved guess and see if the fractional change is within a threshold.
;; This does seem to improve the situation, both for very small and very large
;; numbers.  (At least it works better for the examples we cited above.)
;;
;; This implementation can be improved.  The way it is done now, we will need
;; to call the procedure `improve' twice in each iteration: once in the
;; good-enough? procedure, and again in sqrt-iter.  There is a simple way to
;; call it once and reuse its result when we learn about lexical scoping and
;; local variables in a following section.
;;
;; Note that even though this approach gives better answers, the answers are
;; still not "perfect".  For example, the square-root of 0.000001 is still not
;; exactly 0.001, though it's much closer to it compared to what we got
;; earlier.  Getting the best possible answers for such computations is what
;; the entire field of Numerical Analysis about.  We will not go down that path
;; here.

;; Exercise 1.8
;;
;; This should be straight-forward, very similar to the sqrt implementation,
;; with only a few details different.
(define (cbrt x)
  (define (square x)
    (* x x))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess)
    (< (/ (abs (- (improve guess) guess)) guess) 0.000001))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
	guess
	(cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

;; Oh, BTW: we have made use of block structure and internal/inner/embedded
;; definitions---introduced in the next section---for this implementation, in
;; case you were wondering why this code looks a little "strange".

;; Here's the square-root procedure again, reimplemented using block structure
;; and inner/embedded definitions, as given in the next section.  Note the this
;; doesn't include the improvement to good-enough? shown above.
(define (sqrt x)
  (define (square x)
    (* x x))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; A definition of the factorial function that creates a linear *recursive*
;; process:
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; And here's a modified version of the factorial function that creates a
;; linear *iterative* process:
(define (factorial n)
  ;; This implementation makes use of this inner auxiliary function so that its
  ;; execution gives rise to a linear iterative process.  This function returns
  ;; the product p when the counter c exceeds n.  Note that the computation of
  ;; the result---the factorial---is happening in the parameters/values passed
  ;; to the (recursive) calls to this function, not in its body!
  (define (fact-iter p c)
    (if (> c n)
	p
	(fact-iter (* p c) (+ c 1))))
  (fact-iter 1 1))

;; The advantage of a linear iterative process, over a linear recursive
;; process, is that most scheme implementations are able to execute them in
;; constant space, using tail-call optimization.

;; Exercise 1.9
;;
;; The first definition results in this sequence of calls when (+ 4 5) is
;; evaluated:
;;
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;;
;; This is clearly a linear recursive process.  On the other hand, the second
;; definition produces this sequence of calls:
;;
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;;
;; This is a linear iterative process.

;; Exercise 1.10
;;
;; This is the definition of Ackermann's function in Scheme, as given in the
;; book in the exercise:
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

;; It's trivial to simply evaluate the values of the given expressions at the
;; REPL (after evaluating the above definition in the REPL).  However, it's
;; more interesting to try and do these on paper and figure out the values
;; yourself.  Ackermann's function is a crazy (deep?) function in math.
;; Working out these values will give you some experience with it.
;;
;; Anyway, the values are:
;;
;; (A 1 10) => 1024 [(A 1 n) is basically 2^n]
;; (A 2 4) => 

;; Recursive and iterative computations of Fibonacci numbers, as given in
;; section 1.2.2.  First the (tree) recursive version:
(define (fib n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

;; Even though this works, it's very inefficient: its runtime grows
;; *exponentially* with n.  Here's a much better, (linear) iterative
;; implementation:
(define (fib n)
  (define (fib-iter a b i)
    (if (= i n)
	a
	(fib-iter (+ a b) a (+ i 1))))
  (fib-iter 0 1 0))

;; Here's the counting-change procedure, as given in the same section.
;; (Slightly modified to use block structure and inner definitions.)
(define (count-change amount)
  (define (denom k)
    ;; Return the k-th denomination in our set of denominations.
    (cond ((= k 1) 1)
	  ((= k 2) 5)
	  ((= k 3) 10)
	  ((= k 4) 25)
	  ((= k 5) 50)))
  (define (cc n k)
    ;; Return the number of ways we can produce change for the amount n using k
    ;; types of denominations.
    (cond ((= n 0) 1)
	  ((or (< n 0) (= k 0)) 0)
	  (else (+ (cc n (- k 1))
		   (cc (- n (denom k)) k)))))
  (cc amount 5))

;; When I initially saw this solution (in the book), I had difficulty
;; understanding the "first degenerate case: the number of ways to produce
;; change when the amount is exactly 0 is 1."  I thought it should be 0.  Do we
;; take this value to be 1 just to maintain some sort of mathematical
;; consistency or convenience?
;;
;; The answer seems to be: no, it's not just for mathematical/theoretical
;; reasons.  It's *required* to get the right answer, as you can verify for
;; simple cases like (count-change 2).  If we use the value 0 for that
;; (degenerate) case, we get the wrong answer (0), whereas with the value used
;; above (1), it gives us the right answer (1).  So it's some food for thought:
;; why is it correct to take the number of ways to produce change for the
;; amount 0 to be 1?  Does it make sense intuitively?

;; Exercise 1.11
;;
;; f(n) = 3 if n < 3
;;      = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3.
;;
;; Here's a way to compute f(n) using a recursive process:
(define (f-recursive n)
  (if (< n 3)
      n
      (+
       (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

;; And here's how we can implement this using a linear-iterative process:
(define (f-iterative n)
  (define (aux i-3 i-2 i-1 i)
    (cond ((< n 3) n)
	  ((> i n) i-1)
	  (else (aux i-2 i-1 (+ i-1 (* 2 i-2) (* 3 i-3)) (+ i 1)))))
  (aux 0 1 2 3))
