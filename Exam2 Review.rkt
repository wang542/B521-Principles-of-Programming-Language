#lang racket
(require racket/trace)
#|
(lambda (b)
  ((lambda (c)
     ((c )
      (b b)))
   ((lambda (d) b)
      (lambda (b) b))))|#

(define member-cps
  (lambda (x ls k)
    (cond
      ((null? ls) (k #f))
      ((eqv? x (car ls)) (k ls))
      (else (member-cps x (cdr ls) k)))))
;(member-cps 'x '(a b x c d e x) (lambda (v) v))

(define member-cps-cps
  (lambda (x ls c-cps k)
    (cond
      ((null? ls) (c-cps #f k))
      ((eqv? x (car ls)) (c-cps ls k))
      (else (member-cps-cps x (cdr ls) c-cps k)))))
(member-cps-cps 'x '(a b c x d e x) (lambda (v g) (g v)) (lambda (k) k))
#|(lambda (e)
  ((lambda (f)
     (lambda (g) f))
   ((lambda (g) e)
    (e
     (lambda (g)
       ((lambda (e)
          (lambda (h) g))
        e))))))|#
(define empty-k
  (lambda ()
    (lambda (k)
      k)))
#|
(define stutter-if-cps
  (lambda (f-cps y k)
    (cond
      ((null? y) (k '()))
      (else
       
       (cond
         (f-cps (car y) (lambda (z)
                        (stutter-if-cps f-cps (cdr y) (lambda (e)
         
         (k(cons z (cons z e)))))))
             (else (stutter-if-cps f-cps (cdr y) (lambda (q) (k(cons (car y) q))))))))))|#

(define stutter-if-cps
  (lambda (f-cps y k)
    (cond
      ((null? y) (k '()))
      (else
       (let ((z (f-cps (car y))))
         (cond
           (z (stutter-if-cps f-cps (cdr y) (lambda (v) (k (cons z (cons z v))))))
           (else (stutter-if-cps f-cps (cdr y) (lambda (d) (k (cons (car y) d)))))))))))
(trace stutter-if-cps)
(stutter-if-cps (lambda (x) (memv x '(2 4 8))) '(1 2 3 4) (lambda (v) v))
(define stutter-if-cps2
  (lambda (f-cps y k)
    (cond
      ((null? y) (k '()))
      (else
       (f-cps (car y) (lambda (e)
         (cond
           (e (stutter-if-cps2 f-cps (cdr y) (lambda (v) (k (cons e (cons e v))))))
           (else (stutter-if-cps2 f-cps (cdr y) (lambda (d) (k (cons (car y) d))))))))))))
(trace stutter-if-cps2)
(stutter-if-cps2 (lambda (x k) (k (memv x '(2 4 8)))) '(1 2 3 4) (lambda (v) v))
(define stutter-if
  (lambda (f y)
    (cond
      ((null? y) '())
      (else
       (let ((z (f (car y))))
         (cond
           (z (cons z (cons z (stutter-if f (cdr y)))))
           (else (cons (car y) (stutter-if f (cdr y))))))))))
(trace stutter-if)
;(stutter-if (lambda (x) (memv x '(2 4 8))) '(1 2 3 4))

(define valof
  (lambda (e env)
    (match e
      [`,n #:when (number? n) n]
      [`,x #:when (symbol? x) (env x)]
      [`(let ([,id ,e]) ,body)
       (let ([a (valof e env)])
         (valof body (lambda (y)
                      (if (eq? y id)
                          a
                          (env y)))))]
      [`(lambda (,id) ,body)
       (lambda (a env) (lambda (y)
                         (if (eq? y id)
                             a
                             (env y))))]
      [`(+ ,e1 ,e2) (+ (valof e1 env)
                       (valof e2 env))]
      [`(,rator ,rand)
       ((valof rator env)
        (valof rand env)
        env)])))
(trace valof)
(valof
 `(let ([g (lambda (y) (f (+ 6 y)))])
    (let ([f (lambda (z) (+ 7 z))])
      (g 5)))
      (lambda (y) (error "blah" y)))

(define fib-cps-thunked
  (lambda (n k)
    (lambda ()
      (cond
        [(< n 2) (k n)]
        [else
         (fib-cps-thunked
          (sub1 n)
          (lambda (v)
            (fib-cps-thunked
             (- n 2)
             (lambda (w)
               (k (+ v w))))))]))))
(define trampoline
  (lambda (th) (trampoline (th))))
;(call/cc (trampoline (fib-cps-thunked 6 (lambda (k) k))))
(call/cc (lambda (k) (trampoline (fib-cps-thunked 6 k))))
;((call/cc (lambda (v) (trampoline (fib-cps-thunked v k))))6)
;(trampoline (call/cc (lambda (k) (fib-cps-thunked 6 k))))