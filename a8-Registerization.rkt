#lang racket
(require racket/trace)
;Ack
(define m* #f)
(define n* #f)
(define k* #f)
(define v* #f)
(define empty-k-ack
  (lambda ()
    `(empty-k)))
(define make-ack-k
  (lambda (m k)
    `(make-ack-k ,m ,k)))
(define apply-k-ack
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(make-ack-k ,m ,k) (begin
                             [set! m* (sub1 m)]
                             [set! n* v*]
                             [set! k* k]
                             (ack))])))
(define ack
  (lambda ()
    (cond
      [(zero? m*) (begin
                    [set! v* (add1 n*)]
                    (apply-k-ack))]
      [(zero? n*) (begin
                    [set! m* (sub1 m*)]
                    [set! n* 1]
                    (ack))]
      [else (begin
              [set! k* (make-ack-k m* k*)]
              [set! n* (sub1 n*)]
              (ack))])))

(define ack-reg-driver
  (lambda (m n)
    (begin
      [set! m* m]
      (set! n* n)
      (set! k* (empty-k-ack))
      (ack))))



;Depth
(define ls* #f)
(define k-d* #f)
(define v-d* #f)
(define empty-k-depth
  (lambda ()
    `(empty-k-depth)))
(define depth-inner-k
  (lambda (l k)
    `(depth-inner-k ,l ,k)))
(define depth-outer-k
  (lambda (ls k)
    `(depth-outer-k ,ls ,k)))
(define apply-k-depth
  (lambda ()
    (match k-d*
      [`(empty-k-depth) v-d*]
      [`(depth-inner-k ,l ,k) (if (< (add1 l) v-d*)
                                  (begin
                                    [set! k-d* k]
                                    (apply-k-depth))
                                  (begin
                                    [set! v-d* (add1 l)]
                                    [set! k-d* k]
                                    (apply-k-depth)))]
      [`(depth-outer-k ,ls ,k) (begin
                                 [set! k-d* (depth-inner-k v-d* k)]
                                 [set! ls* (cdr ls)]
                                 (depth))]
      )))
(define depth
  (lambda ()
    (cond
      [(null? ls*) (begin
                     [set! v-d* 1]
                     (apply-k-depth))]
      [(pair? (car ls*)) (begin
                           [set! k-d* (depth-outer-k ls* k-d*)]
                           [set! ls* (car ls*)]
                           (depth))]
      [else (begin
              [set! ls* (cdr ls*)]
              (depth))])))
(define depth-reg-driver
 (lambda (ls)
   (begin
     (set! ls* ls)
     (set! k-d* (empty-k-depth))
     (depth))))




;Factorial
(define empty-k-fact
  (lambda ()
    `(empty-k)))


(define apply-k-fact
  (lambda () ;k v
    (match apply-k-fact-k
      [`(empty-k) apply-k-fact-v]
      [`(make-fact-k ,n ,k) (begin
                              [set! apply-k-fact-k k]
                              [set! apply-k-fact-v (* n apply-k-fact-v)]
                              (apply-k-fact))]
      )))
;(apply-k k (* n v))
(define make-fact-k
  (lambda (n k)
    `(make-fact-k ,n ,k)))
    ;(lambda (v) (apply-k k (* n v)))))
(define fact-n #f)
(define fact-k #f)
;(define v* #f)
(define fact-function #f)
(define fact-lambda1-fact #f)
(define fact-lambda1-k #f)
(define fact-lambda2-fact #f)
(define fact-lambda2-n #f)
(define fact-lambda2-k #f)
(define apply-k-fact-k #f)
(define apply-k-fact-v #f)

(define fact
  (lambda () ;n k
    (set! fact-lambda1-k fact-k)
    (set! fact-lambda1-fact
       (lambda () ;fact n k
       (cond
         [(zero? fact-lambda2-n) (begin
                       [set! apply-k-fact-v 1]
                       [set! apply-k-fact-k fact-lambda2-k]
                       (apply-k-fact))]
         [else (begin
                            [set! fact-lambda2-k (make-fact-k fact-lambda2-n fact-lambda2-k)]
                            [set! fact-lambda2-n (sub1 fact-lambda2-n)]
                            (fact-lambda2-fact))])))

    ((lambda () ;fact k
       (set! fact-lambda2-k fact-lambda1-k)
        (set! fact-lambda2-n fact-n)
        (set! fact-lambda2-fact fact-lambda1-fact)
        (fact-lambda1-fact)))))
     #|(lambda () ;fact n k
       (cond
         [(zero? (fact-lambda2-n) (begin
                       [set! apply-k-fact-v 1]
                       (apply-k-fact)))]
         [else ((begin
                            [set! fact-lambda2-k (make-fact-k fact-lambda2-n fact-lambda2-k)]
                            [set! fact-lambda2-n (sub1 fact-lambda2-n)]
                            (fact-lambda2-fact)))]))|#
       
;(apply-k k* 1)
;(make-fact-k n* k*)
;(sub1 n*)
(define fact-reg-driver
  (lambda (n)
  (begin
    [set! fact-n n]
    [set! fact-k (empty-k-fact)]
    (fact))
   ))


;Pascal
(define apply-k-pascal-k #f)
(define apply-k-pascal-v #f)
(define pascal-m #f)
(define pascal-a #f)
(define pascal-n #f)
(define pascal-pascal #f)

(define pascal
  (lambda () ; n k
    (begin
      (set! pascal
           (lambda ();pascal k
             (begin
               [set! apply-k-pascal-v (lambda ()
		  (cond
		    [(> pascal-m pascal-n) (begin
                                               [set! apply-k-pascal-v '()]
                                               (apply-k-pascal))]
		    [else
			    (begin
                              [set! apply-k-pascal-k (pascal-outer pascal-m pascal-a apply-k-pascal-k)]
                              (pascal))]))]
               (apply-k-pascal))))
      [set! apply-k-pascal-k (pascal-f apply-k-pascal-k)]
       (pascal))))
;(trace pascal)

;(apply-k-pascal k '())
;(pascal-outer m a k)
;(pascal-f k)
(define apply-k-pascal
  (lambda ()
    (match apply-k-pascal-k
      [`(empty-k-pascal) apply-k-pascal-v]
      [`(pascal-inner ,m ,a ,k)(begin
                                  [set! apply-k-pascal-k k]
                                  [set! apply-k-pascal-v (cons (+ a m) apply-k-pascal-v)]
                                  (apply-k-pascal))]
      [`(pascal-outer ,m ,a ,k) (begin
                                   [set! apply-k-pascal-k (pascal-inner m a k)]
                                   [set! pascal-m (add1 m)]
                                   [set! pascal-a (+ a m)]
                                   (apply-k-pascal-v))]
      [`(pascal-f ,k) (begin
                         [set! apply-k-pascal-k k]
                         [set! pascal-m 1]
                         [set! pascal-a 0]
                         (apply-k-pascal-v))]
      )))
;(trace apply-k-pascal)
;(apply-k-pascal k (cons (+ a m) v))
;(v (add1 m) (+ a m) (pascal-inner a m k))
;(v 1 0 k)
(define empty-k-pascal
  (lambda ()
    `(empty-k-pascal)))
(define pascal-inner
  (lambda (m a k)
    `(pascal-inner ,m ,a ,k)))
(define pascal-outer
  (lambda (m a k)
    `(pascal-outer ,m ,a ,k)))
    ;(lambda (f) (f (add1 m) (+ a m) (pascal-inner a m k)))))
(define pascal-f
  (lambda (k)
    `(pascal-f ,k)))
    ;(lambda (f) (f 1 0 k))))
(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! pascal-n n)
      (set! apply-k-pascal-k (empty-k-pascal))
      (pascal))))






;Brainteaser-Incomplete, only trampolinized fib, rampoline incomplete
(define n-fib #f)
(define k-fib #f)
(define v-fib #f)
(define pc-fib #f)
(define done-fib #f)
(define empty-k-fib
  (lambda()
    `(empty-k-fib)))
(define fib-inner-k
  (lambda (n1 k)
    `(fib-inner-k ,n1 ,k)))
(define fib-outer-k
  (lambda (n k)
    `(fib-outer-k ,n ,k)))
(define apply-k-fib
  (lambda ()
    (match k-fib
      [`(empty-k-fib) (set! done-fib #t)]
      [`(fib-inner-k ,n1 ,k) (begin
                               [set! k-fib k]
                               [set! v-fib (+ n1 v-fib)]
                               (set! pc-fib apply-k-fib))]
      [`(fib-outer-k ,n ,k)  (begin
                               [set! k-fib (fib-inner-k v-fib k)]
                               [set! n-fib (sub1 (sub1 n))]
                               (set! pc-fib fib))]
      )))
(define fib
  (lambda ()
    (cond
      [(and (not (negative? n-fib)) (< n-fib 2)) (begin
                                                   [set! v-fib n-fib]
                                                   [set! pc-fib apply-k-fib])]
      [else (begin
              [set! k-fib (fib-outer-k n-fib k-fib)]
              [set! n-fib (sub1 n-fib)]
              (set! pc-fib fib))]
       )))
(define trampoline-fib
  (lambda ()
    (if done-fib
        v-fib
    (begin (pc-fib) (trampoline-fib)))))
(define fib-driver
  (lambda (n)
    (begin
      (set! n-fib n)
      (set! k-fib (empty-k-fib))
      (set! done-fib #f)
      (set! pc-fib fib)
      (trampoline-fib))))
#|
(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (let/cc jumpout
      (rampoline
        (lambda ()
          (fib n1 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n2 (ramp-empty-k jumpout)))
        (lambda ()
          (fib n3 (ramp-empty-k jumpout)))))))|#