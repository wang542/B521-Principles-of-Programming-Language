#lang racket
(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
            (error 'empty-k "You can only invoke the empty continuation once")
            (begin (set! once-only #t) v))))))

(require racket/trace)
;Q1
;I rewrote the original functions which will call the cpsed version functions.
;And just renamed the actual original function to xxx-old
(define binary-to-decimal-old
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (v)
                                              (k (+ (car n)( * 2 v)))))])))
(define binary-to-decimal
  (lambda (n)
    (binary-to-decimal-cps n (empty-k))))

;Q2
(define times-old
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v)
                                  (k (* (car ls) v))))])))
(define times
  (lambda (ls)
    (times-cps ls (empty-k))))
;Q3
(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (lambda (v)
                                  (k (* (car ls) v))))])))
(define times-shortcut
  (lambda (ls)
    (times-cps-shortcut ls (empty-k))))

;Q4
(define plus-old
  (lambda (m)
    (lambda (n)
      (+ m n))))
(define plus-cps
  (lambda (m k)
    (k (lambda (n x)
           (x (+ m n))))))
(define plus
  (lambda (m)
    (lambda (n)
    (plus-cps m (lambda (m1) (m1 n (empty-k)))))))

;Q5
(define remv-first-9*-o
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9*-o (car ls)))
          (cons (car ls) (remv-first-9*-o (cdr ls)))]
         [else (cons (remv-first-9*-o (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9*-o (cdr ls)))])))

(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (cond
         [(remv-first-9*-cps (car ls) (lambda (d)
                                        (equal? (car ls) d )))
          (remv-first-9*-cps (cdr ls) (lambda (v)
                                        (k (cons (car ls) v))))]
         [else (remv-first-9*-cps (car ls) (lambda (v)
                                             (k (cons v (cdr ls)))))])]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (v)
                                          (k (cons (car ls) v))))])))
(define remv-first-9*
  (lambda (ls)
    (remv-first-9*-cps ls (empty-k))))

;Q6
(define cons-cell-count-old
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count-old (car ls)) (cons-cell-count-old (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (car ls)
                            (lambda (v)
                              (cons-cell-count-cps (cdr ls)
                                                   (lambda (d)
                                                     (k (add1 (+ v d)))))))]
      [else (k 0)])))
(define cons-cell-count
  (lambda (ls)
    (cons-cell-count-cps ls (empty-k))))


;Q7
(define find-old 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find-old (cdr pr) s) u))))
(define find-cps
  (lambda (u s k)
    (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s k) (k u)))))
(define find
  (lambda (u s)
    (find-cps u s (empty-k))))


;Q8
(define ack-old
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack-old (sub1 m) 1)]
      [else (ack-old (sub1 m)
                 (ack-old m (sub1 n)))])))
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v)
                                (ack-cps (sub1 m) v k)))])))
(define ack
  (lambda (m n)
    (ack-cps m n (empty-k))))


;Q9
;(define fib
 ; (lambda (n)
  ;  ((lambda (fib)
   ;    (fib fib n))
    ; (lambda (fib n)
     ;  (cond
;	 [(zero? n) 0]
;	 [(zero? (sub1 n)) 1]
;	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))
(define fib-cps
  (lambda (n k)
    ((lambda (fib k)
       (fib fib n k))
     (lambda (fib n k)
       (cond
         [(zero? n) (k 0)]
         [(zero? (sub1 n)) (k 1)]
         [else (fib fib (sub1 n) (lambda (v)
                                   (fib fib (sub1 (sub1 n)) (lambda (d)
                                                              (k (+ v d))))))]))
     (lambda (a) (k a)))))
(define fib
  (lambda (n)
    (fib-cps n (empty-k))))

;Q10
(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))
(define unfold-cps
  (lambda (p f g seed k)
    ((lambda (h k)
       (h h (lambda (h1) (h1 seed '() k))))
     (lambda (h k)
       (k(lambda (seed ans k)
         (p seed (lambda (p)
                   (if p
                       (k ans)
                       (h h (lambda (h2)
                       (g seed (lambda (g)
                                 (f seed (lambda (f)
                                           (h2 g (cons f ans) k)))))))))))))
     k)))
(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

;Q11
(define empty-s
  (lambda ()
    '()))
;(define unify
 ; (lambda (u v s)
  ;  (cond
   ;   ((eqv? u v) s)
    ;  ((number? u) (cons (cons u v) s))
     ; ((number? v) (unify v u s))
      ;((pair? u)
       ;(if (pair? v)
;	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
 ;            (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
;	   #f))
 ;     (else #f))))
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k (cons (cons u v) s)))
      ((number? v) (unify-cps v u s (lambda (v) (k v))))
      ((pair? u)
       (if (pair? v)
           (find-cps (car u) s (lambda (fcar)
                                 (find-cps (car v) s (lambda (fcar2)
                                                       (unify-cps fcar fcar2 s (lambda (d)
           (let ((s d))
             (if s (find-cps (cdr u) s (lambda (fcdr)
                                         (find-cps (cdr v) s (lambda (fcdr2)
                                                               (unify-cps fcdr fcdr2 s (lambda (v) (k v)))))))
                 #f))))))))
           #f))
      (else #f))))
;(trace unify-cps)
(define unify
  (lambda (u v s)
    (unify-cps u v s (empty-k))))



;Q12
(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))
(define M-cps
  (lambda (f k)
    (k(lambda (ls k)
      (cond
        ((null? ls) (k '()))
        (else (M-cps f (lambda (mf) (mf (cdr ls) (lambda (ml)
                                                    (f (car ls) (lambda (fcar)
                                                                  (k (cons fcar ml))))))))))))))
                                      
;(trace M-cps)

;Q13
(define use-of-M-cps
  (M-cps (lambda (n k) (k (add1 n))) (lambda (g) (g '(1 2 3 4 5) (empty-k)))))


;Brain Teaser
;Q14
(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))
;(trace strange)
(define strange-cps
  (lambda (x k)
    ((lambda (g k) (k(lambda (x k) (g g k))))
     (lambda (g k) (k(lambda (x k) (g g k))))
     k)))

;(trace strange-cps)
;Q15
(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))
;(define use-of-strange-cps-shortcut
;  (strange-cps 10 (lambda (s1) (s1 10 (empty-k)))))
(define use-of-strange-cps
  (strange-cps 5 (lambda (s1) (s1 6 (lambda (s2) (s2 7 (lambda (n)
                                                         (let ([strange^^ n])
    (strange^^ 8 (lambda (s4) (s4 9 (lambda (s5) (s5 10 (empty-k))))))))))))))


;Q16
(define why-old
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))
(define why-cps
  (lambda (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g (lambda (g) (g x k))))
          k))
     (lambda (g k)
       (f (lambda (x k) (g g (lambda (g) (g x k))))
          k))
    k)))
(define almost-length-cps
  (lambda (f k)
    (k(lambda (ls k)
      (if (null? ls) (k 0)
          (f (cdr ls) (lambda (fcdr) (k (add1 fcdr)))))
      ))))
;(trace why-cps)
;(trace almost-length-cps)
;tried to rewrite why and almost-length to call the cps version, doesn't seem to work
(define why
  (lambda (f)
    (why-cps f (lambda (d) (d f)))))
(define almost-length
  (lambda (f)
    (almost-length-cps f (lambda (v) v))))
;((why almost-length) '(a b c d e) (empty-k))
;(why-cps almost-length-cps (lambda (f)
;                               (f '(a b c d e) (empty-k))))