#lang racket
(require racket/trace)
(require rackunit (for-syntax syntax/parse))
(define filter
  (lambda (p ls)
    (cond
      [(null? ls) '()]
      [(p (car ls)) (cons (car ls) (filter p (cdr ls)))]
      [else (filter p (cdr ls))])))

(define filter-sps
  (lambda (f ls store)
    (cond
      [(null? ls) (values '() store)]
      [(f (car ls)) (let-values ([(pass fail) (filter-sps f (cdr ls) store)])
                                 (values (cons (car ls) pass) fail))]
      [else (filter-sps f (cdr ls) (cons (car ls) store))])))
;(trace filter-sps)

(define filter*-sps
  (lambda (f ls store)
    (cond
      [(null? ls) (values '() store)]
      [(pair? (car ls)) (let-values ([(pass-car fail-car) (filter*-sps f (car ls) store)])
                          (let-values ([(pass-cdr fail-cdr) (filter*-sps f (cdr ls) store)])
                            (values (cons pass-car pass-cdr) (cons fail-car fail-cdr))))]
      [(f (car ls)) (let-values ([(pass fail) (filter*-sps f (cdr ls) store)])
                                 (values (cons (car ls) pass) fail))]
      [else (let-values ([(pass fail) (filter*-sps f (cdr ls) store)])
                                 (values pass (cons (car ls) fail)))])))
;(trace filter*-sps)

(define sub2
  (compose sub1 sub1))
(define fib-sps
  (lambda (n store)
    (cond
      [(assv n store) => (lambda (pr) (values (cdr pr) store))]
      [(eqv? 0 n) (values 0 (cons (cons 0 0) store))]
      [(eqv? 1 n) (values 1 (cons (cons 1 1) store))]
      [else (let-values ([(fibsub1 fibsub1-store) (fib-sps (sub1 n) store)])
              (let-values ([(fibsub2 fibsub2store) (fib-sps (sub2 n) fibsub1-store)])
                (let ([answer (+ fibsub1 fibsub2)])
                  (values answer (cons (cons n answer) fibsub2store)))))])))
;(trace fib-sps)

(define-syntax and*
  (syntax-rules ()
    ((and*) #t)
    ((and* e1) e1)
    ((and* e1 e2 ...) (if e1 (and* e2 ...) #f))))
(define-syntax list*
  (syntax-rules ()
    ((list* e1) e1)
    ((list* e1 e2 ...)
     (cons e1 (list* e2 ...)))))
(define-syntax macro-list
  (syntax-rules ()
    ((macro-list) '())
    ((macro-list e1 e2 ...)
     (cons e1 (macro-list e2 ...)))))

(define-syntax mcond
  (syntax-rules(else)
    ((mcond (else v3)) v3)
    ((mcond (e4 v4)) (if e4 v4 (void)))
    ((mcond (e1 v1) (e2 v2) ...) (if e1 v1 (mcond (e2 v2) ...)))))
    
(define-syntax copy-code
    (syntax-rules ()
      [(_ x) `(,x x)]))
(define-syntax quote-quote
    (syntax-rules ()
      [(_ e) (quote (quote e))]))
(define-syntax macro-map
  (syntax-rules()
    ((macro-map procedure '()) '()) 
    ((macro-map procedure '(e1 e2 ...)) (list(procedure e1) (procedure e2) ...))))