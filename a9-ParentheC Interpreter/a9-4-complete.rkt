#lang racket
(require "parenthec.rkt")
(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))
;environment
(define-union envri
  (empty-env)
  (extend-env x val))
;(define empty-env
 ; (lambda ()
  ;  `(empty-env)))
;(define extend-env
 ; (lambda (x val)
  ; `(extend-env ,x ,val)))
(define apply-env
  (lambda (env y k)
    (union-case env envri
      [(empty-env) (error "unbound identifier")]
      [(extend-env x val) (if (zero? y)
                                 (apply-k k val)
                                 (apply-env x (sub1 y) k))])))
;closure
(define-union clos
  (closur body env))
;(define make-closure
 ; (lambda (body env)
  ;  `(make-closure ,body ,env)))
(define apply-closure
  (lambda (closur_ y k^)
    (union-case closur_ clos
      [(closur body env) (value-of-cps body (envri_extend-env env y) k^)])))


;continuation
(define-union ktniu
  (empty-k)
  (mult-innerk x1env^ k^)
  (mult-outerk x2^ env^ k^)
  (sub1-outerk k^)
  (zero-outerk k^)
  (conseq-innerk k^)
  (alt-innerk k^)
  (if-outerk conseq^ alt^ env^ k^)
  (throw-innerk kenv^)
  (throw-outerk v-exp^ env^)
  (let-outerk body env^ k^)
  (app-innerk ratorenv^ k^)
  (app-outerk rand^ env^ k^))

#|
(define empty-k
  (lambda ()
    `(empty-k)))
(define mult-innerk
  (lambda (x1env^ k^)
    `(mult-innerk ,x1env^ ,k^)))
(define mult-outerk
  (lambda (x2^ env^ k^)
    `(mult-outerk ,x2^ ,env^ ,k^)))
(define sub1-outerk
  (lambda (k^)
    `(sub1-outerk ,k^)))
(define zero-outerk
  (lambda (k^)
    `(zero-outerk ,k^)))
(define conseq-innerk
  (lambda (k^)
    `(conseq-innerk ,k^)))
(define alt-innerk
  (lambda (k^)
    `(alt-innerk ,k^)))
(define if-outerk
  (lambda (conseq^ alt^ env^ k^)
    `(if-outerk ,conseq^ ,alt^ ,env^ ,k^)))
(define throw-innerk
  (lambda (kenv^)
    `(throw-innerk ,kenv^)))
(define throw-outerk
  (lambda (v-exp^ env^)
    `(throw-outerk ,v-exp^ ,env^)))
(define let-outerk
  (lambda (body env^ k^)
    `(let-outerk ,body ,env^ ,k^)))
(define app-innerk
  (lambda (ratorenv^ k^)
    `(app-innerk ,ratorenv^ ,k^)))
(define app-outerk
  (lambda (rand^ env^ k^)
    `(app-outerk ,rand^ ,env^ ,k^)))|#
(define apply-k
  (lambda (k v)
    (union-case k ktniu
      [(empty-k) v]
      [(mult-innerk x1env^ k^) (apply-k k^ (* x1env^ v))]
      [(mult-outerk x2^ env^ k^) (value-of-cps x2^ env^ (ktniu_mult-innerk v k^))]
      [(sub1-outerk k^) (apply-k k^ (sub1 v))]
      [(zero-outerk k^) (apply-k k^ (zero? v))]
      [(conseq-innerk k^) (apply-k k^ v)]
      [(alt-innerk k^) (apply-k k^ v)]
      [(if-outerk conseq^ alt^ env^ k^)(if v
                                                (value-of-cps conseq^ env^ (ktniu_conseq-innerk k^))
                                                (value-of-cps alt^ env^ (ktniu_alt-innerk k^)))]
      [(throw-innerk kenv^) (apply-k kenv^ v)]
      [(throw-outerk v-exp^ env^) (value-of-cps v-exp^ env^ (ktniu_throw-innerk v))]
      [(let-outerk body^ env k^) (value-of-cps body^ (envri_extend-env env v) k^)]
      [(app-innerk ratorenv^ k^) (apply-closure ratorenv^ v k^)]
      [(app-outerk rand^ env^ k^) (value-of-cps rand^ env^ (ktniu_app-innerk v k^))]
      ;[else (k v)]



      )))
    
(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
      [(const exp) (apply-k k exp)]
      [(mult x1 x2) (value-of-cps x1 env (ktniu_mult-outerk x2 env k))]
      [(sub1 x) (value-of-cps x env (ktniu_sub1-outerk k))]
      [(zero x) (value-of-cps x env (ktniu_zero-outerk k))]
      [(if test conseq alt) (value-of-cps test env (ktniu_if-outerk conseq alt env k))]
      [(letcc body) (value-of-cps body (envri_extend-env env k) k)]
      [(throw k-exp v-exp) (value-of-cps k-exp env (ktniu_throw-outerk v-exp env))]
      [(let e body) (value-of-cps e env (ktniu_let-outerk body env k))]
      [(var expr) (apply-env env expr k)]
      [(lambda body) (apply-k k (clos_closur body env))]
      [(app rator rand) (value-of-cps rator env (ktniu_app-outerk rand env k))]
      )))

(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (envri_empty-env)
     (ktniu_empty-k))))