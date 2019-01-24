#lang racket
(require "parenthec.rkt")
(require racket/trace)
(define-registers
  env-env
  env-y
  env-k
  closur_
  clos-y
  clos-k
  appk
  v
  exp
  env
  k)
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
(define apply-env
  (lambda ();env-env env-y env-k
    (union-case env-env envri
      [(empty-env) (error 'env "unbound identifier")]
      [(extend-env x val) (if (zero? env-y)
                              (begin
                                (set! v val)
                                (set! appk env-k)
                                (apply-k))
                                 (begin
                                   (set! env-y (sub1 env-y))
                                   (set! env-env x)
                                   (apply-env)))])))
(trace apply-env)

;closure
(define-union clos
  (closur body env))
(define apply-closure
  (lambda ();closur_ clos-y clos-k
    (union-case closur_ clos
      [(closur body env^) (begin
                           (set! env (envri_extend-env env^ clos-y))
                           (set! exp body)
                           (set! k clos-k)
                           (value-of-cps))])))


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


(define apply-k
  (lambda ();appk v
    (union-case appk ktniu
      [(empty-k) v]
      [(mult-innerk x1env^ k^) (begin
                                 (set! v (* x1env^ v))
                                 (set! appk k^)
                               (apply-k))]
      [(mult-outerk x2^ env^ k^) (begin
                                   (set! k (ktniu_mult-innerk v k^))
                                   (set! exp x2^)
                                   (set! env env^)
                                 
                                   (value-of-cps))]
      [(sub1-outerk k^) (begin
                          (set! v (sub1 v))
                          (set! appk k^)
                          (apply-k))]
      [(zero-outerk k^) (begin
                          (set! v (zero? v))
                          (set! appk k^)
                          (apply-k))]
      [(conseq-innerk k^) (begin
                            (set! appk k^)
                            (apply-k))]
      [(alt-innerk k^) (begin
                         (set! appk k^)
                         (apply-k))]
      [(if-outerk conseq^ alt^ env^ k^)(if v
                                                (begin
                                                  (set! k (ktniu_conseq-innerk k^))
                                                  (set! exp conseq^)
                                                  (set! env env^)
                                                  (value-of-cps))
                                                (begin
                                                  (set! k (ktniu_alt-innerk k^))
                                                  (set! exp alt^)
                                                  (set! env env^)
                                                  (value-of-cps)))]
      [(throw-innerk kenv^) (begin
                              (set! appk kenv^)
                              (apply-k))]
      [(throw-outerk v-exp^ env^) (begin
                                    (set! k (ktniu_throw-innerk v))
                                    (set! exp v-exp^)
                                    (set! env env^)
                                    (value-of-cps))]
      [(let-outerk body^ env^ k^) (begin
                                   (set! env (envri_extend-env env^ v))
                                   (set! k k^)
                                   (set! exp body^)
                                   (value-of-cps))]
      [(app-innerk ratorenv^ k^) (begin
                                   (set! closur_ ratorenv^)
                                   (set! clos-y v)
                                   (set! clos-k k^)
                                   (apply-closure))]
      [(app-outerk rand^ env^ k^) (begin
                                    (set! k (ktniu_app-innerk v k^))
                                    (set! exp rand^)
                                    (set! env env^)
                                    (value-of-cps))]
      ;[else (k v)]



      )))
(trace apply-k)
(define value-of-cps
  (lambda ();exp env k
    (union-case exp expr
      [(const exp) (begin
                     (set! v exp)
                     (set! appk k)
                     (apply-k))]
      [(mult x1 x2) (begin
                      (set! k (ktniu_mult-outerk x2 env k))
                      (set! exp x1)
                      (value-of-cps))]
      [(sub1 x) (begin
                  (set! k (ktniu_sub1-outerk k))
                  (set! exp x)
                  (value-of-cps))]
      [(zero x) (begin
                  (set! k (ktniu_zero-outerk k))
                  (set! exp x)
                  (value-of-cps))]
      [(if test conseq alt) (begin
                              (set! k (ktniu_if-outerk conseq alt env k))
                              (set! exp test)
                              (value-of-cps))]
      [(letcc body) (begin
                      (set! env (envri_extend-env env k))
                      (set! exp body)
                      (value-of-cps))]
      [(throw k-exp v-exp) (begin
                             (set! k (ktniu_throw-outerk v-exp env))
                             (set! exp k-exp)
                             (value-of-cps))]
      [(let e body) (begin
                      (set! k (ktniu_let-outerk body env k))
                      (set! exp e)
                      (value-of-cps))]
      [(var expr) (begin
                    (set! env-env env)
                    (set! env-y expr)
                    (set! env-k k)
                    (apply-env))]
      [(lambda body) (begin
                       (set! v (clos_closur body env))
                       (set! appk k)
                       (apply-k))]
      [(app rator rand) (begin
                          (set! k (ktniu_app-outerk rand env k))
                          (set! exp rator)
                          (value-of-cps))]
      )))
;(trace apply-env)
;(trace apply-closure)
;(trace apply-k)
;(trace value-of-cps)
(define main 
  (lambda ()
    (begin
      (set! exp
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
       (expr_const 5))))
     (set! env (envri_empty-env))
     (set! k (ktniu_empty-k))
     (value-of-cps))))