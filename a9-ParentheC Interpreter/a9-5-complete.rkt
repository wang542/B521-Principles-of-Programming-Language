#lang racket
(require "parenthec.rkt")
(require racket/trace)
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
  (lambda (env-env env-y env-k)
    (union-case env-env envri
      [(empty-env) (error "unbound identifier")]
      [(extend-env x val) (if (zero? env-y)
                              (let* ((v val)
                                     (k env-k))
                                 (apply-k k v))
                                 (let* ((env-y (sub1 env-y))
                                        (env-env x))
                                   (apply-env env-env env-y env-k)))])))
;closure
(define-union clos
  (closur body env))
(define apply-closure
  (lambda (closur_ clos-y clos-k)
    (union-case closur_ clos
      [(closur body env) (let* ((env (envri_extend-env env clos-y))
                                (exp body)
                                (k clos-k))
                           (value-of-cps exp env k))])))


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
  (lambda (k v)
    (union-case k ktniu
      [(empty-k) v]
      [(mult-innerk x1env^ k^) (let* ((v (* x1env^ v))
                                      (k k^))
                               (apply-k k v))]
      [(mult-outerk x2^ env^ k^) (let* ((k (ktniu_mult-innerk v k^))
                                      (exp x2^)
                                      (env env^))
                                 
                                   (value-of-cps exp env k))]
      [(sub1-outerk k^) (let* ((v (sub1 v))
                               (k k^))
                          (apply-k k v))]
      [(zero-outerk k^) (let* ((v (zero? v))
                               (k k^))
                          (apply-k k v))]
      [(conseq-innerk k^) (let* ((k k^))
                            (apply-k k v))]
      [(alt-innerk k^) (let* ((k k^))
                         (apply-k k v))]
      [(if-outerk conseq^ alt^ env^ k^)(if v
                                                (let* ((k (ktniu_conseq-innerk k^))
                                                       (exp conseq^)
                                                       (env env^))
                                                  (value-of-cps exp env k))
                                                (let* ((k (ktniu_alt-innerk k^))
                                                       (exp alt^)
                                                       (env env^))
                                                  (value-of-cps exp env k)))]
      [(throw-innerk kenv^) (let* ((k kenv^))
                              (apply-k k v))]
      [(throw-outerk v-exp^ env^) (let* ((k (ktniu_throw-innerk v))
                                         (exp v-exp^)
                                         (env env^))
                                    (value-of-cps exp env k))]
      [(let-outerk body^ env k^) (let* ((env (envri_extend-env env v))
                                        (k k^)
                                        (exp body^))
                                   (value-of-cps exp env k))]
      [(app-innerk ratorenv^ k^) (let* ((closur_ ratorenv^)
                                        (clos-y v)
                                        (clos-k k^))
                                        (apply-closure closur_ clos-y clos-k))]
      [(app-outerk rand^ env^ k^) (let* ((k (ktniu_app-innerk v k^))
                                         (exp rand^)
                                         (env env^))
                                    (value-of-cps exp env k))]
      ;[else (k v)]



      )))
    
(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
      [(const exp) (let* ((v exp))
                     (apply-k k v))]
      [(mult x1 x2) (let* ((k (ktniu_mult-outerk x2 env k))
                           (exp x1))
                      (value-of-cps exp env k))]
      [(sub1 x) (let* ((k (ktniu_sub1-outerk k))
                       (exp x))
                  (value-of-cps exp env k))]
      [(zero x) (let* ((k (ktniu_zero-outerk k))
                       (exp x))
                          (value-of-cps exp env k))]
      [(if test conseq alt) (let* ((k (ktniu_if-outerk conseq alt env k))
                                   (exp test))
                                      (value-of-cps exp env k))]
      [(letcc body) (let* ((env (envri_extend-env env k))
                           (exp body))
                                (value-of-cps exp env k))]
      [(throw k-exp v-exp) (let* ((k (ktniu_throw-outerk v-exp env))
                                  (exp k-exp))
                                     (value-of-cps exp env k))]
      [(let e body) (let* ((k (ktniu_let-outerk body env k))
                           (exp e))
                      (value-of-cps exp env k))]
      [(var expr) (let* ((env-env env)
                         (env-y expr)
                         (env-k k))
                    (apply-env env-env env-y env-k))]
      [(lambda body) (let* ((v (clos_closur body env)))
                       (apply-k k v))]
      [(app rator rand) (let* ((k (ktniu_app-outerk rand env k))
                               (exp rator))
                                  (value-of-cps exp env k))]
      )))
(trace apply-env)
;(trace apply-closure)
;(trace apply-k)
;(trace value-of-cps)
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