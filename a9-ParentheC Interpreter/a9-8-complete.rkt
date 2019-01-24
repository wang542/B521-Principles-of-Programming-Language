#lang racket
(require "parenthec.rkt")
(require racket/trace)
(define-registers
  env
  env-y
  k
  closur_
  clos-y
  v
  exp)
(define-program-counter pc)
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
(define-label apply-env
;env env-y k
    (union-case env envri
      [(empty-env) (error 'value-of-cps "unbound variable")]
      [(extend-env x val) (if (zero? env-y)
                              (begin
                                (set! v val)
                                     ;(k k))
                                 (apply-k))
                                 (begin
                                   (set! env-y (sub1 env-y))
                                   (set! env x)
                                   (set! pc apply-env)))]))
;closure
(define-union clos
  (closur body env))
(define-label apply-closure
  ;closur_ clos-y  k
    (union-case closur_ clos
      [(closur body env^) (begin
                           (set! env (envri_extend-env env^ clos-y))
                           (set! exp body)
                                ;(k clos-k))
                           (set! pc value-of-cps))]))


;continuation
(define-union ktniu
  (empty-k dismount)
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


(define-label apply-k
 ;k v
    (union-case k ktniu
      [(empty-k dismount) (dismount-trampoline dismount)]
      [(mult-innerk x1env^ k^) (begin
                                 (set! v (* x1env^ v))
                                 (set! k k^)
                                 (set! pc apply-k))]
      [(mult-outerk x2^ env^ k^) (begin
                                   (set! k (ktniu_mult-innerk v k^))
                                   (set! exp x2^)
                                   (set! env env^)
                                   (set! pc value-of-cps))]
      [(sub1-outerk k^) (begin
                          (set! v (sub1 v))
                          (set! k k^)
                          (set! pc apply-k))]
      [(zero-outerk k^) (begin
                          (set! v (zero? v))
                          (set! k k^)
                          (set! pc apply-k))]
      [(conseq-innerk k^) (begin
                            (set! k k^)
                            (set! pc apply-k))]
      [(alt-innerk k^) (begin
                         (set! k k^)
                         (set! pc apply-k))]
      [(if-outerk conseq^ alt^ env^ k^)(if v
                                                (begin
                                                  (set! k (ktniu_conseq-innerk k^))
                                                  (set! exp conseq^)
                                                  (set! env env^)
                                                  (set! pc value-of-cps))
                                                (begin
                                                  (set! k (ktniu_alt-innerk k^))
                                                  (set! exp alt^)
                                                  (set! env env^)
                                                  (set! pc value-of-cps)))]
      [(throw-innerk kenv^) (begin
                              (set! k kenv^)
                              (set! pc apply-k))]
      [(throw-outerk v-exp^ env^) (begin
                                    (set! k (ktniu_throw-innerk v))
                                    (set! exp v-exp^)
                                    (set! env env^)
                                    (set! pc value-of-cps))]
      [(let-outerk body^ env^ k^) (begin
                                   (set! env (envri_extend-env env^ v))
                                   (set! k k^)
                                   (set! exp body^)
                                   (set! pc value-of-cps))]
      [(app-innerk ratorenv^ k^) (begin
                                   (set! closur_ ratorenv^)
                                   (set! clos-y v)
                                   (set! k k^)
                                   (set! pc apply-closure))]
      [(app-outerk rand^ env^ k^) (begin
                                    (set! k (ktniu_app-innerk v k^))
                                    (set! exp rand^)
                                    (set! env env^)
                                    (set! pc value-of-cps))]
      ;[else (k v)]



      ))
    
(define-label value-of-cps
  ;exp env k
    (union-case exp expr
      [(const exp) (begin
                     (set! v exp)
                     (set! pc apply-k))]
      [(mult x1 x2) (begin
                      (set! k (ktniu_mult-outerk x2 env k))
                      (set! exp x1)
                      (set! pc value-of-cps))]
      [(sub1 x) (begin
                  (set! k (ktniu_sub1-outerk k))
                  (set! exp x)
                  (set! pc value-of-cps))]
      [(zero x) (begin
                  (set! k (ktniu_zero-outerk k))
                  (set! exp x)
                  (set! pc value-of-cps))]
      [(if test conseq alt) (begin
                              (set! k (ktniu_if-outerk conseq alt env k))
                              (set! exp test)
                              (set! pc value-of-cps))]
      [(letcc body) (begin
                      (set! env (envri_extend-env env k))
                      (set! exp body)
                      (set! pc value-of-cps))]
      [(throw k-exp v-exp) (begin
                             (set! k (ktniu_throw-outerk v-exp env))
                             (set! exp k-exp)
                             (set! pc value-of-cps))]
      [(let e body) (begin
                      (set! k (ktniu_let-outerk body env k))
                      (set! exp e)
                      (set! pc value-of-cps))]
      [(var expr) (begin
                   ; (set! env env)
                    (set! env-y expr)
                        ; (env-k k))
                    (set! pc apply-env))]
      [(lambda body) (begin
                       (set! v (clos_closur body env))
                       (set! pc apply-k))]
      [(app rator rand) (begin
                          (set! k (ktniu_app-outerk rand env k))
                          (set! exp rator)
                          (set! pc value-of-cps))]
      ))
;(trace apply-env)
(define-label main
 
     (begin
       (set! exp (expr_let 
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
     ;(set! k (ktniu_empty-k))
     (set! pc value-of-cps)
     (mount-trampoline ktniu_empty-k k pc)
     (printf "Fact 5: ~s\n" v)))