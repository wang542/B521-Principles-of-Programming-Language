(require "parenthec.rkt")
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
(define-union envri
  (empty-env)
  (extend-env x val))
(define-label apply-env
    (union-case env envri
      [(empty-env) (begin
                     (set! v (error "unbound identifier"))
                     (set! pc apply-k))]
      [(extend-env x val) (if (zero? env-y)
                              (begin
                                (set! v val)
                                 (apply-k))
                                 (begin
                                   (set! env-y (sub1 env-y))
                                   (set! env x)
                                   (set! pc apply-env)))]))

(define-union clos
  (closur body env))
(define-label apply-closure
    (union-case closur_ clos
      [(closur body env^) (begin
                           (set! env (envri_extend-env env^ clos-y))
                           (set! exp body)
                           (set! pc value-of-cps))]))


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
                                    (set! pc value-of-cps))]))
    
(define-label value-of-cps
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
                    (set! env-y expr)
                    (set! pc apply-env))]
      [(lambda body) (begin
                       (set! v (clos_closur body env))
                       (set! pc apply-k))]
      [(app rator rand) (begin
                          (set! k (ktniu_app-outerk rand env k))
                          (set! exp rator)
                          (set! pc value-of-cps))]
      ))