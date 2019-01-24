#lang racket
(define lex
  (lambda (exp lst)
    (match exp
      [(? symbol?) (cons `var (list (index-of lst exp)))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x lst)))]
      [`,n #:when (number? n) `(const ,n)]
      [`,b #:when (boolean? b) `(const ,b)]
      [`(* ,a1 ,a2) `(* ,(lex a1 lst) ,(lex a2 lst))]
      [`(sub1 ,body) `(sub1 ,(lex body lst))]
      [`(zero? ,body) `(zero? ,(lex body lst))]
      [`(if ,t ,c ,a) `(if ,(lex t lst) ,(lex c lst) ,(lex a lst))]
      [`(,rator ,rand) (list (lex rator lst) (lex rand lst))]
      [`(let (( ,var ,exp)) ,body) `(let ,(lex exp lst) ,(lex body (cons var lst)))])))



(define empty-env
  (lambda ()
    `(empty-env)))
(define extend-env
  (lambda (x arg env)
    `(extend-env ,x ,arg ,env)))

(define apply-env
  (lambda (env var)
    (match env
      [`(empty-env) (error `value-of "unbound variables ~s" var)]
      [`(extend-env, x ,arg ,env) (if (eqv? var x) arg
                                         (apply-env env var))])))
(define closure-ds
  (lambda (arg body env)
    `(closure-ds ,arg ,body ,env)))

(define apply-closure-ds
  (lambda (closure-exp x)
    (match closure-exp
      [`(closure-ds ,arg ,body ,env) (value-of-ds body (extend-env arg x env))])))
(define value-of-ds
  (lambda (e env)
    (match e
      [`,n #:when (integer? n) n]    
      [`,y #:when (boolean? y) y]   
      [`,z #:when (symbol? z) (apply-env env z)]   
      [`(lambda (,arg),body) #:when (symbol? arg) (closure-ds arg body env)]
      [`(zero?, ex) (zero? (value-of-ds ex env))]   
      [`(sub1 ,nexp) (sub1 (value-of-ds nexp env))]   
      [`(*, exp1, exp2) (* (value-of-ds exp1 env) (value-of-ds exp2 env))]    
      [`(if ,t ,c ,a) (if (value-of-ds t env)    
                                (value-of-ds c env)
                                (value-of-ds a env))]    
      [`(let ([,var ,value]) ,body) (let ([l (value-of-ds value env)])     
                                      (value-of-ds body
                                                   (extend-env var l env)))]
      [`(,rator ,rand) [apply-closure-ds (value-of-ds rator env)
                        (value-of-ds rand env)]]    
      )))

(define closure-fn
  (lambda (arg body env)
    (lambda (x)
      (value-of-fn body (extend-env arg x env)))))

(define apply-closure-fn
  (lambda (rator x)
    (rator x)))

(define value-of-fn
  (lambda (e env)
    (match e
      [`,n #:when (integer? n) n]    
      [`,y #:when (boolean? y) y]    
      [`,z #:when (symbol? z) (apply-env env z)]   
      [`(lambda (,arg),body) #:when (symbol? arg) (closure-fn arg body env) ]
      [`(zero?, ex) (zero? (value-of-fn ex env))]    
      [`(sub1 ,nexp) (sub1 (value-of-fn nexp env))]   
      [`(*, exp1, exp2) (* (value-of-fn exp1 env) (value-of-fn exp2 env))]   
      [`(if ,t ,c ,a) (if (value-of-fn t env)    
                                (value-of-fn c env)
                                (value-of-fn a env))]    
      [`(let ([,var ,value]) ,body) (let ([l (value-of-fn value env)])     
                                      (value-of-fn body
                                                   (extend-env var l env)))]
      [`(,rator ,rand) [apply-closure-fn (value-of-fn rator env)
                        (value-of-fn rand env)]]   
      )))

(define value-of-dynamic
  (lambda (exp env)
    (match exp
       ;car
      [`(car ,lst) (car (value-of-dynamic lst env))]
      ;cdr
      [`(cdr ,lst) (cdr (value-of-dynamic lst env))]
      ;cons
      [`(cons ,a ,b) (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      ;null
      [`(null? ,a) (null? (value-of-dynamic a env))]
      ;number
      [`,n #:when (number? n) n]
      ;boolean
      [`,b #:when (boolean? b) b]
      ;var
      [`,v #:when (symbol? v) (apply-env env v)]
      ;lambda
      [`(lambda (,x) ,body) `(lambda (,x) ,body)]
      ;zero
      [`(zero? ,a) (zero? (value-of-dynamic a env))]
      ;sub1
      [`(sub1 ,exp) (sub1 (value-of-dynamic exp env))]
      ;*
      [`(* ,x1 ,x2) (* (value-of-dynamic x1 env)
                       (value-of-dynamic x2 env))]
      ;if
      [`(if ,t ,c ,a) (if (value-of-dynamic t env)
                          (value-of-dynamic c env)
                          (value-of-dynamic a env))]
      ;let
      [`(let ([,var ,exp]) ,body) (value-of-dynamic body (extend-env var (value-of-dynamic exp env) env))]
      ;quote
      [`(quote ,v) v]
      ;ratorrand
      [`(,rator ,rand) (match (value-of-dynamic rator env)
                         [`(lambda (,x) ,body) (match (value-of-dynamic rand env)
                          [`,a (value-of-dynamic body (extend-env x a env))])])]

      )))






;Brain Teaser
(define empty-env-fn
  (lambda ()
    (lambda (y)
      (error `value-of "unbound variable ~s" y))))
(define extend-env-fn
  (lambda (x arg env)
    (lambda (var)
      (if (eqv? x var) arg (apply-env-fn env var)))))
(define apply-env-fn
  (lambda (env var)
    (env var)))
(define empty-env-ds
  (lambda ()
    `(empty-env-ds)))
(define extend-env-ds
  (lambda (x var env)
    `(extend-env-ds ,x ,var ,env)))
(define apply-env-ds
  (lambda (env arg)
    (match env
      [`(empty-env-ds) (error `apply-env-ds "unbound variable ~s" arg)]
      [`(extend-env-ds ,x ,var ,env) (if (eqv? arg x) var (apply-env-ds env arg))])))



(define closure-fn-ri
  (lambda (x body env extend-env-ri value-of-ri)
    (lambda (var)
      ((value-of-ri (extend-env-ri x var env))body))))

(define apply-closure-fn-ri
  (lambda (lambdaexpression rand extend-env-ri value-of-ri)
    (lambdaexpression rand)))

(define closure-ds-ri
  (lambda (x body env extend-env-ri value-of-ri)
    `(closure-ds-ri ,x ,body ,env)))

(define apply-closure-ds-ri
  (lambda (exp rand extend-env-ri value-of-ri)
    (match exp
      [`(closure-ds-ri ,x ,body ,env)((value-of-ri (extend-env-ri x rand env)) body)])))

(define value-of-ri
  (lambda (empty-env extend-env apply-env closure apply-closure)
    (letrec ([value-of-ri-r (lambda (env)
                              (lambda (exp)
                                (match exp
                                  ;number
                                  [`,x #:when (integer? x) x]
                                  ;boolean
                                  [`,x #:when (boolean? x) x]
                                  ;var
                                  [`,var #:when (symbol? var) (apply-env env var)]
                                  ;lambda
                                  [`(lambda (,x) ,body) (closure x body env extend-env value-of-ri-r)]
                                  ;zero
                                  [`(zero? ,a) (zero? ((value-of-ri-r env)a ))]
                                  ;sub1
                                  [`(sub1 ,a) (sub1 ((value-of-ri-r env) a))]
                                  ;*
                                  [`(* ,n1 ,n2) (* ((value-of-ri-r env) n1)
                                                   ((value-of-ri-r env) n2))]
                                  ;if
                                  [`(if ,t ,c ,a) (if ((value-of-ri-r env) t)
                                                      ((value-of-ri-r env) c)
                                                      ((value-of-ri-r env) a))]
                                  ;let
                                  [`(let ([,var ,value]) ,body) (value-of-ri-r body (extend-env env var value))]
                                                                                  
                                  ;ratorrand
                                  [`(,rator ,rand) (apply-closure ((value-of-ri-r env) rator)
                                                                  ((value-of-ri-r env) rand) extend-env value-of-ri-r)])))])
      (value-of-ri-r empty-env))))
                               