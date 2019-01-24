#lang racket
(provide value-of value-of-fn value-of-ds)
(define value-of
  (lambda (e env)
    (match e
      [`,n #:when (integer? n) n]
     
      [`,y #:when (boolean? y) y]
      
      [`,z #:when (symbol? z) (unbox(env z))]
     
      [`(lambda (,arg),body) #:when (symbol? arg) (lambda (x) (value-of body
                                                      (let ([a (box x)])
                                                    (lambda (y)
                                                      (if (eqv? y arg) a (env y))))))]
     
     
      [`(zero?  ,ex) (zero? (value-of ex env))]
    
      [`(sub1 ,nexp) (sub1 (value-of nexp env))]
    
      [`(*, exp1, exp2) (* (value-of exp1 env) (value-of exp2 env))]
 
      [`(if ,test,then,alt) (if (value-of test env)
                      
                                (value-of then env)
                                (value-of alt env))]    
      [`(let ([,var ,value]) ,body) (let* ([l (value-of value env)]
                                          [a (box l)])
             
                                      (value-of body
                                                (lambda (x)
                                                  (if (eqv? x var) a (env x)))))]
   ;brain-teaser begin2
      [`(begin2 ,var1 ,var2) (begin (value-of var1 env) (value-of var2 env))]
   ;brain-teaser set!   
      [`(set! ,arg ,value) (set-box! (env arg) (value-of value env))]
       [`(,rator ,rand) [(value-of rator env)
                        (value-of rand env)]]
      
      ))) 

(define empty-env-fn
  (lambda ()
    (lambda (var)
      (error  `value-of "unbound variables" var))))

(define apply-env-fn
  (lambda (env y)
    (env y)))

(define extend-env-fn
  (lambda (x arg env)
    (lambda (y)
      (if (eqv? y x) arg (apply-env-fn env y)))))

(define value-of-fn
  (lambda (e env)
    (match e
      [`,n #:when (integer? n) n]    
      [`,y #:when (boolean? y) y]    
      [`,z #:when (symbol? z) (apply-env-fn env z)]   
      [`(lambda (,arg),body) #:when (symbol? arg) (lambda (x) (value-of-fn body
                                                    (extend-env-fn arg x env)))]   
      [`(zero?, ex) (zero? (value-of-fn ex env))]    
      [`(sub1 ,nexp) (sub1 (value-of-fn nexp env))]   
      [`(*, exp1, exp2) (* (value-of-fn exp1 env) (value-of-fn exp2 env))]   
      [`(if, test,then,alt) (if (value-of-fn test env)    
                                (value-of-fn then env)
                                (value-of-fn alt env))]    
      [`(let ([,var ,value]) ,body) (let ([l (value-of-fn value env)])     
                                      (value-of-fn body
                                                   (extend-env-fn var l env)))]
      [`(,rator ,rand) [(value-of-fn rator env)
                        (value-of-fn rand env)]]   
      )))

;notes on tagged list representation environments
(define empty-env-ds
  (lambda ()
    `(empty-env-ds)))
(define extend-env-ds
  (lambda (x arg env)
    `(extend-env-ds ,x ,arg ,env)))
(define apply-env-ds
  (lambda (env var)
    (match env
      [`(empty-env-ds) (error `value-of-ds "unbound variables" var)]
      [`(extend-env-ds, x ,arg ,env) (if (eqv? var x) arg
                                         (apply-env-ds env var))])))
(define value-of-ds
  (lambda (e env)
    (match e
      [`,n #:when (integer? n) n]    
      [`,y #:when (boolean? y) y]   
      [`,z #:when (symbol? z) (apply-env-ds env z)]   
      [`(lambda (,arg),body) #:when (symbol? arg) (lambda (x) (value-of-ds body
                                                    (extend-env-ds arg x env)))]   
      
      [`(zero?, ex) (zero? (value-of-ds ex env))]   
      [`(sub1 ,nexp) (sub1 (value-of-ds nexp env))]   
      [`(*, exp1, exp2) (* (value-of-ds exp1 env) (value-of-ds exp2 env))]    
      [`(if, test,then,alt) (if (value-of-ds test env)    
                                (value-of-ds then env)
                                (value-of-ds alt env))]    
      [`(let ([,var ,value]) ,body) (let ([l (value-of-ds value env)])     
                                      (value-of-ds body
                                                   (extend-env-ds var l env)))]
      [`(,rator ,rand) [(value-of-ds rator env)
                        (value-of-ds rand env)]]    
      ))) 

(define empty-env
  (lambda ()
    (lambda (var)
      (error  `value-of "unbound variables" var))))

(define apply-env
  (lambda (env y)
    (env y)))

(define extend-env
  (lambda (x arg env)
    (lambda (y)
      (if (eqv? y x) arg (apply-env-fn env y)))))

;inverse the matching clauses
(define fo-eulav   
  (lambda (e env)
    (match e
      [`,n #:when (integer? n) n]   
      [`,y #:when (boolean? y) y]    
      [`,z #:when (symbol? z) (apply-env env z)]   
      [`(,body (,arg),adbmal) #:when (symbol? arg) (lambda (x) (fo-eulav body
                                                    (extend-env arg x env)))]    
      
      [`(,ex ?orez) (zero? (fo-eulav ex env))]    
      [`(,nexp 1bus) (sub1 (fo-eulav nexp env))]    
      [`(,exp2 ,exp1 *) (* (fo-eulav exp1 env) (fo-eulav exp2 env))]    
      [`(,alt,then,test fi) (if (fo-eulav test env)   
                                (fo-eulav then env)
                                (fo-eulav alt env))]    
      ;[`(let ([,var ,value]) ,body) (let ([l (value-of-fn value env)])    
       ;                               (value-of-fn body
        ;                                           (extend-env-fn var l env)))]
      [`(,rand ,rator) [(fo-eulav rator env)
                        (fo-eulav rand env)]]   
      ))) 

(define value-of-lex
  (lambda (exp env)
    (match exp
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
      [`(zero ,x) (zero? (value-of-lex x env))]
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))

;no lambda meaning extend and apply = procedures?
;((lambda (var 0)) (const 5))
(define extend-env-lex
  cons)

(define apply-env-lex
  list-ref)