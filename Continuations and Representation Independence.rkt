#lang racket
(require rackunit)
(require racket/trace)
;Q1
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
	((last-non-zero
	   (lambda (ls)
	     (cond
	       ;; fill in lines here
               ((null? ls) ls)
               ((eqv? (car ls) 0) (k (last-non-zero (cdr ls))))
               (else (cons (car ls) (last-non-zero (cdr ls))))
  	       ))))
	(last-non-zero ls)))))

;Q2
(define lex
  (lambda (exp acc)
    (match exp
      [(? symbol?) (cons `var (list (index-of acc exp)))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x acc)))]
      [`,n #:when (number? n) `(const ,n)]
      [`,b #:when (boolean? b) `(const ,b)]
       [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      [`(sub1 ,body) `(sub1 ,(lex body acc))]
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      [`(let/cc ,k ,body) `(letcc ,(lex body (cons k acc)))]
      [`(throw ,k,exp) `(throw ,(lex k acc) ,(lex exp acc))]
      [`(if ,t ,c ,a) `(if ,(lex t acc) ,(lex c acc) ,(lex a acc))]
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]
      [`(let (( ,var ,exp)) ,body) `(let ,(lex exp acc) ,(lex body (cons var acc)))])))



;Q3
(define empty-k
  (lambda ()
    `(empty-k)))
(define empty-env
  (lambda ()
    `(empty-env)))
(define extend-env
  (lambda (x val)
   `(extend-env ,x ,val)))
(define apply-env
  (lambda (env y k)
    (match env
      [`(empty-env) (error "unbound identifier")]
      [`(extend-env ,x ,val) (if (zero? y)
                                 (apply-k k val)
                                 (apply-env x (sub1 y) k))])))

(define make-closure
  (lambda (body env)
    `(make-closure ,body ,env)))
(define apply-closure
  (lambda (closure y k^)
    (match closure
      [`(make-closure ,body ,env) (value-of-cps body (extend-env env y) k^)])))

(define mult-innerk
  (lambda (x1env^ k^)
    `(mult-innerk ,x1env^ ,k^)))
    ;(lambda (x2env)
     ; (apply-k k^ (* x1env^ x2env)))))
(define mult-outerk
  (lambda (x2^ env^ k^)
    `(mult-outerk ,x2^ ,env^ ,k^)))
    ;(lambda (x1env)
     ; (value-of-cps x2^ env^ (mult-innerk x1env k^)))))
(define sub1-outerk
  (lambda (k^)
    `(sub1-outerk ,k^)))
    ;(lambda (xenv)
     ; (apply-k k^ (sub1 xenv)))))
(define zero-outerk
  (lambda (k^)
    `(zero-outerk ,k^)))
    ;(lambda (xenv)
     ; (apply-k k^ (zero? xenv)))))
(define conseq-innerk
  (lambda (k^)
    `(conseq-innerk ,k^)))
    ;(lambda (conenv)
     ; (apply-k k^ conenv))))
(define alt-innerk
  (lambda (k^)
    `(alt-innerk ,k^)))
    ;(lambda (altenv)
     ; (apply-k k^ altenv))))
(define if-outerk
  (lambda (conseq^ alt^ env^ k^)
    `(if-outerk ,conseq^ ,alt^ ,env^ ,k^)))
    ;(lambda (tenv)
     ; (if tenv
      ;    (value-of-cps conseq^ env^ (conseq-innerk k^))
       ;   (value-of-cps alt^ env^ (alt-innerk k^))))))
(define throw-innerk
  (lambda (kenv^)
    `(throw-innerk ,kenv^)))
   ; (lambda (venv)
    ;  (apply-k kenv^ venv))))
(define throw-outerk
  (lambda (v-exp^ env^)
    `(throw-outerk ,v-exp^ ,env^)))
    ;(lambda (kenv)
     ; (value-of-cps v-exp^ env^ (throw-innerk kenv)))))
(define let-outerk
  (lambda (body env^ k^)
    `(let-outerk ,body ,env^ ,k^)))
    ;(lambda (eenv)
     ; (value-of-cps body^ (extend-env eenv) k^))))
;(define let-outerk
 ; (lambda (body^ env^ k^)
  ;  `(let-outerk ,body^ ,env^ ,k^)))
(define app-innerk
  (lambda (ratorenv^ k^)
    `(app-innerk ,ratorenv^ ,k^)))
   ; (lambda (randenv)
    ;  (apply-closure ratorenv^ randenv k^))))
(define app-outerk
  (lambda (rand^ env^ k^)
    `(app-outerk ,rand^ ,env^ ,k^)))
    ;(lambda (ratorenv)
     ;       (value-of-cps rand^ env^ (app-innerk ratorenv k^)))))
(define apply-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(mult-innerk ,x1env^ ,k^) (apply-k k^ (* x1env^ v))]
      [`(mult-outerk ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (mult-innerk v k^))]
      [`(sub1-outerk ,k^) (apply-k k^ (sub1 v))]
      [`(zero-outerk ,k^) (apply-k k^ (zero? v))]
      [`(conseq-innerk ,k^) (apply-k k^ v)]
      [`(alt-innerk ,k^) (apply-k k^ v)]
      [`(if-outerk ,conseq^ ,alt^ ,env^ ,k^)(if v
                                                (value-of-cps conseq^ env^ (conseq-innerk k^))
                                                (value-of-cps alt^ env^ (alt-innerk k^)))]
      [`(throw-innerk ,kenv^) (apply-k kenv^ v)]
      [`(throw-outerk ,v-exp^ ,env^) (value-of-cps v-exp^ env^ (throw-innerk v))]
      [`(let-outerk ,body^ ,env ,k^) (value-of-cps body^ (extend-env env v) k^)]
      [`(app-innerk ,ratorenv^ ,k^) (apply-closure ratorenv^ v k^)]
      [`(app-outerk ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (app-innerk v k^))]
      ;[else (k v)]



      )))
    
(define value-of-cps
  (lambda (exp env k)
    (match exp
      [`(const ,exp) (apply-k k exp)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (mult-outerk x2 env k))]
      [`(sub1 ,x) (value-of-cps x env (sub1-outerk k))]
      [`(zero ,x) (value-of-cps x env (zero-outerk k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (if-outerk conseq alt env k))]
      [`(letcc ,body) (value-of-cps body (extend-env env k) k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env (throw-outerk v-exp env))]
      [`(let ,e ,body) (value-of-cps e env (let-outerk body env k))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (apply-k k (make-closure body env))]
      [`(app ,rator ,rand) (value-of-cps rator env (app-outerk rand env k))]
      )))

;Brainteaser
(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))
 
(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))
(define inf-1s (cons$ 1 inf-1s))
(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))

      
(define worlds-worst-random
    (delay (random 4)))                        
 
(define trib$-zip
  (lambda (n1 n2 n3)
    (cons$ (+ (car$ n1) (car$ n2) (car$ n3)) (trib$-zip (cdr$ n1) (cdr$ n2) (cdr$ n3)))))

(define trib$
  (cons$ 0 (cons$ 1 (cons$ 1 (trib$-zip trib$ (cdr$ trib$) (cdr$ (cdr$ trib$)))))))
  


#|
        (check-equal? (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
        (check-equal? (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
        (check-equal? (value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
        (check-equal? (value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
        (check-equal?(value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
        (check-equal?(value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k)) #f)
        (check-equal?(value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k)) 4)
        (check-equal?(value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
        (check-equal?(value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 5) 
        (check-equal?(value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
        (check-equal?(value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
        (check-equal?(value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
        (check-equal?(value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
        (check-equal?(value-of-cps '(letcc (const 5)) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(letcc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
        (check-equal?(value-of-cps '(letcc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
        (check-equal?(value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
        (check-equal?(value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)
        (check-equal?(value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                       (empty-env)
                       (empty-k))
         4)
        (check-equal?(value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                       (empty-env)
                       (empty-k))
         4)
        (check-equal?(value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                             (lambda
                               (lambda 
                                 (if (zero (var 0))  
                                     (const 1)
                                     (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                       (empty-env)
                       (empty-k))
         1)|#