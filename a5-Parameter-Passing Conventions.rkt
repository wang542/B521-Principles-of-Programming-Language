;; -------------------------------------------------------------------
;; Username: wang542
;; Assignment: a5
;; -------------------------------------------------------------------
;; Grader: Fred
;; Grade: 
;; -------------------------------------------------------------------
;; Scale:
;; S+	Great work.
;; S	Better than average.
;; S-	Below average. Contains a good deal of mistakes or
;; 	unattempted problems.
;; U	Unacceptable. Code which does not compile
;; 	in Racket and execute the student test
;; 	file without exception or warning receives a U.
;; N	Not turned in, or turned in after the due date
;; -------------------------------------------------------------------
;; Comments:
;; 
;; -------------------------------------------------------------------
#lang racket
(define empty-env
  (lambda ()
    '()))
(define extend-env
  (lambda (id arg env)
    `((,id . ,arg) . ,env)))
(define apply-env
  (lambda (env var)
    (match env
      [`() (error "unbound variable" var)]
      [`((,id . ,arg) . ,env^) (if (eqv? id var) arg (apply-env env^ var))])))
;(define make-closure
 ; (lambda (x body env)
  ; (lambda (a)
   ;  (val-of-cbv body (extend-env x a env)))))

(define apply-closure
  (lambda (clos a)
    (clos a)))

;(define make-closure-cbv
 ; (lambda (x body env)
  ; (lambda (a)
   ;  (val-of-cbv body (extend-env x a env)))))
#|(define value-of
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(sub1 ,n) (sub1 (value-of n env))]
      [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                  (value-of conseq env)
                                  (value-of alt env))]
      [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
      [`(random ,n) (random (value-of n env))]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(lambda (,x) ,body) (make-closure x body env)]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))|#

#|(define val-of-cbv
  (lambda (exp env)
    (match exp
      ;boolean
      [`,b #:when(boolean? b) b]
      ;number
      [`,n #:when(number? n) n]
      ;variable
      [`,v #:when(symbol? v) (unbox(apply-env env v))]
      ;zero
      [`(zero? ,x) (zero? (val-of-cbv x env))]
      ;sub1
      [`(sub1 ,x) (sub1 (val-of-cbv x env))]
      ;*
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      ;if
      [`(if ,t ,c ,a) (if (val-of-cbv t env)
                          (val-of-cbv c env)
                          (val-of-cbv a env))]
      ;random
      [`(random ,n) (random (val-of-cbv n env))]
      ;begin2
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      ;set!
      [`(set! ,x ,rhs)
       (let ([vrhs (val-of-cbv rhs env)])
         (set-box! (apply-env env x) vrhs))]
      ;Brain-teaser section
      [`(add1 ,x) (add1 (val-of-cbv x env))]
      [`(null? ,ls) (null? (val-of-cbv ls env))]
      [`(quote ,v) v]
      [`(let ([,x ,exp])
          ,body)
       (val-of-cbv body (extend-env x (box(val-of-cbv exp env)) env))]
      [`(cons^ ,e1 ,e2) (cons (box (lambda () (val-of-cbv e1 env))) (box (lambda () (val-of-cbv e2 env))))]
      [`(car^ ,ls) ((unbox (car (val-of-cbv ls env))))]
      [`(cdr^ ,ls) ((unbox (cdr (val-of-cbv ls env))))]
      [`(car ,ls) (car (val-of-cbv ls env))]
      [`(cdr ,ls) (cdr (val-of-cbv ls env))]
      [`(cons ,e1 ,e2) (cons (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(,rator ,x) #:when(symbol? x) (apply-closure(val-of-cbv rator env) (box (unbox (apply-env env x))))]
      [`(,rator ,rand) #:when (not (symbol? rand))
                       (apply-closure(val-of-cbv rator env) (box (val-of-cbv rand env)))])))|#

(define make-closure-cbr
  (lambda (id body env)
    (lambda (a)
      (val-of-cbr body (extend-env id a env)))))
(define val-of-cbr
  (lambda (exp env)
    (match exp
       ;boolean
      [`,b #:when(boolean? b) b]
      ;number
      [`,n #:when(number? n) n]
      ;variable
      [`,v #:when(symbol? v) (unbox (apply-env env v))]
      ;zero
      [`(zero? ,x) (zero? (val-of-cbr x env))]
      ;sub1
      [`(sub1 ,x) (sub1 (val-of-cbr x env))]
      ;*
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      ;if
      [`(if ,t ,c ,a) (if (val-of-cbr t env)
                          (val-of-cbr c env)
                          (val-of-cbr a env))]
      ;random
      [`(random ,n) (random (val-of-cbr n env))]
      ;begin2
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      ;set!
      [`(set! ,x ,rhs)
       (let ([vrhs (val-of-cbr rhs env)])
         (set-box! (apply-env env x) vrhs))]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      [`(,rator ,x)#:when (symbol? x) (apply-closure(val-of-cbr rator env)(apply-env env x))]
      [`(,rator ,rand) #:when(not (symbol? rand))
                       (apply-closure(val-of-cbr rator env) (box (val-of-cbr rand env)))])))

(define make-closure-cbname
  (lambda (id body env)
    (lambda (a)
      (val-of-cbname body (extend-env id a env)))))
(define val-of-cbname
  (lambda (exp env)
    (match exp
       ;boolean
      [`,b #:when(boolean? b) b]
      ;number
      [`,n #:when(number? n) n]
      ;variable
      [`,v #:when(symbol? v) ((unbox (apply-env env v)))]
      ;zero
      [`(zero? ,x) (zero? (val-of-cbname x env))]
      ;sub1
      [`(sub1 ,x) (sub1 (val-of-cbname x env))]
      ;*
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      ;if
      [`(if ,t ,c ,a) (if (val-of-cbname t env)
                          (val-of-cbname c env)
                          (val-of-cbname a env))]
      ;random
      [`(random ,n) (random (val-of-cbname n env))]
      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      [`(,rator ,x) #:when(symbol? x) (apply-closure(val-of-cbname rator env)(apply-env env x))]
      [`(,rator ,rand)#:when (not (symbol? rand))
                       ((val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))])))

(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))])
      (set-box! b (lambda () val))
      val)))
(define make-closure-cbneed
  (lambda (id body env)
    (lambda (a)
      (val-of-cbneed body (extend-env id a env)))))
(define val-of-cbneed
  (lambda (exp env)
     (match exp
       ;boolean
      [`,b #:when(boolean? b) b]
      ;number
      [`,n #:when(number? n) n]
      ;variable
      [`,v #:when(symbol? v) (unbox/need (apply-env env v))]
      ;zero
      [`(zero? ,x) (zero? (val-of-cbneed x env))]
      ;sub1
      [`(sub1 ,x) (sub1 (val-of-cbneed x env))]
      ;*
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      ;if
      [`(if ,t ,c ,a) (if (val-of-cbneed t env)
                          (val-of-cbneed c env)
                          (val-of-cbneed a env))]
      ;random
      [`(random ,c) (random (val-of-cbneed c env))]
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      [`(,rator ,x) #:when(symbol? x) ((apply-closure(val-of-cbneed rator env)(apply-env env x)))]
      [`(,rator ,rand)#:when (not (symbol? rand))
                       ((val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))])))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
            (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
      (random 2)))


(define cons-test
    '(let ((fix (lambda (f)
                 ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f)
                            (lambda (l)
                               (if (null? l)
                                   '()
                                   (cons^ (f (car^ l))
                                          ((map f) (cdr^ l))))))))))
          (let ((take (fix (lambda (take)
                             (lambda (l)
                               (lambda (n)
                                 (if (zero? n)
                                     '()
                                      (cons (car^ l) 
                                            ((take (cdr^ l)) (sub1 n))))))))))
            ((take ((fix (lambda (m)
                           (lambda (i)
                             (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))