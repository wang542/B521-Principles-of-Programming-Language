#lang racket

(define list-ref
  (lambda (ls n)
    (letrec
        ((nth-cdr
          (lambda (n)
            (cond
              ((zero? n) ls)
              (else (cdr (nth-cdr (sub1 n)))))
            )))
      (car (nth-cdr n)))))

(define union
  (lambda (ls1 ls2)
    (cond
      ((null? ls2) ls1)
      ((member (car ls2) ls1) (union ls1 (cdr ls2)))
      (else (union (cons (car ls2) ls1) (cdr ls2))))))

(define extend
  (lambda (x pred)
    (lambda (y)
      (or (eqv? x y) (pred y)))))

(define walk-symbol
  (lambda (x ls)
    (cond
      ((not (assv x ls)) x)
      ((symbol? (cdr (assv x ls))) (walk-symbol (cdr (assv x ls)) ls))
      (else (cdr (assv x ls))))))

;Part 2

(define lambda->lumbda 
  (lambda (exp)
    (match exp
       [(? symbol?) exp]
       [`(lambda(,x) ,body) `(lumbda (,x) ,(lambda->lumbda body))]
        [`(,rator,rand) `(,(lambda->lumbda rator),(lambda->lumbda rand))])))

(define var-occurs?
 (lambda (var exp)
    (match exp
      [(? symbol?) (eqv? var exp)]
        [`(lambda(,x) ,body) (if (eqv? x var) #f (var-occurs? var body))]
        [`(,rator ,rand) (or(var-occurs? var rator) (var-occurs? var rand))]
        )))

(define vars
  (lambda (exp)
    (match exp
      [(? symbol?) (list exp)]
      [`(lambda(,x) ,body) (vars body)]
       [`(,rator ,rand) (append (vars rator) (vars rand))])))

(define unique-vars
  (lambda (exp)
    (match exp
      [(? symbol?) (list exp)]
       [`(lambda(,x) ,body) (unique-vars body)]
       [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))])))

(define var-occurs-free?
  (lambda (var exp)
    (match exp
      [(? symbol?) (eqv? var exp)]
      [`(lambda (,x) ,body) (if (eqv? x var) #f (var-occurs-free? var body))]
      [`(,rator,rand) (or (var-occurs-free? var rator) (var-occurs-free? var rand))])))

(define var-occurs-bound?
  (lambda (var exp)
    (match exp
      [(? symbol?) #f]
      [`(lambda (,x) ,body) (or (var-occurs-bound? var body) (and (eqv? x var) (var-occurs-free? var body)))]
      [`(,rator ,rand) (or (var-occurs-bound? var rator) (var-occurs-bound? var rand))])))

(define unique-free-vars
  (lambda (exp)
    (match exp
      [(? symbol?) (list exp)]
      [`(lambda (,x) ,body) (remv x (unique-free-vars body))]
      [`(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand))])))

(define unique-bound-vars
  (lambda (exp)
    (match exp
      [(? symbol?) '()]
      [`(lambda (,x) ,body) (if (member x (unique-vars body))
                                (union (list x) (unique-bound-vars body)) (unique-bound-vars body))]
      [`(,rator ,rand) (union (unique-bound-vars rator) (unique-bound-vars rand))])))

(define lex
  (lambda (exp lst)
    (match exp
      ;[`,x (symbol? x)(not (null? lst))(cons `var (index-of lst x))]
      ;[`,x (index-of lst x)]
      [(? symbol?) (cons `var (list (index-of lst exp)))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x lst)))]
      [`(,rator ,rand) (list (lex rator lst) (lex rand lst))])))


(define walk-symbol-update
  (lambda (var ls)
    (cond
      [(not (assv var ls)) var]
      ;only works if not considering bad data input,
      ;remember to complete condition
      ;[(symbol? (unbox (cdr (assv var ls)))) (walk-symbol-update (cdr (assv var ls))ls)]
      [else (set-box! (cdr (assv var ls)) (walk-symbol-update (unbox(cdr (assv var ls)))ls)) (unbox (cdr (assv var ls)))])))
     
(define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))
;

;(walk-symbol-update (unbox (cdr (assv var ls))) ls)

;define walk-symbol
 ; (lambda (x ls)
  ;  (cond
   ;   ((not (assv x ls)) x)
    ;  ((symbol? (cdr (assv x ls))) (walk-symbol (cdr (assv x ls)) ls))
     ; (else (cdr (assv x ls))))))


;var-occurs-both? only works partially
(define var-occurs-both?
  (lambda (var exp)
    (match exp
      [(? symbol?) (values (eqv? var exp) #f)] 
      [`(lambda (,x) ,body) (values
                              (and(not(eqv? x var)) (var-occurs-both? var body))
                              (or(eqv? var x) (var-occurs? var body) (var-occurs-both? var body)))] 
      [`(,rator ,rand) (values
                        (or (var-occurs-both?  rator) (var-occurs-both?  rand))
                        (or (var-occurs-both?  rator) (var-occurs-both?  rand)))])))
#|same structure 3 cases, return freevalue and boundvalue
      ;[... value(freevalue) (boundvalue)]
      ;[...]
      ;[...]
|#
(define var-occurs-both-trial
  (lambda (e exp)
    (values
     (var-occurs-free? e exp)
     (var-occurs-bound? e exp))))