#lang racket
(require "monads.rkt")

(define findf-maybe
  (lambda (p ls)
    (cond
      [(null? ls) (Nothing)]
      [(p (car ls)) (Just (car ls))]
      [else (findf-maybe p (cdr ls))])))

(define partition-writer
  (lambda (p ls)
    (cond
      [(null? ls)
       (inj-writer '())]
      [(p (car ls))
       (bind-writer
             (partition-writer p (cdr ls))
             (lambda (ls^)
             (inj-writer (cons (car ls) ls^))))]
      [else (bind-writer
        (tell (car ls)) (lambda (_) (partition-writer p (cdr ls))))])))


(define power-complex
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n)
       (bind-writer
        (power-complex x (sub1 n))
        (lambda (res)
          (bind-writer
          (tell res) (lambda (_) (inj-writer (* x res))))))]
      [(even? n)
       (bind-writer
        (power-complex x (/ n 2)) (lambda (y)
                                     (bind-writer
                                      (tell y)
                                     (lambda (_) (inj-writer (* y y))))))])))
(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (inj-writer 1)]
      [(zero? (sub1 n)) (inj-writer x)]
      [(odd? n)
       (go-on ([res (powerXpartials x (sub1 n))]
               [_ (tell res)])
              (inj-writer (* x res)))]
      [(even? n)
       (go-on ([res^ (powerXpartials x (/ n 2))]
               [_ (tell res^)])
              (inj-writer (* res^ res^)))])))

(define replace-with-count
  (lambda (x ls)
    (cond
      [(null? ls) (inj-state '())]
      [(symbol? ls) (if (equal? x ls)
                        (go-on ([s (get)]
                                [_ (put (add1 s))])
                                (inj-state s))
                        (inj-state ls))]
      [(pair? (car ls))
       (go-on ([car-res (replace-with-count x (car ls))]
               [cdr-res (replace-with-count x (cdr ls))])
              (inj-state (cons car-res cdr-res)))]
      [(equal? x (car ls))
       (go-on ([s (get)]
               [_ (put (add1 s))]
               [res (replace-with-count x (cdr ls))])
              (inj-state (cons s res)))]
      [else
       (go-on ([res (replace-with-count x (cdr ls))])
              (inj-state (cons (car ls) res)))])))


(define traverse
    (lambda (inj bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (go-on ([a (trav (car tree))]
                        [d (trav (cdr tree))])
                  (inj (cons a d)))]
               [else (f tree)]))))
        trav)))
(define reciprocal
  (lambda (n)
    (cond
      [(zero? n) (Nothing)]
      [else (Just (/ 1 n))])))
(define traverse-reciprocal
    (traverse Just bind-maybe reciprocal))

(define halve
  (lambda (n)
    (cond
      [(even? n) (inj-writer (/ n 2))]
      (else (bind-writer
             (tell n) (lambda (_) (inj-writer n)))))))
(define traverse-halve
    (traverse inj-writer bind-writer halve))

(define state/sum
  (lambda (n)
    (go-on ([res (get)]
            [_ (put (+ n res))])
           (inj-state res))))
(define traverse-state/sum
    (traverse inj-state bind-state state/sum))



(define apply-env
  (lambda (env x)
    (env x)))

(define extend-env
  (lambda (x a env)
    (lambda (y)
      (if (eqv? x y)
          a
          (apply-env env y)))))

(define empty-env
  (lambda ()
    (lambda (x)
      (error "unbound variable" x))))

(define closure
  (lambda (x body env)
    (lambda (a)
      (value-of-cps body (extend-env x a env)))))

(define apply-proc
  (lambda (c a)
    (c a)))
(define value-of-cps
  (lambda (expr env)
    (match expr
      [(? number?) (inj-cont expr)]
      [(? boolean?) (inj-cont expr)]
      [(? symbol?) (inj-cont (apply-env env expr))]
      [`(* ,x1 ,x2)
       (go-on ([x1^ (value-of-cps x1 env)]
               [x2^ (value-of-cps x2 env)])
              (inj-cont (* x1^ x2^)))]
      [`(sub1 ,x)
       (go-on ([x^ (value-of-cps x env)])
              (inj-cont (sub1 x^)))]
      [`(zero? ,x)
       (go-on ([x^ (value-of-cps x env)])
              (inj-cont (zero? x^)))]
      [`(if ,test ,conseq ,alt)
       (go-on ([res (value-of-cps test env)])
              (if res
                  (value-of-cps conseq env)
                  (value-of-cps alt env)))]
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                       (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp  ,v-exp)
       (go-on ([kexp^ (value-of-cps k-exp env)]
               [vexp^ (value-of-cps v-exp env)])
              (kexp^ vexp^))]
      [`(lambda (,id) ,body) (inj-cont (closure id body env))]
      [`(,rator ,rand)
       (go-on ([rator^ (value-of-cps rator env)]
               [rand^ (value-of-cps rand env)])
              (apply-proc rator^ rand^))])))

(define fact-5
    '((lambda (f)
        ((f f) 5))
      (lambda (f)
        (lambda (n)
          (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))
(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))

#|
(findf-maybe symbol? '(1 2 c))
(findf-maybe boolean? '(#f 1 2 c))
(findf-maybe number? '(a b c))
(run-writer (partition-writer even? '(1 2 3 4 5 6 7 8 9 10)))
(run-writer (partition-writer odd? '(1 2 3 4 5 6 7 8 9 10)))
(run-writer (powerXpartials 2 6))
(run-writer (powerXpartials 3 5))
(run-writer (powerXpartials 5 7))
((run-state (replace-with-count 'o '(a o (t o (e o t ((n . m) . o) . f) . t) . r))) 0)
((run-state (replace-with-count 'o '(((h (i s . o) . a) o s o e . n) . m))) 0)
((run-state (replace-with-count 'o '(o (h (o s . o) . o) . o))) 1)
(reciprocal 0)
(reciprocal 2)
(traverse-reciprocal '((1 . 2) . (3 . (4 . 5))))
(traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))
(run-writer (halve 6))
(run-writer (halve 5))
(run-writer (traverse-halve '((1 . 2) . (3 . (4 . 5)))))
((run-state (state/sum 5)) 0)
((run-state (state/sum 2)) 0)
((run-state (state/sum 2)) 3)
((run-state (traverse-state/sum '((1 . 2) . (3 . (4 . 5))))) 0)
((run-cont (value-of-cps fact-5 (empty-env))) (lambda (v) v))
((run-cont (value-of-cps capture-fun (empty-env))) (lambda (v) v))|#