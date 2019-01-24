#lang racket

(require "monads.rkt")

(define find
  (λ (x ls)
    (match ls
      ['() (Nothing)]
      [`((,aa . ,da) . ,d)
       (if (eqv? x aa)
           (Just da)
           (find x d))])))


#|
symb (list (symb . number)) -> Maybe number
|#
(define find-and-/2
  (λ (x ls)
    (bind-maybe (find x ls)
                (λ (n) (Just (/ n 2))))
    #;
    (match (find x ls)
      [(Nothing) (Nothing)]
      [(Just n) (Just (/ n 2))])))

#;
(find-and-/2 'x `((x . 10) (y . 42) (z . 5)))

#;
(define valof
  (λ (e env)
    (match e
      [`,y
       #:when (symbol? y)
       (env y)]
      [`,n
       #:when (number? n)
       n]
      [`(λ (,x) ,b)
       (λ (a)
         (valof b (λ (y) (if (eqv? y x) a (env y)))))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))

#|
Exp Env -> Val

Exp -> State Val
|#

(define lookup
  (λ (env y)
    (match env
      ['() (error 'opps)]
      [`((,a . ,d) . ,env^)
       (if (eqv? y a)
           d
           (lookup env^ y))])))

(define valof
  (λ (e)
    (go-on ([env (get)])
      (begin
        (displayln env)
        (displayln e)
        (displayln "")
        (match e
          [`,y
           #:when (symbol? y)
           (go-on ([env (get)])
             (inj-state (lookup env y)))
           #;
           (bind-state
            (get)
            (λ (env)
              (inj-state (lookup env y))))]
          [`,n
           #:when (number? n)
           (inj-state n)]
          [`(+ ,n ,m)
           (go-on ([n^ (valof n)]
                   [m^ (valof m)])
             (inj-state (+ n^ m^)))
           #;
           (bind-state
            (valof n)
            (λ (n^)
              (bind-state
               (valof m)
               (λ (m^)
                 (inj-state (+ n^ m^))))))]
          [`(λ (,x) ,b)
           (inj-state
            (λ (a)
              (go-on ([env (get)]
                      [_ (put `((,x . ,a) . ,env))])
                (valof b))
              #;
              (bind-state
               (get)
               (λ (env)
                 (bind-state
                  (put `((,x . ,a) . ,env))
                  (λ (_)
                    (valof b)))))))]
          [`(,rator ,rand)
           (go-on ([clos (valof rator)]
                   [a (valof rand)])
             (clos a))
           #;
           (bind-state
            (valof rator)
            (λ (clos)
              (bind-state
               (valof rand)
               (λ (a)
                 (clos a)))))])))))

((run-state
  (valof `(((λ (x) (λ (y) (+ x y))) 5) 10)))
 '())
