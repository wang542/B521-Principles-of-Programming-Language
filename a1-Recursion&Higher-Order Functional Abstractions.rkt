#lang racket
(define plus
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (add1 (plus x (sub1 y)))))))

(define countdown
  (lambda (n)
    (cond
    ((zero? n) '(0))
    ;use 0 as condition output so the countdown goes all the way to 0
    (else(cons n (countdown  (sub1 n)))))))

(define insertR
  (lambda (x y ls)
    (cond
      ((null? ls) '())
      ((eqv? (car ls) x) (cons x (cons y(insertR x y(cdr ls)))))
      (else (cons (car ls )(insertR x y (cdr ls)))))))

(define remv-1st
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((eqv? (car ls) x) (cdr ls))
      (else (cons (car ls)  (remv-1st x (cdr ls)))))))

(define list-index-ofv?
  (lambda (x ls)
    (cond
      ((null? ls) 0)
      ((eqv?(member x ls) #f) 'Bad-Data)
      ((eqv? (car ls) x) 0)
      (else (add1 (list-index-ofv? x(cdr ls)))))))

(define filter
  (lambda (predicate ls)
    (cond
      ((null? ls) '())
      ((predicate (car ls)) (cons (car ls) (filter predicate (cdr ls))))
      (else (filter predicate (cdr ls))))))
                                 
(define zip
  (lambda (ls1 ls2)
    (cond
      ((null? ls2) '())
      ;if return the ls1 when ls2 is empty, the function produce unparallel zip
      ;use '() instead
      ((null? ls1) '())
      (else  (cons(cons(car ls1)(car ls2))(zip(cdr ls1)(cdr ls2)))))))

(define map
  (lambda (p ls)
    (cond
     ((null? ls) '())
     (else (cons (p (car ls)) (map p (cdr ls)))))))

(define append
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) ls2)
      (else (cons(car ls1) (append (cdr ls1) ls2))))))


(define reverse
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (append (reverse (cdr ls)) (list (car ls)))))))

(define fact
  (lambda (n)
    (cond
      ((zero? n) 1)
      (else ( * n (fact (sub1 n)))))))

(define memv
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((eqv? (member x ls) #f) '#f)
      ((eqv? (car ls) x) ls)
      (else (memv x (cdr ls))))))

(define fib
  (lambda (n)
    (cond
      ((zero? n) 0)
      ((= n 1) 1)
      ((= n 2) 1)
      (else (+   (fib (sub1 n)) (fib (- n 2)))))))

;((w . (x . ())) y. (z . ()))
;(equal? '((w . (x . ())) y. (z . ())) '((w x) y (z)))

(define binary->natural
  (lambda (ls)
    (cond
      ((null? ls) 0)
      (else (+ (car ls) (* 2 (binary->natural (cdr ls))))))))

(define minus
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (minus x (sub1 y)))))))

(define div
  (lambda (x y)
    (cond
      ((zero? y) 'bad-data)
      ((eqv? x y) 1)
      (else (add1 (div (- x y) y ))))))

(define append-map
  (lambda (p ls)
    (cond
      ((null? ls) '())
      (else (append (p (car ls)) (append-map p (cdr ls)))))))

(define set-difference
  (lambda (ls1 ls2)
    (cond
      ((null? ls2) ls1)
      ((null? ls1) '())
      ;ls1 will eventually run out, write null to handle this condition
      ((equal? (member (car ls1) ls2) #f) (cons (car ls1)(set-difference (cdr ls1) ls2)))
      (else (set-difference (cdr ls1) ls2)))))

(define powerset
  (lambda (ls)
    (cond
      ((null? ls) '(()))
      (else (append-map (lambda (x)
                    (list x  (cons (car ls) x))) (powerset (cdr ls)))))))

(define cartesian-product
 (lambda (ls)
    (cond
      ((null? (car ls)) '())
      (else (lambda (x) (cons (car (car ls))  (cartesian-product (car (cdr ls))))
            (cons (car (car ls)) (car(cdr (cdr ls)))))))))
;Cartesian-product is wrong

(define insertR-fr
 (lambda (x y ls)
    (foldr (lambda (a accu)
             (cond
               ((eqv? a x) (cons x (cons y accu)))
               (else (cons a accu)))) '() ls)))

(define filter-fr
  (lambda (p ls)
    (foldr (lambda (a accu)
             (cond
               ((p a) (cons a accu))
               (else accu))) '() ls)))

(define map-fr
  (lambda (p ls)
    (foldr (lambda (a accu)
            (cons (p a) accu)) '() ls)))

(define append-fr
  (lambda (ls1 ls2)
    (foldr (lambda (a accu)
             (cons a accu)) ls2 ls1)))
      
(define reverse-fr
  (lambda (ls)
    (foldr (lambda (a accu)
             (append accu(list a))) '() ls)))

(define binary->natural-fr
  (lambda (ls)
    (foldr (lambda (a accu)
             (cond
               ((zero? a) (* 2 accu))
               (else (add1 ( * 2 accu))))) 0 ls)))

(define append-map-fr
  (lambda (p ls)
    (foldr (lambda (a accu)
             (append (p a) accu)) '() ls)))

(define set-difference-fr
  (lambda (ls1 ls2)
    (foldr (lambda (a accu)
           (cond
             ((eqv? (member a ls2) #f) (cons a accu))
             (else accu))) '() ls1)))


(define powerset-fr
  (lambda (ls)
    (foldr (lambda (a accu)
            (append-map (lambda (x)
                (cons a x)) accu)) '(()) ls)))
;powerset-fr is wrong

(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
     (even-case (one-case (odd-case base)))
      ;From looking at the logic of each sub-function
      ;Maybe the order of cases doesnt't matter as long as they wrap together
      ;Different orders of cases produce same result
      ;base in the center
    ))