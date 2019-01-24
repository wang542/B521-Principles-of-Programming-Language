#lang racket
(require "mk.rkt")
(require "numbers.rkt")
(require racket/trace)

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.


;; I had problem with a10-student-tests.rkt, if you encounter the same issue
;; Every test case from c311 website is at the bottom 

;; 1 What is the value of 

(run 2 (q)
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))
#|
The output of above is ((5))
The first line (==5 q) associates q with 5
Then evaluation goes to the outer conde then it goes to inner conde first,
The first == in the inner conde checks if 5=q which is true but second == checks
if q=6 which can't happen because q can't be both 5 and 6. Then evaluation goes to the
last line (== q 5) which succeeds, output is ((5))
|#

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))
#|
The output is (((_0 _1)))
First we get two new variable a and b, then we associate `(,a ,b) to q
Now q is a list made where a is the car and b is the cdr
Next we check if 'tag is not present in q or setting the constraint that 'tag can't be in q,
and the last line checks if a is symbol also saying a has to be a symbol.
I think the output (((_0 _1))) means any symbol in the position _0 _1 will make this
evaluation work. Below is just two modified test runs
(run 1 (q) 
  (fresh (a b)
    (== 'tag b)
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a))) -> '() because q can't have 'tag and b is 'tag 
(run 1 (q) 
  (fresh (a b)
    (== 1 a)
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a))) -> '() because a has to be a symbol
|#

;; 3 What do the following miniKanren constraints mean?
;; a ==-Associates two arguments, check if two arguments are equal
;; b =/=-Check if two arguments are not equal
;; c absento-Check if the first argument is not in the second argument
;; d numbero-Check if the argument is a number
;; e symbolo-Check if the argument is a symbol

;; Part II goes here.
(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))


(defrel (assoco x ls out)
    (fresh (a d aa da)
           (== `((,aa . ,da) . ,d) ls)
           (== `(,aa . ,da) a)
    (conde
     ((== aa x) (== a out))
     ((=/= aa x) (assoco x d out)))))
;(trace assoco)



(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))


(defrel (reverseo ls out)
    (conde
     ((== '() ls) (== '() out))
     ((=/= '() ls)
     (fresh (a d res)
            (== `(,a . ,d) ls)
            (reverseo d res)
            (appendo res `(,a) out)))))




(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))


(defrel (stuttero ls out)
    (conde
     ((== '() ls) (== '() out))
     ((=/= '() ls)
      (fresh (a d res)
             (== `(,a . ,d) ls)
             (== `(,a ,a . ,res) out)
             (stuttero d res)))))
;(trace stuttero)



;BrainTeaser
(defrel (lengtho ls out)
    (conde
    ((== '() ls) (== (build-num 0) out))
    ((=/= '() ls)
     (fresh (a d res)
            (== `(,a . ,d) ls)
            (lengtho d res)
            (pluso (build-num 1) res out)))))
;(trace lengtho)


;Test Cases
#|
(run* q (assoco 'x '() q))
(run* q (assoco 'x '((x . 5)) q))
(run* q (assoco 'x '((y . 6) (x . 5)) q))
(run* q (assoco 'x '((x . 6) (x . 5)) q))
(run* q (assoco 'x '((x . 5)) '(x . 5)))
(run* q (assoco 'x '((x . 6) (x . 5)) '(x . 6)))
(run* q (assoco 'x '((x . 6) (x . 5)) '(x . 5)))
(run* q (assoco q '((x . 6) (x . 5)) '(x . 5)))
(run* q (assoco 'x '((x . 6) . ,q) '(x . 6)))
(run 5 q (assoco 'x q '(x . 5)))
(run 5 q (fresh (x y z)
                (assoco x y z)
                (== `(,x ,y ,z) q)))
(run* q (reverseo '() q))
(run* q (reverseo '(a) q))
(run* q (reverseo '(a b c d) q))
(run* q (fresh (x) (reverseo `(a b ,x c d) q)))
(run* x (reverseo `(a b ,x d) '(d c b a)))
(run* x (reverseo `(a b c d) `(d . ,x)))
(run* q (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
(run 10 q (fresh (x y) (reverseo x y) (== `(,x . ,y) q)))
(run 1 q (stuttero q '(1 1 2 2 3 3)))
(run* q (stuttero q '(1 1 2 2 3 3)))
(run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero a `(1 ,b ,c 2 3 ,d))))
(run 1 q (fresh (a b c d) (== q `(,a ,b ,c ,d)) (stuttero `(,b 1) `(,c . ,d))))
(run 1 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
(run 2 q (fresh (e f g) (== q `(,e ,f ,g)) (stuttero `(,e . ,f) g)))
(run 1 q (lengtho '() q))
(run 1 q (lengtho '(a b) q))
(run 1 q (lengtho '(a b c) q))
(run 1 q (lengtho '(a b c d e f g) q))
(run 1 q (lengtho q (build-num 0)))
(run 1 q (lengtho q (build-num 5)))
(run 10 q (fresh (x y) (lengtho x y) (== `(,x ,y) q)))|#


