#lang racket
(require "monads.rkt")
(require "mk.rkt")
#|(lambda (x)
  ((lambda (w)
     (lambda (w)
       (lambda (x)
         y)))
   ((z
     ((lambda (Z)
        (lambda (z)
          y))
      w))
    (lambda (w)
      (lambda (w)
        (lambda (w)
          z))))))|#

;(lambda (h)
 ; (lambda (g)
  ;  ((lambda (g)
   ;    (((lambda (g) h) g)
    ;    (lambda (h) h)))
     ;(lambda (g) (g g)))))

(define occurs?
  (lambda (v exp)
    (match exp
      [`,x #:when (symbol? x) (eqv? v x)]
      [`(lambda (,x) ,body) (occurs? v body)]
      [`(,rator ,rand) (or (occurs? v rator) (occurs? v rand))])))
(define find-maybe
  (lambda (p ls)
    (cond
      ((null? ls) (Nothing))
      ((p (car ls)) (Just (car ls)))
      (else (find-maybe p (cdr ls))))))


(defrel (baro d c)
    (conde
     ((== c d)
      (fresh (e)
              (conde
               ((== `(,e) d))
               ((== '() d)))))
     ((fresh (f g h i j k l)
             (== `(,f . ,g) d)
             (== `(,i . ,j) h)
             (== `(,i . ,l) c)
             (baro g h)
             (baro k j)
             (baro `(,f . ,k) l)))))

;(run* q
 ;     (fresh (a b c)
  ;           (== `(,a ,q ,c) `(,b ,a . ,a))))
(run* q
      (fresh (a b c)
             (== q `(,b))
             (== b `(,a ,q))))
;(run* q
 ;     (fresh (a b c)
  ;           (== `(,q . ,b) `(,c ,c))
   ;          (== `(,b ,c) `(,a . ,a))))