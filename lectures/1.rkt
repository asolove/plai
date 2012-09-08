#lang plai

(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])


(define (parse e)
  (cond
    [(number? e) (numC e)]
    [(list? e)
     (case (first e)
       [(+) (plusC (parse (second e)) (parse (third e)))]
       [(*) (multC (parse (second e)) (parse (third e)))])]))

(define (interp e)
  (type-case ArithC e 
    (numC (n) n)
    (plusC (l r) (+ (interp l) (interp r)))
    (multC (l r) (* (interp l) (interp r)))))