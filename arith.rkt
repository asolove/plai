#lang plai

(define-type ArithC
  [ifC (test ArithC?) (if-case ArithC?) (then-case ArithC?)]
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

(define (parse e)
  (cond
    ((number? e) (numS e))
    ((list? e)
     (case (first e)
       [(if) (ifS (parse (second e)) (parse (third e)) (parse (fourth e)))]
       [(+) (plusS (parse (second e)) (parse (third e)))]
       [(*) (multS (parse (second e)) (parse (third e)))]
       [(-) (case (length e)
              [(2) (uminusS (parse (second e)))]
              [(3) (bminusS (parse (second e)) (parse (third e)))])]))))

(define (interp e)
  (type-case ArithC e
    [ifC (test then-case else-case)
         (if (= 0 (interp test)) (interp else-case) (interp then-case))]
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(define-type ArithS
  [ifS (test ArithS?) (if-case ArithS?) (then-case ArithS?)]
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)]
  [uminusS (e ArithS?)]
  [multS (l ArithS?) (r ArithS?)])

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [ifS (a b c) (ifC (desugar a) (desugar b) (desugar c))]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [uminusS (e) (multC (numC -1) (desugar e))]
    [bminusS (l r) (plusC (desugar l)
                      (multC (numC -1) (desugar r)))]))