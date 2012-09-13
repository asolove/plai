#lang plai


(define (parse e)
  (cond
    ((number? e) (numC e))
    ((symbol? e) (idC e))
    ((list? e)
     (case (first e)
       [(+) (plusC (parse (second e)) (parse (third e)))]
       [(*) (multC (parse (second e)) (parse (third e)))]
       [symbol? (appC (second e) (third e))]))
    [else (error "unrecognized syntax")]))

(define-type ExprC
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [idC (s symbol?)]
  [appC (fn symbol?) (arg ExprC?)])

(define-type FunDefC
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])

(define (interp e fds)
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [idC (s) (error "Unbound identifier")]
    [appC (name arg) 
          (local ([define fn (get-fundef name fds)])
            (interp (subst arg
                           (fdC-arg fn)
                           (fdC-body fn))
                    fds))]))

(define (get-fundef name fds)
  (cond
    [(null? fds) (error "undefined function")]
    [(eq? name (fdC-name (first fds))) (first fds)]
    [else (get-fundef name (rest fds))]))

(define (subst value for in)
  (type-case ExprC in
    [numC (n) (numC n)]
    [plusC (l r) (plusC (subst value for l) (subst value for r))]
    [multC (l r) (multC (subst value for l) (subst value for r))]
    [idC (s) (if (eq? s for) value (idC s))]
    [appC (name arg) (appC name (subst value for arg))]))
    