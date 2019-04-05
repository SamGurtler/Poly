(define (^ base exp) (if (not(integer? exp))
                         #;(expt base exp)
                         [raise (quasiquote((unquote base)^,exp))]
                         [cond
                           ((= exp 0) 1)
                           ((= exp 1) base)
                           ((positive? exp) (* base (^ base (- exp 1))))
                           ((negative? exp) (/ (^ base (- exp))))]))

(define term-h [case-lambda
                  (() term-h)
                  ((coe) [case-lambda
                           ((exp) [case-lambda
                                    (() [^ coe exp])
                                    ((var) [ (monomial coe exp) var ])])
                           (() coe)])])

;f(x) or coe*[f(x)]^exp
(define monomial
  (case-lambda
    {() (lambda (x) x)}
    [(coe exp) (*f coe (expt-f exp))]
    ))

;c*f(x)
(define (coe-f c) [*f c (monomial)])

;(f(x))^e 
(define (expt-f e) [^f (monomial) e])

;a^f(x)
(define (exp-f a) [^f a (monomial)])

;returns value!
(define term [case-lambda
              (() 0)
              ((const)  [(term-h const)])
              ((base exp) [((term-h base)exp)])
              ((coe var exp) [((term-h coe) exp) var])])

#|; maybe if we do combine like terms
#:(define term-l [case-lambda
              (() 0)
              ((const) (list (term-h 1) 1 const))
              ((base exp) (list (term-h 1) exp base))
              ((coe var exp) (list (term-h coe) exp var))]
|#

(define (red f lst) (if(pair? lst)
                       [if (null? (cdr lst))
                        (car lst)
                        (red f (cons (f (car lst) (cadr lst)) (cddr lst)))]
                       lst))

(define op-func
  (lambda (operator f g)
    (lambda (x)
      [cond
        ([and (procedure? f) (procedure? g)] (operator (f x) (g x)))
        ([or (procedure? f) (procedure? g)]
         (if [procedure? f]
             (operator (f x) g)
             ((op-func operator g f)x)))
        (else (operator f g))])))

(define +f (lambda args... (red (lambda (f g) (op-func + f g)) args...)))
(define -f (lambda args... (red (lambda (f g) (op-func - f g)) args...)))
(define *f (lambda args... (red (lambda (f g) (op-func * f g)) args...)))
(define /f (lambda args... (red (lambda (f g) (op-func / f g)) args...)))
 
;Limiting ^f to two parameters at a time to prevent accidental growth
(define (^f base exp) (op-func ^ base exp))
;;; EXAMPLES!!!
(let* [
       (x (lambda (x) x))
       (3x (+f x x x))
       (three_x (lambda (x) (+ x x x)))
       (6x (+f three_x 3x))
       (12x (+f [*f three_x 2] [*f 3x 2]))]
  `((12x 12) =,(12x 12)))