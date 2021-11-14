#lang eopl

;; interpreter for the LET language. 


(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;; implement const-exp here
      (const-exp (num) (num-val num))

      ;; implement var-exp here
      (var-exp (var) (apply-env env var))

      ;; implement comp-exp here


      
      ;; implement op-exp here
      (op-exp (exp1 str exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                  (cond
                    [(equal? "'add'" str) (num-val (+ num1 num2))]
                    [(equal? "'mult'" str) (num-val (* num1 num2))]
                    [(equal? "'div'" str) (num-val (/ num1 num2))]
                    [(equal? "'sub'" str) (num-val (- num1 num2))]
                    [else 'error]))))
      
      ;; if-exp
      (if-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

      ;; implement my-cond-exp here


      
      ;; implement str-exp here
      (str-exp (exp1) (str-val exp1))


      ;; implement bool-exp here
      (bool-exp (str)
                (cond
                  [(equal? "#true" str) (bool-val #t)]
                  [(equal? "#false" str) (bool-val #f)]
                  [else 'error]))

      
      ;; implement zero-exp here
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      ;; implement let-exp here
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

)))


;(trace value-of)