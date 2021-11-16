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

      (comp-exp (exp1 cond1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of cond1 env))
                    (val3 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (eval1 (expval->string val2))
                      (num2 (expval->num val3)))
                  (cond ((equal? eval1 "'greater'") (bool-val (> num1 num2)))
                        ((equal? eval1 "'less'") (bool-val(< num1 num2)))
                        ((equal? eval1 "'equal'") (bool-val(= num1 num2))))
                        )))
      
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
     (my-cond-exp (cond1 exp1 conds exps else-exp)
                  (let ((val1 (value-of cond1 env))
                        (val2 (my-cond-helper (value-of-my-cond conds exps env))))
                    (if (null? val2)
                        (if (expval->bool val1)
                            (value-of exp1 env)
                            (value-of else-exp env))
                          val2)))
                              
      
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

      ;; implement find-hypotenuse here
      (hypotenuse-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (num-val (sqrt (+ (* num1 num1) (* num2 num2)))))))

)))

(define value-of-my-cond
  (lambda (conds exps env)
    (cond ((null? conds)
           (cons 'stop '()))
          ((expval->bool (value-of (car conds) env))
           (cons  (value-of (car exps) env) (value-of-my-cond (cdr conds) (cdr exps) env)))
          (else
           (value-of-my-cond (cdr conds) (cdr exps) env)))))

(define my-cond-helper
  (lambda (lst)
    (cond ((or (equal? (car lst) 'stop)) (null? lst)'())
          ((equal? (cadr lst) 'stop) (car lst))
          (else
           (my-cond-helper(cdr lst))))))

;(trace value-of)