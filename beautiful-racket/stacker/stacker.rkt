#lang racket

(require (only-in br/quicklang
                  define-macro
                  format-datums))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))

(provide read-syntax)


(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     (intermediate-display HANDLE-EXPR) ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f])
    (cond
      [(number? arg) (push-stack! arg) arg]
      [(or (equal? + arg) (equal? * arg))
       (define op-result (arg (pop-stack!) (pop-stack!)))
       (push-stack! op-result)
       op-result]
      [else #f]))

(define (intermediate-display res)
  (when res
    (display (string-append "  > " (number->string res) "\n"))))

(provide handle)




(define-macro (my-app PROC ARGS ...)
  #'(begin
      (display (format "~a: ~a~n" PROC (list ARGS ...)))
      (apply PROC (list ARGS ...))))
(provide (rename-out [my-app #%app]))


;(define + '+)
;(define * '*)
(provide + *)
(provide #%datum #%top-interaction)


