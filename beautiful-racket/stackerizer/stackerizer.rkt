#lang racket

(require (only-in br/quicklang
                  define-macro
                  define-macro-cases))

; "module language" = uses standard s-expression reader
; (thus, only provides an expander)

(provide + *)
(provide #%app #%datum #%top-interaction)


(define-macro (stackerizer-mb EXPR)
  #'(#%module-begin
     (for-each displayln (reverse (flatten EXPR)))))
(provide (rename-out [stackerizer-mb #%module-begin]))



#;(define-macro (define-op OP)
    #'(define-macro-cases OP
        [(OP FIRST) #'FIRST]
        [(OP FIRST NEXT (... ...)) #'(list 'OP FIRST (OP NEXT (... ...)))]))

;(define-op +)
;(define-op *)

(define-macro (define-ops OP ...)
  #'(begin
      (define-macro-cases OP
        [(OP FIRST) #'FIRST]
        [(OP FIRST NEXT (... ...)) #'(list 'OP FIRST (OP NEXT (... ...)))])
      ...))

(define-ops + *)
