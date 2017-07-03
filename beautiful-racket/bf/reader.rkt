#lang racket

(require (only-in br/quicklang
                  ))
(require brag/support)

(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod bf/expander  ;; "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer
       [(eof) eof]
       [(char-set "<>-.,+[]") lexeme]
       [any-char (next-token)]))
    (bf-lexer port))
  next-token)
