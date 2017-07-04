#lang racket

(require brag/support
         racket/contract)

(module+ test
  (require rackunit))

(define (jsonic-token? x)
  (or (eof-object? x) (string? x) (token-struct? x)))

(module+ test
  (check-true (jsonic-token? eof))
  (check-true (jsonic-token? "a string"))
  (check-true (jsonic-token? (token 'A-TOK "hi")))
  (check-false (jsonic-token? 42)))

(define (make-tokenizer port)
  (port-count-lines! port)
  (define (next-token)
    (define jsonic-lexer
      (lexer
       [(eof) eof]
       [(from/stop-before "//" "\n") (next-token)]
       [(from/to "@$" "$@")
        (token 'SEXP-TOK (trim-ends "@$" lexeme "$@")
               #:position (+ (pos lexeme-start) 2)
               #:line (line lexeme-start)
               #:column (+ (col lexeme-start) 2)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start) 4))]
       [any-char (token 'CHAR-TOK lexeme
                        #:position (pos lexeme-start)
                        #:line (line lexeme-start)
                        #:column (col lexeme-start)
                        #:span (- (pos lexeme-end)
                                  (pos lexeme-start)))]))
    (jsonic-lexer port))
  next-token)
(provide (contract-out
          [make-tokenizer (input-port? . -> . (-> jsonic-token?))]))

(module+ test
  (check-equal?
   (apply-tokenizer-maker make-tokenizer "// comment\n")
   empty)
  (check-equal?
   (apply-tokenizer-maker make-tokenizer "@$ (+ 6 7  ) $@// comment\n")
   (list (token-struct 'SEXP-TOK " (+ 6 7  ) " 3 1 2 11 #f)))
  (check-equal?
   (apply-tokenizer-maker make-tokenizer "hi")
   (list (token-struct 'CHAR-TOK "h" 1 1 0 1 #f)
         (token-struct 'CHAR-TOK "i" 2 1 1 1 #f))))

