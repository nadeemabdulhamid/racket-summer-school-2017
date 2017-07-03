#lang racket
(require jsonic/parser jsonic/tokenizer brag/support)

(parse-to-datum (apply-tokenizer-maker make-tokenizer "// line comment\n"))

(parse-to-datum (apply-tokenizer-maker make-tokenizer "// line comment\n @$ 42 $@"))

(parse-to-datum (apply-tokenizer-maker make-tokenizer "hi// line comment\n"))

(parse-to-datum (apply-tokenizer-maker make-tokenizer #<<DEREK
"foo"
// comment
@$ 42 $@
DEREK
))

