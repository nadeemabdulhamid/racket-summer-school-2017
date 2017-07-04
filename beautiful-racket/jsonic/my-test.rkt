#lang jsonic

{ "name" : "Nadeem",   // full name
  "yob" : @$ (+ 1900 77) $@,
  "data" :
  [
    null, @$ (string-append "41"   ; ignore this racket comment
    (substring
    (number->string (exact->inexact (/ 4 10)))
    1)) $@, true,
    @$ "test" $@
  ]
}
  // good
  
