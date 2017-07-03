#lang jsonic

{ "name" : "Nadeem",
  "yob" : @$ (+ 1900 77) $@,
  "data" : [ null, @$ (string-append "41"   ; ignore this racket comment
                                     (substring
                                      (number->string (exact->inexact (/ 4 10)))
                                      1)) $@, true ]
  }
// good

