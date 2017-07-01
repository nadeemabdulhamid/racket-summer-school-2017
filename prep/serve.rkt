#lang racket

(require xml
         net/url
         racket/control)

;; More: Systems Programming with Racket
;; https://docs.racket-lang.org/more/index.html


(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))
    ;(kill-thread t)
    ;(tcp-close listener)))


(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024))  ;; 50 MB limit
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              ;(sleep (random 10))
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ; Watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))
            ;(kill-thread t))))


(define (handle in out)
  (define req
    ; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))

  (when req
    ; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    
    ; dispatch:
    (let ([xexpr (prompt (dispatch (list-ref req 1)))])
      ; send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))
      ;(display "<html><body>Hello, world!</body></html>" out))


(define dispatch-table (make-hash))

(define (dispatch str-path)
  ; parse the request as a URL:
  (define url (string->url str-path))
  ; extract the path part:
  (define path (map path/param-path (url-path url)))
  ; find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ; call a handler:
      (h (url-query url))
      ; no handler found:
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))
  
(hash-set! dispatch-table "hello"
             (lambda (query)
               `(html (body "Hello, world!"))))



;; 9. SERVLETS AND SESSIONS

(define (build-request-page label next-url hidden)
  `(html
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                                      [value ""]))
                (input ([type "hidden"] [name "hidden"]
                                        [value ,hidden]))
                (input ([type "submit"] [name "enter"]
                                        [value "Enter"]))))))


(define (many query)
  (build-request-page "Number of greetings: " "/reply" ""))

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)



;; 10. CONTINUATIONS

(define (sum query)
  (build-request-page "First number: " "/one" ""))

(define (one query)
  (build-request-page "Second number: " "/two"
                      (cdr (assq 'number query))))

(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
        [m (string->number (cdr (assq 'number query)))])
    `(html (body (p "The sum is " ,(number->string (+ m n)))
                 (p (a ([href "/sum"]) "Again"))))))

(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)


;; --- direct style (continuations)

(define (sum2 query)
  (define m (get-number "First number: "))
  (define n (get-number "Second number: "))
  `(html (body (p "The sum is " ,(number->string (+ m n)))
                 (p (a ([href "/sum2"]) "Again")))))

(hash-set! dispatch-table "sum2" sum2)


(define (get-number label)
  (define query
    ; generate a url for the current computation:
    (send/suspend
     ; receive the computation-as-URL here:
     (lambda (k-url)
       ; generate the query-page result for this connection.
       ; send the query result to the saved-computation url:
       (build-request-page label k-url ""))))
  ; we arrive here later in a new connection
  (string->number (cdr (assq 'number query))))



#|

(prompt (+ 2 (control k (k 5))))
 = (prompt (+ 2 •)[(control k (k 5))])
=>
(prompt ((lambda (k) (k 5))
         (lambda (v) (+ 2 •)[v])))
=>
(prompt ((lambda (v) (+ 2 •)[v])   5))
=>
(prompt (+ 2 •)[5]) => (prompt 7) => 7

-----------

(prompt (+ 2 (control k (+ 1 (control k1 (k1 6))))))
=
(prompt (+ 2 •)[(control k (+ 1 (control k1 (k1 6))))]
=>
(prompt ((lambda (k) (+ 1 (control k1 (k1 6))))
         (lambda (v) (+ 2 •)[v])))
=>
(prompt (+ 1 (control k1 (k1 6))



|#


(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    (abort (mk-page (string-append "/" tag)))))



#|

> (define m (printf "what's up?.... ~a" (get-number "first ")))
`(html ...)

> m
error: undefined

> ((hash-ref dispatch-table "k1498926479018.363") `((number . "10")))

> m
what's up?.... 10

|#
