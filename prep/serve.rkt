#lang racket

(require xml net/url)

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
    (let ([xexpr (dispatch (list-ref req 1))])
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



