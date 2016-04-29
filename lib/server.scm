(define lys:default-port 1225) ; "ly" in numbers

(define (lys:start-server . args)
  (let* ((port (if (null? args) lys:default-port (car args)))
         (server-socket (lys:open-socket-for-listening port)))
    (display (format "\nListening on port ~a (pid ~a)\n" port (getpid)))
    (while #t
      (let ((connection (accept server-socket)))
        (lys:fork-worker (lambda () (lys:client-handler (car connection))))))))

(define (lys:open-socket-for-listening port) (let (
  (s (socket PF_INET SOCK_STREAM 0)))
  (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
  (bind s AF_INET INADDR_ANY port)
  (listen s 5) ; Queue size of 5
  s))

(define lys:socket #f)
(define lys:persist #f)

(define (lys:client-handler socket) (let* (
    (old-output (current-output-port))
  )
  (display (format "start client handler (pid ~a)\n" (getpid)))
  (set! lys:socket socket)
  ; redirect stdout, stderr to client
  (redirect-port socket (current-output-port))
  (redirect-port socket (current-error-port))
  (display (format "GNU LilyPond Server ~a\n" (lilypond-version)) socket)
  (lys:eval-loop socket)
  (display "client done!\n")
  (primitive-exit)))
    
(define (lys:close) (shutdown lys:socket 1))

