(define lys:default-port 1225) ; "ly" in numbers

(define lys:output-port (dup (current-output-port)))
(define (lys:debug msg)
  (begin
    (display (format "~a (~a): " (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time))) (getpid)) lys:output-port)
    (display msg lys:output-port)
    (newline lys:output-port)
    (force-output lys:output-port))) 

(define (lys:start-server . args)
  (let* ((port (if (null? args) lys:default-port (car args)))
         (server-socket (lys:open-socket-for-listening port)))
    (newline lys:output-port)
    (lys:debug (format "Listening on port ~a" port))
    (flush-all-ports)

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

(define (lys:client-handler socket) (begin
  (lys:debug "start client handler")

  (set! lys:socket socket)
  (set! lys:output-port (dup (current-output-port)))

  ; redirect stdout, stderr to client
  (redirect-port socket (current-output-port))
  (redirect-port socket (current-error-port))
  (display (format "GNU LilyPond Server ~a\n>" (lilypond-version)) socket)
  (lys:eval-loop socket)
  (lys:debug "client done!")
  (primitive-exit)))
    
(define (lys:close) (shutdown lys:socket 1))
