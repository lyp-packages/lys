(use-modules (ice-9 rdelim))

(define lys:default-port 1225) ; "ly" in numbers

(define (lys:start . args)
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

; double fork worker. the child process is reaped using wait-pid.
; the grand-child is left running and calls 
(define (lys:fork-worker proc) (let* ((child-pid (primitive-fork)))
  (if (zero? child-pid)
      (let ((child-pid (primitive-fork)))
           (if (zero? child-pid)
               (proc)
               (primitive-exit)))
      (waitpid child-pid))))
    
(define lys:socket #f)
(define lys:persist #f)

(define (lys:client-handler socket) (begin
  (display (format "start client handler (pid ~a)\n" (getpid)))
  (set! lys:socket socket)
  ; redirect stdout, stderr to client
  ;(redirect-port socket (current-output-port))
  ;(redirect-port socket (current-error-port))
  (display (format "GNU LilyPond Server ~a" (lilypond-version)) socket)
  
  (let loop ((expr (read socket)))
    (if (not (eof-object? expr))
        (begin (display (format "got: ~a\n" expr))
               (lys:eval expr)
               (loop (read socket))))))
  (display "client done!\n")
  (primitive-exit)
)
    
(define (lys:eval expr)
  (catch #t (lambda () (eval expr (current-module)))
    (lambda (key . params)
      (display (format "Error evaluating expression ~a: ~a\n" key params)))))
      
(define (lys:close) (shutdown lys:socket 1))