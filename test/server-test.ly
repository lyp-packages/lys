\version "2.19.37"

\require "assert"
\pincludeOnce "../package.ly"

#(begin
  (use-modules (ice-9 popen) (ice-9 rdelim))

  (define (setup)
    (let* ((child-pid (primitive-fork)))
          (if (zero? child-pid)
              (lys:start)
              
              (begin
                (display (format "spawned server process ~a\n" child-pid))
                (display "running tests...\n")
                (catch #t (lambda () (run-tests))
                  (lambda (key . params)
                    (display (format "Error: ~a\n" (cadr params)))))
                (display (format "kill server process ~a\n" child-pid))
                (catch #t (lambda () (kill child-pid SIGINT))
                          (lambda (key . params) (display (format "Error ~a:~a\n" key params))))
                (waitpid child-pid 0)))))
                
  
  (define (run-tests) (begin
    (test-connect)))

  (define (test-connect)
    (let* ((output (send-to-server "(lys:close)")))
          (assert:string=? output "GNU LilyPond Server 2.19.37")
  ))
  
  (define (send-to-server cmd)
    (let* ((p (open-pipe "nc localhost 1225" OPEN_BOTH))
           (output ""))
      (display cmd p) (display "\n" p)
      (set! output (read-to-eof p))
      (close-pipe p)
      output))
          
  (define (read-to-eof port)
    (let loop ((buf "") (line (read-line port)))
      (if (eof-object? line)
          buf
          (loop (string-append buf line) (read-line port)))))
          
  (setup)
)