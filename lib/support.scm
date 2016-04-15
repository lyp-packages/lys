(use-modules (ice-9 rdelim))

; double fork worker. the child process is reaped using wait-pid.
; the grand-child is left running and calls 
(define (lys:fork-worker proc) (let* ((child-pid (primitive-fork)))
  (if (zero? child-pid)
      (let ((child-pid (primitive-fork)))
           (if (zero? child-pid)
               (proc)
               (primitive-exit)))
      (waitpid child-pid))))
;(define (lys:fork-worker proc) (let* ((child-pid (primitive-fork)))
;  (if (zero? child-pid) (begin (proc) (primitive-exit)))))

; spawn does a fork and then evaluates the given list of expressions, in a 
; fire-and-forget manner. In the parent, it returns the pid of the child.
; this macro might be useful in scenario where a long-running lilypond process 
; parses and retains multiple music variables. It can then be asked to fork 
; arbitrary processes to actually typeset the music based on different criteria.
(define-macro (lys:spawn . body)
`(let ((l (lambda () (begin . ,body)))
       (c (primitive-fork))) 
      (if (zero? c)
          (begin (l) (primitive-exit))
          c)))

; loop: read scheme expression from given port and evaluate it
(define (lys:eval-loop port)
  (let loop ((expr (read port)))
    (if (not (eof-object? expr))
        (begin (lys:eval expr)
               (loop (read port))))))

; start an evaluate loop on stdin
(define (lys:stdin-eval-loop) (lys:eval-loop (current-input-port)))

; safe eval - catch errors
(define (lys:eval expr)
  (catch #t (lambda () (eval expr (current-module)))
    (lambda (key . params)
      (display (format "Error evaluating expression ~a: ~a\n" key params)))))

(define (lys:elapsed t1 t2) (/ (- t2 t1) (* 1.0 internal-time-units-per-second)))

(define (lys:display-elapsed t1)
  (display (format "Elapsed: ~as\n" (lys:elapsed t1 (get-internal-real-time)))))

