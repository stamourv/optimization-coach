#lang racket/base

(require racket/sandbox racket/port racket/contract racket/path
         "utils.rkt")

(provide/contract
 [run-inside-optimization-coach-sandbox
  (port-name? (-> any) . -> . any)]
 [make-file-predicate
  (port-name? . -> . ((or/c path-string? #f) . -> . any/c))])

(define (log-output in done-chan)
  (let loop ()
    (sync (handle-evt
           (read-line-evt in 'linefeed)
           (lambda (line)
             (cond [(eof-object? line) (channel-put done-chan 'done)]
                   [else
                    (log-warning
                     (format "Optimization Coach Program Output: ~a" line))
                    (loop)]))))))

(define (run-inside-optimization-coach-sandbox port-name thunk)
  (call-with-trusted-sandbox-configuration
   (lambda ()
     ;; If the sandboxed program produces any output, log it as `warning'.
     ;; Mimics what check-syntax does.
     (define log-output? (log-level? (current-logger) 'warning))
     (define-values (log-in log-out)
       (if log-output? (make-pipe) (values #f (open-output-nowhere))))
     (define log-done-chan (make-channel))
     (when log-output? (thread (lambda () (log-output log-in log-done-chan))))
     ;; Set up the environment.
     (begin0
         (parameterize
             ([current-namespace (make-base-namespace)]
              [current-load-relative-directory
               (if (path-string? port-name)
                   (let-values ([(base name _) (split-path port-name)])
                     base)
                   (current-load-relative-directory))]
              [read-accept-reader #t]
              [current-output-port log-out]
              [current-error-port  log-out])
           (thunk))
       (when log-output?
         (close-output-port log-out)
         (sync log-done-chan))))))

;; Returns a predicate that, given a path, returns whether it corresponds
;; to the right file.
;; Note: this used to call `port-name-matches?', which needs an editor.
;;  Refactored to avoid passing editors this far down. Hopefully that
;;  refactoring didn't change semantics. Keep an eye out for that.
(define (make-file-predicate port-name)
  (define unsaved-file?
    (and (symbol? port-name)
         (regexp-match #rx"^unsaved-editor" (symbol->string port-name))))
  (if unsaved-file?
      (lambda (path) ; (or/c path-string? #f)
        ;; we assume that any log entry without a filename comes from the
        ;; unsaved editor
        (not path))
      (lambda (path) ; (or/c path-string? #f)
        (and path ; need to know where it's from
             (path-string? port-name) ; needs to be backed by a file
             ;; from `port-name-matches?'
             (or (equal? path port-name) ; "fast path" check
                 (equal? (normal-case-path (normalize-path port-name))
                         (normal-case-path (normalize-path path))))))))
