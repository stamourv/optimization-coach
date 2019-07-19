#lang racket/base

(require racket/cmdline racket/pretty
         raco/command-name
         "report.rkt")

;; TODO have flags to choose profile file, etc.
(define verbose-mode? #f)
(define profile-mode? #f)
(define filename
  (path->complete-path
   (command-line
    #:program (short-program+command-name)
    #:once-each
    ["-v" "Verbose mode." (set! verbose-mode? #t)]
    ["-p" "Profile mode." (set! profile-mode? #t)]
    #:args (filename)
    filename)))
(define-values (basic-reports verbose-reports)
  (generate-report (open-input-file filename)
                   filename
                   (and profile-mode?
                        (string-append (path->string filename) ".profile"))))
(define report
  (finalize-report
   (append basic-reports
           ;; if we're in verbose mode, or we have profile info for better
           ;; pruning, show the extra reports
           (if (or verbose-mode? profile-mode?) verbose-reports '()))
   `(,values))) ; only filter: anything goes
(for ([x report])
  (pretty-print x) ;; TODO barf, this is just printing structs. need to do a better job. punting for now
  (newline))

(module test racket/base)
