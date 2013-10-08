#lang racket/base

(require racket/contract (only-in profile/analyzer profile?)
         "instrumentation.rkt" "profiling.rkt" "utils.rkt"
         "typed-racket.rkt" "inlining.rkt" "hidden-costs.rkt"
         "locality-merging.rkt" "structs.rkt")

(provide/contract
 [generate-report (input-port? port-name? (or/c profile? #f) any/c
                               . -> . (listof report-entry?))])

(provide locality-merging)

;; profile is currently only used to refine the inlining logs
(define (generate-report input port-name profile verbose?)
  ;; TODO take filters as inputs, so that locality-merging can stay here.
  ;;   means that caching has to be done here, too
  (define-values (TR-log mzc-log info-log) (generate-logs input port-name))
  (define hot-functions (and profile (prune-profile profile)))
  (define (gen-hidden-costs)
    (report-hidden-costs info-log profile hot-functions))
  (append
   (report-typed-racket TR-log profile hot-functions)
   ;; in verbose mode, show hidden costs and inlining reports no matter what
   ;; otherwise, these have too low a SNR to be worth showing
   (if (or verbose? profile)
       (append (report-inlining mzc-log profile hot-functions)
               (gen-hidden-costs))
       '())))

(module+ main
  ;; TODO have this be a raco tool at some point
  (require racket/cmdline racket/pretty)
  ;; TODO have flags for profile file, verbosity, etc.
  (define filename
    (path->complete-path
     (command-line #:args (filename) filename)))
  (for ([x (generate-report (open-input-file filename) filename #f #f)])
    (pretty-print x)
    (newline))
  )
