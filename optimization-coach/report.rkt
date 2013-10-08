#lang racket/base

(require racket/gui/base racket/class
         "instrumentation.rkt" "profiling.rkt"
         "typed-racket.rkt" "inlining.rkt" "hidden-costs.rkt"
         "locality-merging.rkt")

(provide generate-report locality-merging)

;; profile is currently only used to refine the inlining logs
(define (generate-report this profile verbose?)
  ;; TODO take filters as inputs, so that locality-merging can stay here.
  ;;   means that caching has to be done here, too
  (define-values (TR-log mzc-log info-log)
    (generate-logs (open-input-text-editor this) (send this get-port-name)))
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
