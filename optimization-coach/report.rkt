#lang racket/base

(require "instrumentation.rkt" "profiling.rkt"
         "typed-racket.rkt" "inlining.rkt" "hidden-costs.rkt"
         "locality-merging.rkt")

(provide generate-report locality-merging)

;; profile is currently only used to refine the inlining logs
(define (generate-report this profile verbose?)
  ;; TODO take filters as inputs, so that locality-merging can stay here.
  ;;   means that caching has to be done here, too
  (define-values (TR-log mzc-log info-log) (generate-logs this))
  (define hot-functions (and profile (prune-profile profile)))
  (define (gen-hidden-costs)
    (report-hidden-costs info-log profile hot-functions))
  (append
   (report-typed-racket TR-log profile hot-functions)
   (if profile
       ;; inlining and hidden cost reports have too low a SNR to be shown
       ;; w/o profiling-based pruning
       (append (report-inlining mzc-log profile hot-functions)
               (gen-hidden-costs))
       '())
   ;; in verbose mode, show hidden costs no matter what
   (if (and verbose? (not profile))
       (gen-hidden-costs)
       '())))
