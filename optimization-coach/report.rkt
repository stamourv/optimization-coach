#lang racket/base

(require racket/contract racket/list (only-in profile/analyzer profile?)
         "instrumentation.rkt" "profiling.rkt" "utils.rkt"
         "typed-racket.rkt" "inlining.rkt" "hidden-costs.rkt"
         "locality-merging.rkt" "structs.rkt")

(provide/contract
 [generate-report (input-port? port-name? (or/c path-string? #f)
                               . -> . (values (listof report-entry?)
                                              (listof report-entry?)))]
 [finalize-report ((listof report-entry?)
                   (listof (report-entry? . -> . any/c))
                   . -> . (listof display-entry?))])


;; Returns two list of reports: basic reports, which should be shown at all
;; times, and verbose reports, which should only be shown in verbose mode, or
;; when profile information allows for better pruning.
(define (generate-report input port-name profile-file)
  (define-values (orig-syntax expanded-syntax TR-log mzc-log info-log)
    (generate-logs input port-name))

  (define profile
    (and profile-file
         (load-profile profile-file orig-syntax expanded-syntax)))

  (define hot-functions (and profile (prune-profile profile)))

  (define basic-reports ; always shown
    (report-typed-racket TR-log profile hot-functions))

  ;; in verbose mode, show hidden costs and inlining reports no matter what
  ;; otherwise, these have too low a SNR to be worth showing without profile
  ;; info to help us refine results
  (define verbose-reports
    (append (report-inlining mzc-log profile hot-functions)
            (report-hidden-costs info-log profile hot-functions)))

  (values basic-reports verbose-reports))

;; Takes a pre-locality-merging report (possibly a fresh one, possibly a
;; cached one) and filters (to decide what to show and hide) and produces
;; a filtered and merged display report, ready for user consumption.
;; Filtering and merging are decoupled from report generation to allow
;; refreshing filters without having to recompute the report (which is
;; expensive).
(define (finalize-report report filters)
  (locality-merging
   (for/list ([entry (in-list report)]
              #:when (for/or ([f (in-list filters)])
                       (f entry)))
     entry)))


(module+ main
  ;; TODO have this be a raco tool at some point
  (require racket/cmdline racket/pretty)
  ;; TODO have flags to choose profile file, etc.
  (define verbose-mode? #f)
  (define profile-mode? #f)
  (define filename
    (path->complete-path
     (command-line
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
    (pretty-print x)
    (newline))
  )
