#lang racket/base

(require "structs.rkt" "utils.rkt" "profiling.rkt"
         racket/set racket/dict racket/match racket/list)

(provide report-hidden-costs)

;; TODO implement locality merging for hidden costs.
;;  in popup, highlight each problematic operation and merge
;;  to do this, have TR report the operation as an irritant, and propagate it
;;  also, have locality merging combine irritants

;; When in verbose mode, show all hidden costs, even those in cold code, and
;; even if we don't have profiling info available.
;; If profile is #f, we are in verbose mode, and report all hidden costs.
(define (report-hidden-costs info-log* profile hot-functions)
  (define info-log
    (for/list ([e (in-list info-log*)]
               #:when (info-log-entry? e))
      (determine-info-log-entry-kind e)))
  (cond
   [profile
    (define-values (report _)
      (for/fold ([produced-entries '()]
                 [info-set         (list->set info-log)])
          ([node (in-list (profile-nodes profile))])
        (define-values (new-entries consumed-info)
          (process-profile-node node hot-functions info-set
                                (profile-total-time profile)))
        (values (append new-entries produced-entries)
                ;; Profile nodes can overlap (e.g. a hot function nested
                ;; inside another). To avoid reporting hidden costs once for
                ;; each level of nesting, consume info log entries as soon as
                ;; someone reports about them.
                (set-subtract info-set consumed-info))))
    report]
   [else ; verbose mode, report everything
    (map log-entry->report info-log)]))

;; Some log entries already have their `kind' field set to a recognizeable
;; value, others have the full message there. For the latter, replace the
;; message with a common description for the hidden cost class.
(define (determine-info-log-entry-kind entry)
  (match-define (info-log-entry kind msg stx located-stx pos) entry)
  (define old-kind (log-entry-kind entry))
  (define (update-kind k) (info-log-entry k msg stx located-stx pos))
  (cond
   ;; TODO currently, the syntax object payload of these messages is fully
   ;;   expanded syntax, so not great for users. eventually try to get original
   ;;   syntax, or get from DrRacket buffer (at least in GUI mode)
   [(regexp-match "^optimizer: chaperoning to prevent undefined access due to:"
                  old-kind)
    (update-kind class-chaperone-kind)]
   ;; TODO also do logging for letrec checks for undefined.
   ;;   that's trickier, though, needs parsing of log message to get source location
   ;; TODO eventually, handle sequence specialization failure the same way
   [else entry])) ; already fine

(define (process-profile-node profile-entry hot-functions info-log total-time)
  (define produced-entries '())
  (define consumed-info    (set))
  (define (emit e) (set! produced-entries (cons e produced-entries)))
  (define (consume-info i) (set! consumed-info (set-add consumed-info i)))

  (define inside-hot-function? (memq profile-entry hot-functions))

  (define (inside-us? pos)
    (pos-inside-us? pos (node-pos profile-entry) (node-span profile-entry)))

  (define badness-multiplier (/ (node-self profile-entry) total-time))

  (when inside-hot-function?
    (for/list ([info-entry (in-set info-log)]
               #:when (inside-us? (log-entry-pos info-entry)))
      (consume-info info-entry)
      (emit (log-entry->report info-entry badness-multiplier))))

  (values produced-entries consumed-info))


(define hidden-cost-corpus '())
(define-syntax-rule (define-hidden-cost kind message base-badness)
  (set! hidden-cost-corpus
        (dict-set hidden-cost-corpus kind (list message base-badness))))

;; Base badness values below are arbitrary.

(define (parameter-message p)
  (string-append "This function may implicitly dereference the `"
                 p "' parameter. "
                 "It may be faster to take the value of the "
                 "parameter once, outside hot code, and pass it "
                 "to this function as an argument."))
(define-hidden-cost
  "hidden parameter"
  (parameter-message "current-output-port")
  20)
(define-hidden-cost
  "hidden parameter (random)"
  (parameter-message "current-pseudo-random-generator")
  20)

(define real-arith-hidden-cost-msg
  (string-append
   "This expression may use exact rational arithmetic, which is inefficient. "
   "You can avoid this by using operations that don't return fractional "
   ;; TODO don't hard-code `quotient', show the right one depending on the operation
   "results, such as `quotient', or using floating-point numbers."))
(define-hidden-cost
 "possible exact real arith"
 real-arith-hidden-cost-msg
 20)
(define-hidden-cost
 "exact real arith" ; old message (Racket 6.0 and before) for backw. compat.
 real-arith-hidden-cost-msg
 20)
(define-hidden-cost
 "non-optimized fixnum op"
 (string-append
  "The result of this fixnum-specific operation is not guaranteed to be "
  "within fixnum range, which prevents safety checks from being elided. "
  "Consider using the unsafe version of this operation to avoid checks.")
 20)

(define sequence-specialization-hidden-cost-msg
  (string-append
   "This `for' clause is not specialized, which introduces run-time dispatch "
   "overhead. You can avoid this by specializing it, e.g., by wrapping it in "
   "an `in-list', `in-range', or other sequence form."))
(define-hidden-cost
 "non-specialized for clause"
 sequence-specialization-hidden-cost-msg
 20)

(define class-chaperone-kind "class chaperone, use before def")
(define-hidden-cost
  class-chaperone-kind
  (string-append
   "This reference may appear before its definition. "
   "Because of this, the class system must fall back to a conservative "
   "(and slower) implementation strategy. Try avoiding uses before "
   "definitions.")
  20)

(define-hidden-cost
  "non-regexp pattern"
  (string-append
   "This regexp operation does not use a regexp value as its pattern argument, "
   "and uses a string (or byte-string) instead. This prevents the regex from "
   "being compiled ahead of time, which may slow your program down. "
   "Try converting the string to a regexp value using `regexp' or using"
   "regular expression literals using `#rx'.")
  20)

(define-hidden-cost
  "vector of floats"
  (string-append
   "This expression computes a vector of Float. "
   "It may be more efficient to use FlVectors instead.")
  20)

;; Converts an info log entry to a hidden cost report.
;; Optionally takes a badness multiplier, based on profiling information.
;; Default multiplier brings hidden costs to the same badness as other reports.
(define (log-entry->report info-entry [badness-multiplier 1/20])
  (match-define `(,message ,base-badness)
    (dict-ref hidden-cost-corpus (log-entry-kind info-entry)))
  (define badness   (ceiling (* base-badness badness-multiplier)))
  (define pos       (log-entry-pos info-entry))
  (define span      (syntax-span (log-entry-stx info-entry)))
  (define start     (and pos (sub1 pos)))
  (define end       (and start span (+ start span)))
  (unless (and pos span)
    (warn-unsaved-file))
  (near-miss-report-entry
   (log-entry-kind info-entry)
   message
   (log-entry-located-stx info-entry)
   'hidden-cost
   start
   end
   badness
   '())) ; no irritants to highlight
