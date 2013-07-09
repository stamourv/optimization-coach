#lang racket/base

(require "structs.rkt" "utils.rkt" "profiling.rkt"
         racket/set)

(provide report-hidden-costs)

(define (report-hidden-costs info-log profile hot-functions)
  (define-values (report _)
    (for/fold ([produced-entries '()]
               [info-set         (list->set info-log)])
        ([node (in-list (profile-nodes profile))])
      (define-values (new-entries consumed-info)
        (process-profile-node node hot-functions info-set
                              (profile-total-time profile)))
      (values (append new-entries produced-entries)
              ;; Profile nodes can overlap (e.g. a hot function nested inside
              ;; another). To avoid reporting hidden costs once for each level
              ;; of nesting, consume info log entries as soon as someone reports
              ;; about them.
              (set-subtract info-set consumed-info))))
  report)

(define (process-profile-node profile-entry hot-functions info-log total-time)
  (define produced-entries '())
  (define consumed-info    (set))
  (define (emit e) (set! produced-entries (cons e produced-entries)))
  (define (consume-info i) (set! consumed-info (set-add consumed-info i)))

  (define inside-hot-function? (memq profile-entry hot-functions))

  (define (inside-us? pos)
    (pos-inside-us? pos (node-pos profile-entry) (node-span profile-entry)))

  (define badness-multiplier (/ (node-self profile-entry) total-time))
  ;; base values below are arbitrary
  ;; uses ceiling to never go down to 0
  ;; both badness and badness-multiplier are non-0
  (define parameter-access-badness    (ceiling (* 20 badness-multiplier)))
  (define struct-construction-badness (ceiling (* 20 badness-multiplier)))
  (define exact-real-arith-badness    (ceiling (* 20 badness-multiplier)))
  (define for-spec-failure-badness    (ceiling (* 20 badness-multiplier)))

  (define (check-hidden-cost kind message badness)
    (when inside-hot-function?
      (for/list ([info-entry (in-set info-log)]
                 #:when (info-log-entry? info-entry)
                 #:when (equal? (log-entry-kind info-entry) kind)
                 #:when (inside-us? (log-entry-pos info-entry)))
        (define start     (sub1 (log-entry-pos info-entry)))
        (define end       (+ start (syntax-span (log-entry-stx info-entry))))
        (consume-info info-entry)
        (emit (report-entry
               (list (missed-opt-report-entry
                      (log-entry-located-stx info-entry)
                      message
                      'hidden-cost
                      badness
                      '())) ; no irritants to highlight
               start end
               badness)))))

  (check-hidden-cost
   "hidden parameter"
   (string-append "This function may implicitly dereference the "
                  "`current-output-port' parameter. " ;; TODO hard coded
                  "It may be faster to take the value of the "
                  "parameter once, outside hot code, and pass it "
                  "to this function as an argument.")
   parameter-access-badness)

  (check-hidden-cost
   "struct constructor"
   (string-append
    "This struct constructor is used in hot code. "
    "Allocating structs is expensive, consider using vectors instead. "
    "To keep the same interface, consider defining macro wrappers "
    "around the vector operations that have the same name as the "
    "struct constructor and accessors.")
   struct-construction-badness)

  (check-hidden-cost
   "exact real arith"
   (string-append
    "This expression may use exact rational arithmetic, which is inefficient. "
    "You can avoid this by using operations that don't return fractional "
    ;; TODO don't hard-code `quotient', show the right one depending on the operation
    "results, such as `quotient', or using floating-point numbers.")
   exact-real-arith-badness)

  (check-hidden-cost
   "non-specialized for clause"
   (string-append
    "This `for' clause is not specialized, which introduces run-time dispatch "
    "overhead. You can avoid this by specializing it, e.g., by wrapping it in "
    "an `in-list', `in-range', or other sequence form.")
   for-spec-failure-badness)

  (values produced-entries consumed-info))
