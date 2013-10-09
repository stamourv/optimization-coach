#lang racket/base

;; Only includes pitfall-agnostic locality merging (the one that doesn't
;; generate new information) and cross-pitfall locality merging (when
;; different optimizations interact).
;; Pitfall-specific locality merging is done in the pitfall's file.

(require racket/match racket/list
         "structs.rkt" "hidden-costs.rkt" "utils.rkt")

(provide locality-merging)

(define (merge-display-entries prev l)
  (match* (prev l)
    [((display-entry subs1 start1 end1)
      (display-entry subs2 start2 end2))
     (display-entry (append subs1 subs2) start1 end1)]))

(define (locality-merging orig-report)
  (pitfall-agnostic-locality-merging
   (map report-entry->display-entry
        (cross-pitfall-locality-merging orig-report))))

(define (report-entry->display-entry entry)
  (match-define (report-entry kind msg stx provenance start end) entry)
  (display-entry
   (list ; single sub, merging will add more
    (cond [(success-report-entry? entry)
           (success-sub-display-entry stx msg)]
          [(near-miss-report-entry? entry)
           (near-miss-sub-display-entry
            stx
            msg
            (near-miss-report-entry-badness   entry)
            (near-miss-report-entry-irritants entry))]))
   start
   end))

;; Detect overlapping reports and merge them. Strictly for display purposes,
;; does not generate new information.
(define (pitfall-agnostic-locality-merging orig-report)
  ;; sort in order of starting point
  (define report (sort orig-report < #:key display-entry-start))
  (define-values (new-report _)
    (for/fold ([new-report '()]
               [prev #f])
        ([l (in-list report)])
      (match* (prev l)
        [((display-entry subs1 start1 end1)
          (display-entry subs2 start2 end2))
         (=> unmatch)
         (if (< start2 end1) ; l in within prev
             ;; merge the two
             (let ([merged (merge-display-entries prev l)])
               (values (cons merged (cdr new-report))
                       merged))
             (unmatch))]
        [(prev l) ; no overlap, just add to the list
         (values (cons l new-report) l)])))
  new-report)


;; Merges reports from different classes of pitfalls that interact.

;; Currently, only merges reports about sequence specialization from the
;; `for' macros' optimizer and from TR.
;; Sometimes, manual specialization is better than TR's specialization.
;; Sometimes, it's the other way around. No clear winner.

;; Because of this, only consider the combination of the two reports to be
;; a near miss when in hot code (or verbose mode). (As a general rule, we
;; only report things that may or may not be wins when they occur in hot
;; code.)
;; If we're not in hot code, the `for' failure report will not be produced,
;; and only the TR success report will be shown to the user. Since the
;; success report mentions that manual specialization may be better, good
;; enough when in cold code.

(define (cross-pitfall-locality-merging orig-report)
  ;; sequence specialization reports for the same sequence will start and
  ;; end in the same place
  (define by-position
    (group-by (lambda (x) (cons (report-entry-start x) (report-entry-end x)))
              orig-report))
  (apply
   append
   (for/list ([grp (in-list by-position)])
     ;; Assumption: there will be at most one `for' report at this location.
     ;; Idem for TR report.
     (define for-sequence-report
       (for/first ([report (in-list grp)]
                   #:when (and (equal? (report-entry-provenance report)
                                       'hidden-cost)
                               (equal? (report-entry-kind report)
                                       "non-specialized for clause")))
         report))
     (define TR-sequence-report
       (for/first ([report (in-list grp)]
                   ;; TODO should check against the kind, not the msg
                   ;;  problem is: kind is different for in-list and in-range,
                   ;;  etc.
                   #:when (regexp-match
                           #rx"[Ss]equence type specialization."
                           (report-entry-msg report)))
         report))
     (cond
      [(and for-sequence-report TR-sequence-report)
       ;; found both kinds of reports, replace them with a unified report
       (define new-badness ; since TR got part of the way, not as bad
         (ceiling (* (near-miss-report-entry-badness
                      for-sequence-report) ; TR's has no badness, success
                     1/2)))
       (define new-report
         (near-miss-report-entry
          "partial for clause specialization"
          (string-append
           "Typed Racket has already partially specialized this "
           "generic sequence, which is likely to improve performance.\n"
           "However, manually specializing the sequence (by wrapping "
           ;; TODO could guess which one to use by looking at TR msg
           "it in an `in-list', `in-range', or other sequence form.) "
           "may improve performance further.")
          (report-entry-stx for-sequence-report) ; same as TR
          'hidden-cost
          (report-entry-start for-sequence-report) ; same as TR
          (report-entry-end   for-sequence-report)
          new-badness
          '()))
       (cons new-report
             (remove for-sequence-report
                     (remove TR-sequence-report grp)))]
      [else ; nothing to do, leave the group alone
       grp]))))
