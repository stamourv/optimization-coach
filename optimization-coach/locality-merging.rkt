#lang racket/base

;; Only includes pitfall-agnostic locality merging (the one that doesn't
;; generate new information) and cross-pitfall locality merging (when
;; different optimizations interact).
;; Pitfall-specific locality merging is done in the pitfall's file.

(require racket/match racket/list
         "structs.rkt" "hidden-costs.rkt")

(provide locality-merging)

(define (locality-merging orig-report)
  (define merged-entries
    (pitfall-agnostic-locality-merging
     (map report-entry->display-entry
          (cross-pitfall-locality-merging orig-report))))
  ;; For reports in, e.g., the template of a macro, duplicate reports may end
  ;; up here, up to one per expansion site. Showing them all is terrible UI,
  ;; and adding up their badnesses wrecks the curve. Instead, show reports only
  ;; once, with a count if need be.
  ;; TODO what to do about badness? don't want to add up, but may not want to
  ;;  just leave as is either
  (for/list ([entry (in-list merged-entries)])
    ;; for each entry, remove duplicate sub-entries, replace them with a count
    (match-define (display-entry subs start end) entry)
    (display-entry (for/list ([g (in-list (group-by values subs))])
                     (define len (length g))
                     (define rep (first g))
                     (define stx (sub-display-entry-stx rep))
                     (define msg (sub-display-entry-msg rep))
                     (define new-msg (format "~a\n\n(~a times)" msg len))
                     (if (= len 1)
                         rep ; leave unchanged
                         ;; subs in g are either all successes or all failures
                         (if (success-sub-display-entry? rep)
                             (success-sub-display-entry stx new-msg)
                             (near-miss-sub-display-entry
                              stx
                              new-msg
                              (near-miss-sub-display-entry-badness   rep)
                              (near-miss-sub-display-entry-irritants rep)))))
                   start
                   end)))

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

(define (merge-display-entries prev l)
  (match* (prev l)
    [((display-entry subs1 start1 end1)
      (display-entry subs2 start2 end2))
     (display-entry (append subs1 subs2) start1 end1)]))

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


(define merging-rules '())
(define-syntax-rule (define-merging report1-pred report2-pred gen-new-report)
  (set! merging-rules
        (cons (list report1-pred report2-pred gen-new-report)
              merging-rules)))

;; Sequence specialization: `for' fails, TR succeeds.
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
(define (for-sequence-report? report) ; find the report from `for'
  (and (equal? (report-entry-kind report) "non-specialized for clause")
       ;; TR also produces messages with this kind, but these are not the
       ;; ones we want
       (equal? (report-entry-provenance report) 'hidden-cost)))
(define-merging
  for-sequence-report?
  (lambda (report) ; find the report from TR
    ;; TODO should check against the kind, not the msg
    ;;  problem is: kind is different for in-list and in-range, etc.
    (regexp-match #rx"[Ss]equence type specialization."
                  (report-entry-msg report)))
  (lambda (for-sequence-report TR-sequence-report) ; generate a unified report
    (define new-badness ; since TR got part of the way, not as bad
      (ceiling (* (near-miss-report-entry-badness
                   for-sequence-report) ; TR's has no badness, success
                  1/2)))
    (near-miss-report-entry
     "partial for clause specialization"
     (string-append
      "Typed Racket has already partially specialized this generic "
      "sequence, which is likely to improve performance.\n"
      "However, manually specializing the sequence using `"
      (report-entry-kind TR-sequence-report)
      "' may improve performance further.")
     (report-entry-stx for-sequence-report) ; same as TR
     'hidden-cost
     (report-entry-start for-sequence-report) ; same as TR
     (report-entry-end   for-sequence-report)
     new-badness
     '())))

;; Sequence specialization: `for' fails, TR fails.
;; Double failure. Instead of reporting twice, report once, with both
;; explanations. In this case, there are multiple recommendations / solutions
;; because triggering either opt (TR's or manual specialization) is OK.
;; Note: this will only be shown in hot code, o/w only the TR failure will
;; be produced.
(define-merging
  for-sequence-report?
  (lambda (report)
    (and (equal? (report-entry-kind report) "non-specialized for clause")
         (equal? (report-entry-provenance report) 'typed-racket)))
  (lambda (for-sequence-report TR-sequence-report)
    (near-miss-report-entry
     "double for clause specialization failure"
     ;; Keep message from the TR report, since it already mentions both
     ;; solutions.
     (report-entry-msg TR-sequence-report)
     (report-entry-stx for-sequence-report) ; same as TR
     'TR
     (report-entry-start for-sequence-report) ; same as TR
     (report-entry-end   for-sequence-report)
     (near-miss-report-entry-badness TR-sequence-report)
     '())))


;; Merges reports from different classes of pitfalls that interact.
;; Reports need to affect the *exact* same location (same start and end).
(define (cross-pitfall-locality-merging orig-report)
  ;; look for reports that affect the same location
  (define by-position
    (group-by (lambda (x) (cons (report-entry-start x) (report-entry-end x)))
              orig-report))
  (apply
   append
   (for/list ([grp (in-list by-position)])
     ;; Assumption: each rule fires at most once, and order doesn't matter.
     (for/fold ([grp grp])
         ([rule (in-list merging-rules)])
       (match-define `(,report1-pred ,report2-pred ,gen-new-report) rule)
       (define report1 (for/first ([report (in-list grp)]
                                   #:when (report1-pred report))
                         report))
       (define report2 (for/first ([report (in-list grp)]
                                   #:when (report2-pred report))
                         report))
       (cond
        [(and report1 report2) ; found both, replace with a unified report
         (define new-report (gen-new-report report1 report2))
         (cons new-report
               (remove report1
                       (remove report2 grp)))]
        [else ; nothing to do, leave the group alone
         grp])))))
