#lang racket/base

(provide (struct-out report-entry)
         (struct-out success-report-entry)
         (struct-out near-miss-report-entry)
         (struct-out display-entry)
         display-entry-badness
         (struct-out sub-display-entry)
         (struct-out success-sub-display-entry)
         (struct-out near-miss-sub-display-entry)
         (struct-out inliner-log-entry)
         (struct-out inlining-event)
         ;; from typed-racket/optimizer/logging
         (struct-out log-entry)
         (struct-out opt-log-entry)
         (struct-out missed-opt-log-entry)
         (struct-out info-log-entry))


;; Instances of these structs are produced by TR.
;; We use our own definitions (works, they're prefabs) instead of requiring
;; them from TR to avoid version mismatches. (i.e. if we have a recent OC but
;; an old TR, OC can still accept new kinds of messages, even if the old TR
;; doesn't know about them, it just won't receive any)
(struct log-entry (kind msg stx located-stx pos) #:prefab)
(struct opt-log-entry log-entry () #:prefab)
(struct missed-opt-log-entry log-entry
        (irritants merged-irritants badness)
        #:prefab)
(struct info-log-entry log-entry () #:prefab)


;; IR used for the later passes (locality merging, filtering, etc. anything
;; that doesn't directly involve interpreting raw logs).
;; provenance is one of: 'typed-racket 'inlining 'hidden-cost
(struct report-entry (kind msg stx provenance start end)        #:transparent)
(struct success-report-entry   report-entry ()                  #:transparent)
(struct near-miss-report-entry report-entry (badness irritants) #:transparent)


;; Last IR, geared towards GUI display.
;; Also designed to contain info for multiple overlapping log entries.
;; - subs is a list of sub-display-entry, corresponding to all the entries
;;   between start and end
;; - start and end are for the whole set of subs
(struct display-entry (subs start end) #:transparent)
;; multiple of these can be contained in a display-entry
(struct sub-display-entry (stx msg) #:transparent)
(struct success-sub-display-entry sub-display-entry ()
        #:transparent)
(struct near-miss-sub-display-entry sub-display-entry (badness irritants)
        #:transparent)

(define (display-entry-badness d)
  (for/sum ([s (in-list (display-entry-subs d))])
    (if (near-miss-sub-display-entry? s)
        (near-miss-sub-display-entry-badness s)
        0)))


(struct inliner-log-entry log-entry (inlining-event) #:prefab)


(struct inlining-event (kind ; success, miss, out of fuel, ...
                        name ; _what_ gets inlined
                        loc  ; (U #f (List path line col pos span))
                        where-name ; _where_ it gets inlined (enclosing fun)
                        where-loc  ; (U #f (List path line col))
                        size ; size of the closure being inlined
                        threshold ; how big of a closure can we inline
                        ;; the last two use the same units
                        )
        #:transparent)
