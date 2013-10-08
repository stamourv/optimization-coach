#lang racket/base

(provide (struct-out report-entry)
         (struct-out sub-report-entry)
         (struct-out opt-report-entry)
         (struct-out missed-opt-report-entry)
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



;; Similar to the log-entry family of structs, but geared towards GUI display.
;; Also designed to contain info for multiple overlapping log entries.
;; - subs is a list of sub-report-entry, corresponding to all the entries
;;   between start and end
;; - badness is 0 for a report-entry containing only optimizations
;;   otherwise, it's the sum for all the subs
(struct report-entry (subs start end badness) #:transparent)
;; multiple of these can be contained in a report-entry
;; provenance is one of: 'typed-racket 'mzc
(struct sub-report-entry (stx msg provenance) #:transparent)
(struct opt-report-entry        sub-report-entry ()
        #:transparent)
(struct missed-opt-report-entry sub-report-entry (badness irritants)
        #:transparent)


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
