#lang racket/base

(require profile/analyzer profile/sampler racket/serialize syntax/srcloc)

(require "sandbox.rkt")

(provide load-profile prune-profile
         node-source node-line node-col node-pos node-span
         ;; from profile/analyzer
         (struct-out profile)
         (struct-out node)
         (struct-out edge))

(define ((mk accessor) node)
  (define src (node-src node))
  (and src (accessor (build-source-location src))))
(define node-source  (mk srcloc-source))
(define node-line    (mk srcloc-line))
(define node-col     (mk srcloc-column))
(define node-pos     (mk srcloc-position))
(define node-span    (mk srcloc-span))


;; Load profile file (output from `optimization-coach-profile').
(define (load-profile profile-file)
  (define snapshots
    (with-input-from-file profile-file ;; TODO error gracefully if not found
      (lambda ()
        (deserialize (read)))))
  ;; We can't prune what's outside the file yet. We need the entire profile
  ;; to identify hot functions, and to get meaningful caller-callee stats.
  (analyze-samples snapshots))


;; In some cases, we only want to consider "hot" functions for further
;; analysis. `prune-profile' prunes non-hot functions from the profile.
;; To determine what is hot, we pick, in order, the hottest functions
;; (by self time. total time could be used, but may not work as well)
;; until our picks cover `total-relative-time-cutoff' (e.g. half) of
;; the total running time.
(define total-relative-time-cutoff .95) ; picked arbitrarily, subject to tweaking
(define (prune-profile profile)
  (define total-time   (profile-total-time profile))
  (define target-time  (* total-time total-relative-time-cutoff))
  (define sorted-nodes (sort (profile-nodes profile) > #:key node-self))
  (define top-nodes
    (let loop ([nodes sorted-nodes] [res '()] [sum 0])
      ;; The last function we pick can go beyond the target.
      ;; O/w, if we had a single function, taking up 100% time, it would
      ;; be discarded.
      (cond [(or (null? nodes) (> sum target-time))
             res]
            [else
             (define h (car nodes))
             (loop (cdr nodes) (cons h res) (+ sum (node-self h)))])))
  top-nodes)
