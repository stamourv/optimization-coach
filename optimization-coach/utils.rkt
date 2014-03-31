#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define (pos-inside-us? pos our-pos our-span)
  (and pos our-pos our-span (<= our-pos pos (+ our-pos our-span))))

(define port-name? (or/c path-string? symbol? #f)) ; value from get-port-name

;; bug was triggered by running OC on
;; https://raw.githubusercontent.com/logicchains/ArrayAccessBench/master/Rkt2.rkt
;; without saving the file
;; TODO turn into a test
(define (warn-unsaved-file)
  (log-warning
   (string-append "Optimization Coach: Warning: Found report without location. "
                  "Try saving the file before running Optimization Coach")))
