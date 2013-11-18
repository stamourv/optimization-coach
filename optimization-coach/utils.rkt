#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define (pos-inside-us? pos our-pos our-span)
  (and pos our-pos our-span (<= our-pos pos (+ our-pos our-span))))

(define port-name? (or/c path-string? symbol? #f)) ; value from get-port-name
