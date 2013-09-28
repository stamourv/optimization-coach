#lang racket/base

(provide (all-defined-out))

(define (pos-inside-us? pos our-pos our-span)
  (and pos our-pos our-span (<= our-pos pos (+ our-pos our-span))))


;; Changed API between 5.3.6 and 5.90
;; Use our own version to avoid incompatibilities.

;; (x -> y) (listof x) [(y y -> bool)] -> (listof (listof x))
;; groups together elements that are considered equal
;; =? should be reflexive, transitive and commutative
(define (group-by key l [=? equal?])
  (for/fold ([res '()]) ; list of lists
      ([elt (in-list l)])
    (let loop ([classes     res] ; "zipper" of the equivalence classes
               [rev-classes '()])
      (cond [(null? classes)
             ;; did not find an equivalence class, create a new one
             (cons (list elt) res)]
            [(=? (key elt) (key (car (car classes))))
             ;; found the equivalence class
             (append rev-classes ; we keep what we skipped
                     ;; we extend the current class
                     (list (cons elt (car classes)))
                     (cdr classes))] ; and add the rest
            [else ; keep going
             (loop (cdr classes)
                   (cons (car classes) rev-classes))]))))
