#lang racket/base

(provide optimization-coach-profile)

(require profile/sampler profile/analyzer profile/render-text
         racket/serialize (for-syntax racket/base syntax/parse))

(define-syntax (optimization-coach-profile stx)
  (syntax-parse stx
    [(_ body ...)
     (syntax-property
      #`(let ([sampler (create-sampler (current-thread) 0.005)])
          body ...
          (sampler 'stop)
          (define samples (sampler 'get-snapshots))
          (render (analyze-samples samples))
          ;; TODO add kw arg to control filename
          (with-output-to-file #,(string-append (path->string (syntax-source stx)) ".profile")
            #:exists 'replace
            (lambda () (write (serialize samples)))))
      'typechecker:ignore #t)]))
