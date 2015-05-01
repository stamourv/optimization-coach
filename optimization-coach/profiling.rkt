#lang racket/base

(require profile/analyzer profile/sampler
         data/interval-map racket/dict racket/list racket/serialize racket/match
         syntax/parse syntax/srcloc syntax/source-syntax)

(require "sandbox.rkt" "utils.rkt" "structs.rkt")

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

;; TODO surely this must already exist somewhere
(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

;; Load profile file (output from `optimization-coach-profile'), analyse
;; the results and convert to a function graph if need be.
;; We can't prune what's outside the file yet. We need the entire profile
;; to identify hot functions, and to get meaningful caller-callee stats.
;; analyzed
(define (load-profile profile-file orig-syntax expanded-syntax)
  (define snapshots
    (with-input-from-file profile-file ;; TODO error gracefully if not found
      (lambda ()
        (deserialize (read)))))
  ;; TODO if I also want to keep the expression profile around, just call
  ;;  the analyser here on `snapshots', and keep the result
  ;;  in that case, probably also keep the function interval-map, can come
  ;;  in handy, I guess
  (expression-profile->function-profile snapshots orig-syntax expanded-syntax))


(struct function (stx name)) ; syntax? (or/c symbol? #f)

;; Traverses fully-expanded syntax, and finds all function boundaries.
;; Returns a list of syntax objects with the appropriate boundaries
;; We also report top-level expression boundaries, to help grouping later.
;; (Basically, we consider top-level expressions, until we reach a function
;; to also be "functions".)
;; To record function names, any reported function/expression directly under
;; a `define' (or `let') gets the name bound in the `define'.
;; TODO maybe this should be in errortrace (could be used by contract profiler too)
(define (find-all-functions orig-syntax expanded-syntax)
  (define recover-source
    (recover-source-syntax orig-syntax expanded-syntax))
  (remove-duplicates
   (let loop ([stx                 expanded-syntax]
              [found-function-yet? #f]
              [maybe-name          #f])

     (define (recur x)
       (loop x found-function-yet? #f)) ; not directly below a define anymore
     (define ((recur/name name) x)
       (loop x found-function-yet? name))
     (define (recur/function x) ; after finding a function
       (loop x #t #f))
     (define (recur/module x) ; after finding a module
       (loop x #f #f)) ; start recording module-level expressions
     (define (add-this-to functions)
       (cons (function (recover-source stx)
                       (and maybe-name (syntax->datum maybe-name)))
             functions))

     (cond
      [(not found-function-yet?)
       ;; record the current expression (we're at the top/module-level)
       (add-this-to (loop stx #t #f))]
      ;; traverse
      [else
       (syntax-parse stx
         #:literal-sets (kernel-literals)
         [(#%plain-lambda formals body ...)
          (add-this-to
           (append-map recur/function (syntax->list #'(body ...))))]
         [(case-lambda [formals body ...] ...)
          (add-this-to
           (append-map recur/function (syntax->list #'(body ... ...))))]

         [((~or module module*) name lang
           ((~literal #%plain-module-begin) body ...))
          (append-map recur/module (syntax->list #'(body ...)))]
         [(define-values vars body ...)
          (let ([vars* (syntax->list #'vars)])
            (append-map (recur/name (and (pair? vars*) (car vars*)))
                        (syntax->list #'(body ...))))]
         [(~or ((~or let-values letrec-values) ([ids rhs] ...) body ...)
               ;; don't traverse syntax bindings
               (letrec-syntaxes+values ([stx-ids stx-rhs ...]) ([ids rhs] ...)
                                       body ...))
          (append*
           (append
            (map (lambda (id x)
                   (define id* (syntax->list id))
                   ((recur/name (and (pair? id*) (car id*))) x))
                 (syntax->list #'(ids ...))
                 (syntax->list #'(rhs ...)))
            (map recur (syntax->list #'(body ...)))))]

         ;; boring cases
         [((~or if begin begin0 set! #%plain-app #%expression
                #%variable-reference with-continuation-mark begin-for-syntax)
           expr ...)
          (append-map recur (syntax->list #'(expr ...)))]
         ;; skip those
         [(~or ((~or #%provide #%require quote quote-syntax define-syntax) . _)
               :id)
          '()])]))))

;; The errortrace-based profiler returns profile graphs where nodes are
;; expressions, and edges are any control transfer. OTOH, the basic Racket
;; profiler returns profile graphs where nodes are functions and edges are
;; calls. This function converts the former into the latter.
;; This is necessary because inlining analysis expects functions as nodes
;; and calls as edges. So does profile pruning (for now, at least).
;; Other analyses also benefit from a coarser-grained profile for pruning.
;; (e.g. exact rational hidden costs have low self time individually, so
;; with expression profiles, likely to be pruned. But, a big nest of them
;; is worth reporting if it causes the surronding function to be hot.)
;; This function should be a no-op (or morally a no-op) on function
;; profiles, so we should be able to call it indiscriminately.
(define (expression-profile->function-profile
         samples orig-syntax expanded-syntax)

  ;; First step: find function boundaries.
  ;;   We get our function boundary info by traversing the fully expanded
  ;;   syntax. We also group top-level expressions (that are not inside
  ;;   functions) together.
  ;;   Alternative that doesn't work: using inlining logs to get function
  ;;     boundary info. Only reports functions that were attempted to
  ;;     inline, which excludes functions that are only used in a h-o
  ;;     fashion. That's a deal-breaker. Also doesn't do anything for
  ;;     top-level expressions.

  (define all-functions
    (find-all-functions orig-syntax expanded-syntax))

  ;; We need to handle nested functions. Uses an interval map to map to
  ;; innermost function.
  (define function-interval-map (make-interval-map))
  (for ([f (in-list all-functions)])
    (define stx  (function-stx f))
    (define pos  (and stx (syntax-position stx)))
    (define span (and stx (syntax-span stx)))
    (when (and stx pos span)
      (interval-map-set! function-interval-map
                         pos
                         (+ pos (syntax-span (function-stx f)))
                         f)))
  (define (pos->function pos)
    (and pos (interval-map-ref function-interval-map pos #f)))

  ;; Second step: convert the original samples to be in terms of functions.
  ;;   In the process, drop adjacent marks from the same function, otherwise
  ;;   intra-function control transfers will look like recursive calls.
  ;;   To avoid dropping actual recursive calls, if we encounter a mark we
  ;;   already converted within the same "frame", it's a recursive call, and
  ;;   start a new frame.
  (define cache (make-hash)) ; equal?-based
  (define (make-sample f)
    (define sample
      (cons (function-name f)
            (syntax->srcloc (function-stx f))))
    ;; analyzer expects hash-consed locations
    (hash-ref! cache sample sample))
  (define cpu-time (car samples)) ; need to save for later analysis
  (define converted-samples
    (for/list ([s (in-list (cdr samples))]) ; skip CPU time
      (match-define `(,thread-id ,milliseconds . ,marks) s)
      (define-values (rev-converted-marks _1 _2)
        ;; convert the marks of a single sample
        (for/fold ([rev-new-marks    '()]
                   [current-function #f]
                   [current-frame    '()])
            ([m (in-list marks)])
          (match-define `(,maybe-name . ,maybe-srcloc*) m)
          (define maybe-srcloc
            (and maybe-srcloc*
                 ;; barf @ multiple source location representations
                 (cond [(srcloc? maybe-srcloc*) maybe-srcloc*]
                       [(list? maybe-srcloc*)   (apply srcloc maybe-srcloc*)]
                       [(vector? maybe-srcloc*)
                        (apply srcloc (vector->list maybe-srcloc*))])))
          (define maybe-function
            (and maybe-srcloc (pos->function (srcloc-position maybe-srcloc))))
          (cond [(and maybe-function
                      (equal? maybe-function current-function))
                 ;; same function, we have 2 choices:
                 (if (member m current-frame)
                     ;; - we haven't seen this mark in the current frame
                     ;;   -> drop it, we're still in the same frame
                     (values rev-new-marks
                             current-function
                             (cons m current-frame))
                     ;; - we've already seen it
                     ;;   -> recursive call, start a new frame
                     (values (cons (make-sample current-function)
                                   rev-new-marks)
                             current-function ; same function
                             (list m)))] ; new frame
                [maybe-function
                 ;; we're in a different function, new frame
                 (values (cons (make-sample maybe-function)
                               rev-new-marks)
                         maybe-function
                         (list m))]
                [else
                 ;; no source location, or not mapped to any function
                 ;; leave as is ; TODO is this the best we can do?
                 (values (cons m rev-new-marks) ; m is already hash-consed
                         #f
                         '())])))
      (list* thread-id milliseconds (reverse rev-converted-marks))))

  ;; Third step: analyze the converted samples, to get the function graph
  ;; (analyze-samples (cons cpu-time converted-samples)) ;; TODO attempt
  (analyze-samples samples)
  )


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
