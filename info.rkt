#lang info

(define collection "optimization-coach")

(define deps '("base" "profile-lib" "rackunit-lib" "drracket" "gui-lib"
               "images" "sandbox-lib" "string-constants-lib" "typed-racket-lib"
               "unstable-list-lib" "unstable-pretty-lib"))
(define build-deps '("scribble-lib"))

(define scribblings '(("scribblings/optimization-coach.scrbl" () (tool))))

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("Optimization Coach"))
