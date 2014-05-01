#lang setup/infotab

(define collection 'multi) ; for compatibility with 5.3.6

(define version "3.0")

(define deps '("base" "profile-lib" "rackunit-lib" "drracket" "gui-lib"
               "data-lib" "source-syntax"
               "images-lib" "sandbox-lib" "string-constants-lib" "typed-racket-lib"
               "unstable-list-lib" "unstable-pretty-lib"))
(define build-deps '("scribble-lib"))
