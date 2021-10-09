#lang setup/infotab

(define collection 'multi) ; for compatibility with 5.3.6

(define version "3.0")

(define deps '(("base" #:version "6.2.900.6")
               ("drracket" #:version "1.6")
               ("typed-racket-lib" #:version "1.7")
               "profile-lib" "rackunit-lib" "gui-lib"
               "data-lib" "source-syntax"
               "images-lib" "sandbox-lib" "string-constants-lib"))
(define build-deps '("scribble-lib"))

(define pkg-desc "Optimization Coach Plug-In for DrRacket.")
(define pkg-authors '(stamourv))

(define license
  '(Apache-2.0 OR MIT))
