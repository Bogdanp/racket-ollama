#lang info

(define license 'BSD-3-Clause)
(define collections "ollama")
(define deps
  '("base"
    "ollama-lib"))
(define build-deps
  '("http-easy"
    "http-easy-lib"
    "racket-doc"
    "scribble-lib"
    "to-jsexpr"))
(define implies
  '("ollama-lib"))
(define scribblings
  '(("ollama-manual.scrbl")))
