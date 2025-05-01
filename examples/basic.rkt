#lang racket/base

(require ollama)

(define c (make-ollama-client))
(define-values (get-more-data continue)
  (ollama-start-chat
   c "qwen2.5-coder:latest"
   "Write a fibonnaci function in Racket. Respond with just the code."))
(for ([data (in-producer get-more-data eof)])
  (display (hash-ref (hash-ref data 'message) 'content)))
(newline)
(set!-values
 (get-more-data continue)
 (continue "Make it tail recursive."))
(for ([data (in-producer get-more-data eof)])
  (display (hash-ref (hash-ref data 'message) 'content)))
(newline)
