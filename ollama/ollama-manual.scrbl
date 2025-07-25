#lang scribble/manual

@(require scribble/example
          (for-label json
                     net/http-easy
                     ollama
                     racket/base
                     racket/contract/base))

@title{Ollama Client}
@author[@(author+email "Bogdan Popa" "bogdan@defn.io")]

@(define ollama-anchor
   (link "https://ollama.com" "Ollama"))

This package provides a client for @|ollama-anchor|.

@margin-note{This package should be considered @emph{experimental}. All
bindings documented here are subject to change.}

@section{Reference}
@defmodule[ollama]

@subsection{Client}
@defproc[(ollama-client? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is an Ollama client.
}

@defproc[(make-ollama-client [base string? "http://127.0.0.1:11434"]
                             [#:auth auth auth-procedure/c (λ (url headers params)
                                                             (values headers params))])
         ollama-client?]{
 Returns an Ollama client that communicates with Ollama via
 @racket[base]. When the @racket[#:auth] argument is provided, it is
 used to authenticate requests.
}

@defproc[(ollama-start-chat [c ollama-client?]
                            [model string?]
                            [str-or-messages (or/c string? message? (listof (or/c string? message?)))]
                            [#:options options jsexpr? (hasheq)]
                            [#:format output-format (or/c #f string?) #f]
                            [#:tools tools (or/c #f (hash/c symbol? tool-info?)) #f]
                            [#:response->history-entry response-converter
                             (-> jsexpr? (or/c #f message?))
                             (λ (data)
                               (make-message
                                #:role 'assistant
                                (hash-ref (hash-ref data 'message) 'content)))])
         (values
          chat-response/c
          chat-continuation/c)]{
 Starts a chat with @racket[model] via @racket[c] by sending it
 @racket[str-or-messages]. Returns a pair of procedures:

 @itemlist[
 @item{A procedure to return the next part of the model's
   response. Returns @racket[eof] when the end of response has been
   reached.}
 @item{A procedure to continue the conversation by sending the LLM
   subsequent messages. Returns a new pair of chat response and chat
   continuation procedures. Continuing a chat preserves the chat
   history and passes it along to the LLM on every call.}
 ]

 The @racket[#:options] argument can be used to pass arbitrary chat
 options to Ollama.

 The @racket[#:format] argument can be used to control the LLM's
 output format. When @racket[output-format] is @racket['json], the LLM
 is instructed to format its response as JSON (but you should also
 encourage it to do so in your message).

 The @racket[#:tools] argument can be used to supply the LLM with
 @tech{tools} that it can call to perform actions.

 The @racket[#:response->history-entry] procedure can be used to alter
 LLM responses before they're committed to history. For example, you can
 use this hook to filter out reasoning from an LLM response. When the
 result of applying this procedure is @racket[#f], no entry is stored
 for that response.
}

@defthing[#:kind "contract"
          chat-response/c
          (-> (or/c jsexpr? eof-object?))]{
 The contract for chat responses.
}

@defthing[#:kind "contract"
          chat-continuation/c
          (->* [(or/c string? message? (listof (or/c string? message?)))]
               [#:format (or/c #f 'json jsexpr?)
                #:tools (or/c #f (hash/c symbol? tool-info?))]
               (values
                chat-response/c
                (recursive-contract chat-continuation/c)))]{
 The contract for chat continuations. The @racket[#:format] and
 @racket[#:tools] arguments override the values of the same arguments
 passed to any previous chat continuation procedures or to the original
 @racket[ollama-start-chat] call.
}

@subsection{Message}

@defproc[(message? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is a message.
}

@defproc[(make-message [content string?]
                       [#:role role (or/c 'assistant 'system 'user 'tool) 'user]
                       [#:images images (listof bytes?) null])
         message?]{
 Creates a new message with the given @racket[content] and
 @racket[role]. The @racket[#:images] argument can be used to attach
 base64-encoded images to the message for multimodal LLMs.
}

@subsection{Tool}

@deftech{Tools} are Racket functions that an LLM can call.

@defproc[(tool-info? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{tool}.
}

@defform[(define-tool-definer definer-id
           #:getter getter-id
           #:caller caller-id)]{

 Binds @racket[definer-id] as a definition form that declares
 @tech{tools}.

 Binds @racket[getter-id] to a procedure that returns a hash of all the
 tools that have been defined so far via @racket[definer-id].

 Binds @racket[caller-id] to a procedure that, given LLM tool call data,
 calls one of the defined procedures and returns response that can be
 sent to the LLM.

 The syntax of @racket[define-id] is

 @deftogether[(
  @defidform[:]
  @defform[#:literals (:)
           (define-id (tool-name [arg-id : type-expr] ...)
             maybe-description
             maybe-example
             e ...+)
           #:grammar
           ([maybe-description
             (code:line)
             (code:line #:description description-expr)]
            [maybe-example
             (code:line)
             (code:line #:example example-expr maybe-example)])]
  )]{

  A @racket[define-id] form binds @racket[tool-name] as a Racket
  procedure with the given set of @racket[arg-id] positional arguments.
  The procedure and its argument metadata is recorded in an internal
  registry so that all tools defined by @racket[define-id] can be
  retrieved at runtime and their metadata provided to an LLM.

  @deftogether[(
   @defproc[(Array [elem type]) type]
   @defthing[#:kind "value" Boolean type]
   @defproc[(Enum [option string?] ...+) type]
   @defthing[#:kind "value" Null type]
   @defthing[#:kind "value" Number type]
   @defform[(Object [property : type] ...+)]
   @defproc[(Optional [t type]) type]
   @defproc[(Or [t type] ...+) type]
   @defthing[#:kind "value" String type]
  )]{
   Tool arguments must declare one of these types.
  }

  @examples[
   (require ollama)
   (define-tool-definer define-tool
     #:getter get-tools
     #:caller call-tool)
   (define-tool (get-temperature [location : String])
     "451 Fahrenheit")
   (get-tools)
   (call-tool
    (hasheq
     'function
     (hasheq
      'name "get-temperature"
      'arguments (hasheq 'location "Los Angeles"))))
  ]
 }

 The @racket[caller-id] procedure raises an exception when a requested
 tool cannot be found or when it is called incorrectly.
}

@deftogether[(
 @defproc[(exn:fail:tool? [v any/c]) boolean?]
 @defproc[(exn:fail:tool:not-found? [v any/c]) boolean?]
 @defproc[(exn:fail:tool:call? [v any/c]) boolean?]
)]{
 Predicates for errors raised by tools.

 A tool error can be converted to JSON by applying @racket[->jsexpr] to
 it. The conversion adds an @racket['error] key to the original call
 data with the exception message.
}

@defproc[(raise-tool-error [format-string string?]
                           [format-arg any/c] ...
                           [#:hints hints (listof string?) null]) void?]{
 Raise a tool error with the given format and hints. The
 @racket[#:hints] argument can be used to provide a list of strings to
 instruct the LLM on how to recover.
}

@subsection{JSON}

@defidform[#:kind "interface" gen:to-jsexpr]{
 An interface for values that can be converted to JSON.
}

@defproc[(to-jsexpr? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] implements @racket[gen:to-jsexpr].
}

@defproc[(->jsexpr [v to-jsexpr?]) jsexpr?]{
 Converts @racket[v] to a @racket[jsexpr?], assuming @racket[v]
 implements @racket[gen:to-jsexpr].
}
