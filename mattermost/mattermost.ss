;;; -*- Gerbil -*-
;;; © ober
;;; mattermost client binary

(import
  :gerbil/gambit
  :gerbil/gambit/bits
  :gerbil/gambit/misc
  :gerbil/gambit/os
  :gerbil/gambit/ports
  :gerbil/gambit/threads
  :std/actor/message
  :std/actor/proto
  :std/coroutine
  :std/crypto/cipher
  :std/crypto/etc
  :std/crypto/libcrypto
  :std/error
  :std/format
  :std/generic
  :std/iter
  :std/logger
  :std/misc/completion
  :std/misc/list
  :std/misc/threads
  :std/net/request
  :std/net/websocket
  :std/pregexp
  :std/srfi/13
  :std/srfi/19
  :std/srfi/95
  :std/sugar
  :std/text/base64
  :std/text/json
  :std/text/utf8
  :std/text/yaml
  :ober/oberlib
  :ober/mattermost/client)

(declare (not optimize-dead-definitions))

(export main)

(def interactives
  (hash
   ("config" (hash (description: "Set your password.") (usage: "config") (count: 0)))
   ("get-token" (hash (description: "Get token.") (usage: "get-token") (count: 0)))
   ))

(def (main . args)
  (if (null? args)
    (usage))
  (let* ((argc (length args))
         (verb (car args))
         (args2 (cdr args)))
    (unless (hash-key? interactives verb)
      (usage))
    (let* ((info (hash-get interactives verb))
           (count (hash-get info count:)))
      (unless count
        (set! count 0))
      (unless (= (length args2) count)
        (usage-verb verb))
      (apply (eval (string->symbol (string-append "ober/mattermost/client#" verb))) args2))))

(def (usage-verb verb)
  (let ((howto (hash-get interactives verb)))
    (displayln "Wrong number of arguments. Usage is:")
    (displayln program-name " " (hash-get howto usage:))
    (exit 2)))

(def (usage)
  (displayln (format "~a: version ~a" program-name version))
  (displayln "Verbs:")
  (for (k (sort! (hash-keys interactives) string<?))
    (displayln (format "~a: ~a" k (hash-get (hash-get interactives k) description:))))
  (exit 2))
