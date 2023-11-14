#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("mattermost/client"
    (static-exe:
     "mattermost/mattermost"
     "-ld-options"
     "-lpthread -lyaml -lz -lssl -L/usr/lib64")))
