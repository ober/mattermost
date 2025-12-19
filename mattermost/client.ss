;; -*- Gerbil -*-
;; ©ober 2022
;; Mattermost client library

(import
  :gerbil/gambit
  :ober/oberlib
  :std/crypto/cipher
  :std/error
  :std/format
  :std/generic/dispatch
  :std/iter
  :std/logger
  :std/misc/list
  :std/misc/ports
  :std/net/websocket
  :std/net/request
  :std/pregexp
  :std/srfi/13
  (only-in :std/srfi/19 date->string)
  :std/sugar
  :std/text/base64
  :std/text/json
  :clan/text/yaml)

(export #t)

(def version "0.03")

(def program-name "mattermost")
(def config-file (format "~~/.~a.yaml" program-name))

(def user-list (hash))
(def channel-list (hash))
(def user-id #f)
(def team-id #f)

(def (get-token)
  (let-hash (load-config)
    (let ((token "")
	        (url (format "https://~a/api/v4/users/login" .server))
	        (data (my-json-object->string
		             (hash
		              ("login_id" .email)
		              ("password" .token)))))
      (let* ((reply (http-post url headers: (default-headers) data: data))
	           (status (request-status reply))
	           (headers (request-headers reply))
	           (text (request-text reply)))
	      (unless status
	        (error text))
	      (for (header headers)
	        (let ((k (car header))
		            (v (cdr header)))
	          (when (string=? k "Token")
	            (set! token v)))))
      token)))

(def (unread channel)
  "Fetch unread messages and surrounding items from channel"
  (let-hash (load-config)
    (let ((outs [[ "User" "Message" "Id" ]])
          (url (format "https://~a/api/v4/users/~a/channels/~a/posts/unread" .server (email->id .email) (channel->id channel))))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (let-hash body
	        (when (hash-table? .?posts)
	          (hash-for-each
	           (lambda (k v)
	             (when (hash-table? v)
		             (let-hash v
		               (set! outs (cons [ (id->username .?user_id) (one-liner .?message) .?id ] outs)))))
	           .?posts))))
      (style-output outs .?style))))

(def (one-liner string)
  "Convert a multiline string to one."
  (if (and (string? string)
           (> (string-length string) 1))
    (pregexp-replace "\\n" string " ")
    string))

(def (unreads)
  "Get the total unread messages and mentions for your user"
  (let-hash (load-config)
    (let ((outs [[ "stuff" ]])
	        (url (format "https://~a/api/v4/users/~a/teams/unread" .server (email->id .email))))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (pi (car body))))))

(def (privates)
  "Get a page of private channels on a team based on query string"
  (let-hash (load-config)
    (let* ((outs [[ "stuff" ]])
           (url (format "https://~a/api/v4/teams/~a/channels/private" .server (get-team-id))))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (pi body)))))

(def (plugins)
  "Get a page of private channels on a team based on query string"
  (let-hash (load-config)
    (let ((outs [[ "stuff" ]])
	        (url (format "https://~a/api/v4/plugins/statuses" .server)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (pi body)))))

(def (groups)
  "Get a page of private channels on a team based on query string"
  (let-hash (load-config)
    (let ((outs [[ "stuff" ]])
	        (url (format "https://~a/api/v4/teams/~a/groups" .server (get-team-id))))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (pi body)))))

(def (emojis)
  "Get a page of private channels on a team based on query string"
  (let-hash (load-config)
    (let ((outs [[ "Name" "Creator" "Updated" "Created" "Deleted" "Id" ]])
	        (url (format "https://~a/api/v4/emoji" .server)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (for (emoji body)
	        (when (hash-table? emoji)
	          (let-hash emoji
	            (set! outs (cons [ .?name (hash->list (id->user .?creator_id)) .?updated_at .?created_at .?deleted_at .?id ] outs))))))
      (style-output outs .?style))))

(def (users)
  "List users"
  (let-hash (load-config)
    (let ((outs [[ "username" "id" "email" "position" "First Name" "Last Name" "created_at" "updated_at" ]])
	        (url (format "https://~a/api/v4/users?active=true&per_page=200&in_team=~a" .server (get-team-id))))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (when (list? body)
	        (for (user body)
	          (let-hash user
	            (set! outs (cons [
				                        .?username
				                        .?id
				                        .?email
				                        .?position
				                        .?first_name
				                        .?last_name
				                        .?create_at
				                        .?update_at ] outs))))))
      (style-output outs .?style))))

(def (userinfo username)
  "Fetch the user data for user_id"
  (let-hash (load-config)
    (let* ((outs [[ "Username" "First Name" "Last Name" "Nickname" "Position" "Updated" "Roles" "Id" ]])
          (url (format "https://~a/api/v4/users/username/~a" .server username)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (when (hash-table? body)
          (dp (hash->string body))
	        (let-hash body
	          (set! outs (cons [ .?username .?first_name .?last_name .?nickname .?position (print-date (epoch->date .?update_at)) .?roles .?id ] outs)))))
      (style-output outs .?style))))

(def (id->post id)
  "Fetch contents of post id"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts/~a" .server id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      body))))

(def (get-id-from-email email)
  "Fetch the userid based on email"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/email/~a" .server email)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      body))))

(def (email->id email)
  "Wrapper for email-id"
  (let ((user (get-id-from-email email)))
    (when (hash-table? user)
      (dp (hash->string user))
      (let-hash user
        .?id))))

(def (id->user id)
  "Fetch contents of post id"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a" .server id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      body))))

(def (id->username id)
  "Fetch the username"
  (let ((hit (hash-get user-list id)))
    (if hit
      hit
      (let ((user (id->user id)))
	      (when (hash-table? user)
	        (let-hash user
	          (hash-put! user-list id .?username)
	          (or .?username .email)))))))

(def (id->channel id)
  (let ((hit (hash-get channel-list id)))
    (if hit
      hit
      (let ((channel (id->chan id)))
	      (when (hash-table? channel)
	        (let-hash channel
	          (hash-put! channel-list id .?display_name)
	          .?display_name))))))

(def (id->chan id)
  "Fetch contents of post id"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a" .server id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      body))))

(def (post channel message)
  "Post a message to a channel"
  (let-hash (load-config)
    (let ((outs [[ "Message" "Reply Count" "Channel" "User Id" "Pinned?" ]])
	        (url (format "https://~a/api/v4/posts" .server))
	        (data (my-json-object->string
		             (hash
		              ("channel_id" (channel->id channel))
		              ("message" message)
		              ))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
	      (unless status
	        (error body))
	      (when (hash-table? body)
	        (let-hash body
	          (set! outs (cons [
			                        .message
			                        (hash->list .metadata)
			                        ] outs))))))))

(def (posts channel page recur)
  "Get posts for a channel"
  (let-hash (load-config)
    (let* ((outs [[ "Time" "User" "Message" ]])
	         (channel-id (channel->id channel))
           (user-id (get-id-from-email .?email))
           (max-page (+ (any->int page) (any->int recur)))
           (users (hash)))
      (let lp ((page page))
	      (let* ((now (float->int (time->seconds (current-time))))
	             (url (format "https://~a/api/v4/channels/~a/posts?per_page=200&page=~a" .server channel-id page)))
	        (with ([ status body ] (rest-call 'get url (auth-headers)))
	          (unless status
	            (error body))
	          (when (hash-table? body)
	            (dp (hash->list body))
	            (let-hash body
		            (let ((posts (hash-keys .?posts)))
		              (for (item (reverse .?order))
		                (let ((item-sym (string->symbol item)))
		                  (when (member item-sym posts)
			                  (let-hash (hash-ref .?posts item-sym)
			                    (let ((dt (date->string (epoch->date (inexact (* .create_at .001))) "~Y-~m-~d ~H:~M:~S")))
			                      (if (and
				                          (hash-table? .props)
				                          (hash-get .props 'webhook_display_name))
			                        (let-hash .props
				                        (set! outs (cons [ dt
						                                       .?override_username
						                                       (format "~a ~a"
							                                             .?webhook_display_name
							                                             (let-hash (car .attachments)
							                                               (format "~a ~a" .text .fallback))) ] outs)))
      			                  (set! outs (cons [ dt (id->username .?user_id) (lines-to-spaces .message) ] outs))))
			                    )))))
                (when (< (any->int page) max-page)
                  (dp (format "page is ~a less than ~a" page max-page))
                  (lp (1+ (any->int page)))))))))
      (style-output outs .?style))))

(def (whisper channel user message)
  "Post a message to a channel"
  (let-hash (load-config)
    (let ((outs [[ "Message" "Reply Count" "Channel" "User Id" "Pinned?" ]])
	        (url (format "https://~a/api/v4/posts" .server))
	        (data (my-json-object->string
		             (hash
		              ("user_id" (user->id user))
		              ("post" (hash
			                     ("channel_id" (channel->id channel))
			                     ("message" message)))))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
	      (unless status
	        (error body))
	      (when (hash-table? body)
	        (let-hash body
	          (set! outs (cons [
			                        .message
			                        (hash->list .metadata)
			                        ] outs))))))))

(def (search pattern)
  "Search for users named pattern"
  (let-hash (load-config)
    (let* ((outs [[ "User Id" "Message" "Reply Count" "Channel" "Pinned?" ]])
           (url (format "https://~a/api/v4/teams/~a/posts/search" .server (get-team-id)))
	         (data (my-json-object->string
		             (hash
		              ("terms" pattern)
		              ("include_deleted_channels" #f)
		              ("is_or_search" #f)
		              ("per_page" 100)
		              ))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
	      (unless status
	        (error body))
	      (when (hash-table? body)
	        (let-hash body
	          (when (list? .order)
	            (for (order .order)
		            (let-hash (id->post order)
		              (set! outs (cons [
				                            (id->username .?user_id)
				                            .?message
				                            .?reply_count
				                            (id->channel .?channel_id)
				                            .?is_pinned
				                            ] outs))))))))
      (style-output outs .?style))))

(def (user pattern)
  "Search for users named pattern"
  (let-hash (load-config)
    (let ((outs [[ "username" "id" "email" "position" "First Name" "Last Name" "created_at" "updated_at" ]])
	        (url (format "https://~a/api/v4/users/search" .server))
	        (data (my-json-object->string
		             (hash
		              ("term" pattern)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
	      (unless status
	        (error body))
	      (when (list? body)
	        (for (user body)
	          (let-hash user
	            (set! outs (cons [ .?username .?id .?email .?position .?first_name .?last_name .?create_at .?update_at ] outs))))))
      (style-output outs .?style))))

;; Channels

(def (channel->id name)
  "Search for channel named pattern"
  (let-hash (load-config)
    (let ((outs [[  ]])
          (url (format "https://~a/api/v4/teams/~a/channels/name/~a" .server (get-team-id) name)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (when (hash-table? body)
	        (let-hash body
	          .id))))))

(def (user->id name)
  "Search for channel named pattern"
  (let-hash (load-config)
    (let ((outs [[  ]])
	        (url (format "https://~a/api/v4/users/username/~a" .server name)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
	      (when (hash-table? body)
	        (let-hash body
	          .id))))))

;; teams
(def (get-team-id)
  "Get primary team I'm on"
  (let ((body (get-teams)))
    (let-hash (car body)
      .?id)))

(def (get-teams)
  "List teams you are in"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams" .server)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
        body))))

(def (teams)
  "List all teams"
  (let-hash (load-config)
    (let ((outs [[ "Name" "Display" "Open Invite?" "Id" ]])
	        (body (get-teams)))
	    (when (list? body)
	      (for (team body)
	        (let-hash team
	          (set! outs (cons [ .?name .?display_name .?allow_open_invite .?id ] outs)))))
    (style-output outs .?style))))


(def (channels)
  "List all channels"
  (let-hash (load-config)
    (let* ((outs [[ "Name" "Display" "Purpose" "Msg Count" "Updated at" ]])
           (user-id (email->id .email))
           (team-id (get-team-id))
           (url (format "https://~a/api/v4/users/~a/teams/~a/channels" .server user-id team-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
	      (unless status
	        (error body))
        (dp body)
        (when (list? body)
          (for (channel body)
            (dp (hash->string channel))
	          (let-hash channel
	            (set! outs (cons [
				                        .name
				                        .display_name
				                        (org-table-safe .purpose)
				                        .total_msg_count
				                        .update_at
				                        ] outs))))))
      (style-output outs .?style))))

;; Config functions
(def (load-config)
  (let ((config (hash))
        (config-data (yaml-load config-file)))
    (unless (and (list? config-data)
                 (length>n? config-data 0)
                 (hash-table? (car config-data)))
      (displayln (format "Could not parse your config ~a list:~a length:~a table:~a"
                         config-file
                         (list? config-data)
                         (length>n? config-data 0)
                         (hash-table? (car config-data))))
      (exit 2))

    (let ((data (car config-data)))
      (when (hash-table? data)
        (hash-for-each
         (lambda (k v)
           (hash-put! config (string->symbol k) v))
         data))
      (let-hash config
        (when (and .?key .?iv .?password)
	        (hash-put! config 'password (get-password-from-config .key .iv .password)))
        (hash-put! config 'style (or .?style "org-mode"))
        (when .?secrets
	        (let-hash (u8vector->object (base64-decode .secrets))
	          (let ((password (get-password-from-config .key .iv .password)))
	            (hash-put! config 'token password))))
	      config))))


(def (default-headers)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ])

(def (config)
  (let-hash (load-config)
    (displayln "Please enter your mattermost password? (will not echo) :")
    (let* ((password (read-password ##console-port))
	         (cipher (make-aes-256-ctr-cipher))
	         (iv (random-bytes (cipher-iv-length cipher)))
	         (key (random-bytes (cipher-key-length cipher)))
	         (encrypted-password (encrypt cipher key iv password))
	         (enc-pass-store (u8vector->base64-string encrypted-password))
	         (iv-store (u8vector->base64-string iv))
	         (key-store (u8vector->base64-string key))
	         (secrets (base64-encode (object->u8vector
				                            (hash
				                             (password enc-pass-store)
				                             (iv iv-store)
				                             (key key-store))))))
      (displayln "Add the following secrets line to your " config-file)
      (displayln "")
      (displayln "secrets: " secrets))))

(def (get-password-from-config key iv password)
  (bytes->string
   (decrypt
    (make-aes-256-ctr-cipher)
    (base64-string->u8vector key)
    (base64-string->u8vector iv)
    (base64-string->u8vector password))))

(def (auth-headers)
  [
   ["Accept" :: "*/*"]
   ["Content-type" :: "application/json"]
   ["Authorization" :: (format "bearer ~a" (get-token)) ]
   ])

(def (org-table-safe str)
  (if (string? str)
    (pregexp-replace* "\\|" str "-")
    str))

;; ============================================================================
;; BOTS API
;; ============================================================================

(def (create-bot username display-name (description ""))
  "Create a bot"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/bots" .server))
          (data (my-json-object->string
                 (hash
                  ("username" username)
                  ("display_name" display-name)
                  ("description" description)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-bots (page 0) (per-page 60))
  "List bots"
  (let-hash (load-config)
    (let ((outs [[ "Username" "Display Name" "Description" "Owner Id" "Id" ]])
          (url (format "https://~a/api/v4/bots?page=~a&per_page=~a" .server page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (bot body)
            (let-hash bot
              (set! outs (cons [ .?username .?display_name .?description .?owner_id .?user_id ] outs)))))
        (style-output outs .?style)))))

(def (get-bot bot-id)
  "Get a bot by id"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/bots/~a" .server bot-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (patch-bot bot-id username display-name (description ""))
  "Patch a bot"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/bots/~a" .server bot-id))
          (data (my-json-object->string
                 (hash
                  ("username" username)
                  ("display_name" display-name)
                  ("description" description)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (disable-bot bot-id)
  "Disable a bot"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/bots/~a/disable" .server bot-id))
          (data (my-json-object->string (hash))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (enable-bot bot-id)
  "Enable a bot"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/bots/~a/enable" .server bot-id))
          (data (my-json-object->string (hash))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (assign-bot bot-id user-id)
  "Assign a bot to a user"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/bots/~a/assign/~a" .server bot-id user-id))
          (data (my-json-object->string (hash))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; FILES API
;; ============================================================================

(def (upload-file filepath channel-id)
  "Upload a file"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/files?channel_id=~a" .server channel-id)))
      (with ([ status body ] (rest-call 'post url (auth-headers)
                                        (call-with-input-file filepath
                                          (lambda (p) (read-all-as-string p)))))
        (unless status
          (error body))
        body))))

(def (get-file file-id)
  "Get a file"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/files/~a" .server file-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-file-info file-id)
  "Get metadata for a file"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/files/~a/info" .server file-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-file-link file-id)
  "Get a public file link"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/files/~a/link" .server file-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-file-thumbnail file-id)
  "Get a file's thumbnail"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/files/~a/thumbnail" .server file-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-file-preview file-id)
  "Get a file's preview"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/files/~a/preview" .server file-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; REACTIONS API
;; ============================================================================

(def (create-reaction user-id post-id emoji-name)
  "Create a reaction"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/reactions" .server))
          (data (my-json-object->string
                 (hash
                  ("user_id" user-id)
                  ("post_id" post-id)
                  ("emoji_name" emoji-name)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-reactions post-id)
  "Get reactions for a post"
  (let-hash (load-config)
    (let ((outs [[ "User Id" "Post Id" "Emoji" "Created" ]])
          (url (format "https://~a/api/v4/posts/~a/reactions" .server post-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (reaction body)
            (let-hash reaction
              (set! outs (cons [ .?user_id .?post_id .?emoji_name .?create_at ] outs)))))
        (style-output outs .?style)))))

(def (delete-reaction user-id post-id emoji-name)
  "Delete a reaction"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/posts/~a/reactions/~a"
                       .server user-id post-id emoji-name)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; STATUS API
;; ============================================================================

(def (get-user-status user-id)
  "Get user status"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/status" .server user-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (update-user-status user-id status-val)
  "Update user status (online, away, dnd, offline)"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/status" .server user-id))
          (data (my-json-object->string
                 (hash ("status" status-val)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-users-statuses user-ids)
  "Get statuses by ids"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/status/ids" .server))
          (data (my-json-object->string user-ids)))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (set-custom-status user-id emoji text (expires-at ""))
  "Set custom status"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/status/custom" .server user-id))
          (data (my-json-object->string
                 (hash
                  ("emoji" emoji)
                  ("text" text)
                  ("expires_at" expires-at)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (unset-custom-status user-id)
  "Unset custom status"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/status/custom" .server user-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-recent-custom-statuses user-id)
  "Get recent custom statuses"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/status/custom/recent" .server user-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; PREFERENCES API
;; ============================================================================

(def (get-preferences user-id)
  "Get user's preferences"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/preferences" .server user-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (update-preferences user-id preferences)
  "Update user's preferences"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/preferences" .server user-id))
          (data (my-json-object->string preferences)))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (delete-preferences user-id preferences)
  "Delete user's preferences"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/preferences/delete" .server user-id))
          (data (my-json-object->string preferences)))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-preference-category user-id category)
  "Get preference category"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/preferences/~a" .server user-id category)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; WEBHOOKS API
;; ============================================================================

(def (get-incoming-webhooks (page 0) (per-page 60))
  "List incoming webhooks"
  (let-hash (load-config)
    (let ((outs [[ "Display Name" "Channel Id" "Team Id" "Id" ]])
          (url (format "https://~a/api/v4/hooks/incoming?page=~a&per_page=~a"
                       .server page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (hook body)
            (let-hash hook
              (set! outs (cons [ .?display_name .?channel_id .?team_id .?id ] outs)))))
        (style-output outs .?style)))))

(def (create-incoming-webhook channel-id display-name (description ""))
  "Create an incoming webhook"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/incoming" .server))
          (data (my-json-object->string
                 (hash
                  ("channel_id" channel-id)
                  ("display_name" display-name)
                  ("description" description)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-incoming-webhook hook-id)
  "Get an incoming webhook"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/incoming/~a" .server hook-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (update-incoming-webhook hook-id channel-id display-name (description ""))
  "Update an incoming webhook"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/incoming/~a" .server hook-id))
          (data (my-json-object->string
                 (hash
                  ("id" hook-id)
                  ("channel_id" channel-id)
                  ("display_name" display-name)
                  ("description" description)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (delete-incoming-webhook hook-id)
  "Delete an incoming webhook"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/incoming/~a" .server hook-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-outgoing-webhooks (page 0) (per-page 60))
  "List outgoing webhooks"
  (let-hash (load-config)
    (let ((outs [[ "Display Name" "Channel Id" "Team Id" "Id" ]])
          (url (format "https://~a/api/v4/hooks/outgoing?page=~a&per_page=~a"
                       .server page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (hook body)
            (let-hash hook
              (set! outs (cons [ .?display_name .?channel_id .?team_id .?id ] outs)))))
        (style-output outs .?style)))))

(def (create-outgoing-webhook team-id display-name trigger-words callback-urls (channel-id ""))
  "Create an outgoing webhook"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/outgoing" .server))
          (data (my-json-object->string
                 (hash
                  ("team_id" team-id)
                  ("display_name" display-name)
                  ("trigger_words" trigger-words)
                  ("callback_urls" callback-urls)
                  ("channel_id" channel-id)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-outgoing-webhook hook-id)
  "Get an outgoing webhook"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/outgoing/~a" .server hook-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (delete-outgoing-webhook hook-id)
  "Delete an outgoing webhook"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/outgoing/~a" .server hook-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (regen-outgoing-webhook-token hook-id)
  "Regenerate outgoing webhook token"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/hooks/outgoing/~a/regen_token" .server hook-id))
          (data (my-json-object->string (hash))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; COMMANDS API
;; ============================================================================

(def (list-commands (team-id #f))
  "List commands for a team"
  (let-hash (load-config)
    (let* ((outs [[ "Trigger" "Display Name" "Description" "Team Id" "Id" ]])
          (tid (or team-id (get-team-id)))
          (url (format "https://~a/api/v4/commands?team_id=~a" .server tid)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (cmd body)
            (let-hash cmd
              (set! outs (cons [ .?trigger .?display_name .?description .?team_id .?id ] outs)))))
        (style-output outs .?style)))))

(def (create-command team-id trigger url method (username "") (icon-url ""))
  "Create a command"
  (let-hash (load-config)
    (let ((api-url (format "https://~a/api/v4/commands" .server))
          (data (my-json-object->string
                 (hash
                  ("team_id" team-id)
                  ("trigger" trigger)
                  ("url" url)
                  ("method" method)
                  ("username" username)
                  ("icon_url" icon-url)))))
      (with ([ status body ] (rest-call 'post api-url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-command command-id)
  "Get a command"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/commands/~a" .server command-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (update-command command-id trigger url method (username "") (icon-url ""))
  "Update a command"
  (let-hash (load-config)
    (let ((api-url (format "https://~a/api/v4/commands/~a" .server command-id))
          (data (my-json-object->string
                 (hash
                  ("id" command-id)
                  ("trigger" trigger)
                  ("url" url)
                  ("method" method)
                  ("username" username)
                  ("icon_url" icon-url)))))
      (with ([ status body ] (rest-call 'put api-url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (delete-command command-id)
  "Delete a command"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/commands/~a" .server command-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (execute-command channel-id command)
  "Execute a command"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/commands/execute" .server))
          (data (my-json-object->string
                 (hash
                  ("channel_id" channel-id)
                  ("command" command)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; EXTENDED USERS API
;; ============================================================================

(def (create-user email username password (first-name "") (last-name ""))
  "Create a user"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users" .server))
          (data (my-json-object->string
                 (hash
                  ("email" email)
                  ("username" username)
                  ("password" password)
                  ("first_name" first-name)
                  ("last_name" last-name)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (delete-user user-id)
  "Delete a user"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a" .server user-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (update-user user-id email username first-name last-name)
  "Update a user"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a" .server user-id))
          (data (my-json-object->string
                 (hash
                  ("id" user-id)
                  ("email" email)
                  ("username" username)
                  ("first_name" first-name)
                  ("last_name" last-name)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (patch-user user-id updates)
  "Patch a user with updates hash"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/patch" .server user-id))
          (data (my-json-object->string updates)))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-user-by-email-ext email)
  "Get a user by email (extended)"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/email/~a" .server email)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-users-by-ids user-ids)
  "Get users by ids"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/ids" .server))
          (data (my-json-object->string user-ids)))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (update-user-roles user-id roles)
  "Update user roles"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/roles" .server user-id))
          (data (my-json-object->string (hash ("roles" roles)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (update-user-active user-id active)
  "Activate or deactivate a user"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/active" .server user-id))
          (data (my-json-object->string (hash ("active" active)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (update-user-password user-id current-password new-password)
  "Update user password"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/password" .server user-id))
          (data (my-json-object->string
                 (hash
                  ("current_password" current-password)
                  ("new_password" new-password)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-user-sessions user-id)
  "Get user sessions"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/sessions" .server user-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (revoke-user-session user-id session-id)
  "Revoke a user session"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/sessions/revoke" .server user-id))
          (data (my-json-object->string (hash ("session_id" session-id)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-user-audit user-id (page 0) (per-page 60))
  "Get user audit"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/users/~a/audits?page=~a&per_page=~a"
                       .server user-id page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; EXTENDED TEAMS API
;; ============================================================================

(def (create-team name display-name type)
  "Create a team"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams" .server))
          (data (my-json-object->string
                 (hash
                  ("name" name)
                  ("display_name" display-name)
                  ("type" type)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (delete-team team-id)
  "Delete a team"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a" .server team-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (update-team team-id name display-name description)
  "Update a team"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a" .server team-id))
          (data (my-json-object->string
                 (hash
                  ("id" team-id)
                  ("name" name)
                  ("display_name" display-name)
                  ("description" description)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (patch-team team-id updates)
  "Patch a team"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a/patch" .server team-id))
          (data (my-json-object->string updates)))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-team-by-name team-name)
  "Get a team by name"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/name/~a" .server team-name)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-team-stats team-id)
  "Get team statistics"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a/stats" .server team-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-team-members team-id (page 0) (per-page 60))
  "Get team members"
  (let-hash (load-config)
    (let ((outs [[ "User Id" "Team Id" "Roles" ]])
          (url (format "https://~a/api/v4/teams/~a/members?page=~a&per_page=~a"
                       .server team-id page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (member body)
            (let-hash member
              (set! outs (cons [ .?user_id .?team_id .?roles ] outs)))))
        (style-output outs .?style)))))

(def (add-team-member team-id user-id)
  "Add a team member"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a/members" .server team-id))
          (data (my-json-object->string
                 (hash
                  ("team_id" team-id)
                  ("user_id" user-id)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (remove-team-member team-id user-id)
  "Remove a team member"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a/members/~a" .server team-id user-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-team-member team-id user-id)
  "Get a team member"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a/members/~a" .server team-id user-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; EXTENDED CHANNELS API
;; ============================================================================

(def (create-channel team-id name display-name type (purpose "") (header ""))
  "Create a channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels" .server))
          (data (my-json-object->string
                 (hash
                  ("team_id" team-id)
                  ("name" name)
                  ("display_name" display-name)
                  ("type" type)
                  ("purpose" purpose)
                  ("header" header)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (create-direct-channel user-ids)
  "Create a direct message channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/direct" .server))
          (data (my-json-object->string user-ids)))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (create-group-channel user-ids)
  "Create a group message channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/group" .server))
          (data (my-json-object->string user-ids)))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (delete-channel channel-id)
  "Delete a channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a" .server channel-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (update-channel channel-id name display-name purpose header)
  "Update a channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a" .server channel-id))
          (data (my-json-object->string
                 (hash
                  ("id" channel-id)
                  ("name" name)
                  ("display_name" display-name)
                  ("purpose" purpose)
                  ("header" header)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (patch-channel channel-id updates)
  "Patch a channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/patch" .server channel-id))
          (data (my-json-object->string updates)))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (restore-channel channel-id)
  "Restore a deleted channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/restore" .server channel-id))
          (data (my-json-object->string (hash))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-channel-by-name team-id channel-name)
  "Get a channel by name"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a/channels/name/~a"
                       .server team-id channel-name)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (search-channels team-id term)
  "Search channels"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/teams/~a/channels/search" .server team-id))
          (data (my-json-object->string (hash ("term" term)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-channel-members channel-id (page 0) (per-page 60))
  "Get channel members"
  (let-hash (load-config)
    (let ((outs [[ "User Id" "Channel Id" "Roles" ]])
          (url (format "https://~a/api/v4/channels/~a/members?page=~a&per_page=~a"
                       .server channel-id page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        (when (list? body)
          (for (member body)
            (let-hash member
              (set! outs (cons [ .?user_id .?channel_id .?roles ] outs)))))
        (style-output outs .?style)))))

(def (add-channel-member channel-id user-id)
  "Add a channel member"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/members" .server channel-id))
          (data (my-json-object->string (hash ("user_id" user-id)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (remove-channel-member channel-id user-id)
  "Remove a channel member"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/members/~a"
                       .server channel-id user-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-channel-stats channel-id)
  "Get channel statistics"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/stats" .server channel-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-pinned-posts channel-id)
  "Get pinned posts for channel"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/pinned" .server channel-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

;; ============================================================================
;; EXTENDED POSTS API
;; ============================================================================

(def (create-post channel-id message (root-id ""))
  "Create a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts" .server))
          (data (my-json-object->string
                 (hash
                  ("channel_id" channel-id)
                  ("message" message)
                  ("root_id" root-id)))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (update-post post-id message)
  "Update a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts/~a" .server post-id))
          (data (my-json-object->string
                 (hash
                  ("id" post-id)
                  ("message" message)))))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (delete-post post-id)
  "Delete a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts/~a" .server post-id)))
      (with ([ status body ] (rest-call 'delete url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (patch-post post-id updates)
  "Patch a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts/~a/patch" .server post-id))
          (data (my-json-object->string updates)))
      (with ([ status body ] (rest-call 'put url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (get-post-thread post-id)
  "Get a post thread"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts/~a/thread" .server post-id)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-posts-since channel-id time)
  "Get posts since a time"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/posts?since=~a"
                       .server channel-id time)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-posts-before channel-id post-id (page 0) (per-page 60))
  "Get posts before a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/posts?before=~a&page=~a&per_page=~a"
                       .server channel-id post-id page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (get-posts-after channel-id post-id (page 0) (per-page 60))
  "Get posts after a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/channels/~a/posts?after=~a&page=~a&per_page=~a"
                       .server channel-id post-id page per-page)))
      (with ([ status body ] (rest-call 'get url (auth-headers)))
        (unless status
          (error body))
        body))))

(def (pin-post post-id)
  "Pin a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts/~a/pin" .server post-id))
          (data (my-json-object->string (hash))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))

(def (unpin-post post-id)
  "Unpin a post"
  (let-hash (load-config)
    (let ((url (format "https://~a/api/v4/posts/~a/unpin" .server post-id))
          (data (my-json-object->string (hash))))
      (with ([ status body ] (rest-call 'post url (auth-headers) data))
        (unless status
          (error body))
        body))))
