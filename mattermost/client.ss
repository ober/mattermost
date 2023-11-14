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
 :std/sugar
 :std/text/base64
 :std/text/json
 :clan/text/yaml)

(export #t)
(declare (not optimize-dead-definitions))

(def version "0.01")

(def program-name "mattermost")
(def config-file (format "~~/.~a.yaml" program-name))

(def user-list (hash))
(def channel-list (hash))

(def (get-token)
     (let-hash (load-config)
	       (let ((token "")
		     (url (format "https://~a/api/v4/users/login" .server))
		     (data (json-object->string
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
		     (url (format "https://~a/api/v4/users/~a/channels/~a/posts/unread" .server .user_id (channel->id channel))))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (let-hash body
				 (when (table? .?posts)
				   (hash-for-each
				    (lambda (k v)
				      (when (table? v)
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
		     (url (format "https://~a/api/v4/users/~a/teams/unread" .server .user_id)))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (pi (car body))))))

(def (privates)
     "Get a page of private channels on a team based on query string"
     (let-hash (load-config)
	       (let ((outs [[ "stuff" ]])
		     (url (format "https://~a/api/v4/teams/~a/channels/private" .server .team_id)))
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
		     (url (format "https://~a/api/v4/teams/~a/groups" .server .team_id)))
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
			    (when (table? emoji)
			      (let-hash emoji
					(set! outs (cons [ .?name (hash->list (id->user .?creator_id)) .?updated_at .?created_at .?deleted_at .?id ] outs))))))
		 (style-output outs .?style))))

(def (users)
     "List users"
     (let-hash (load-config)
	       (let ((outs [[ "username" "id" "email" "position" "First Name" "Last Name" "created_at" "updated_at" ]])
		     (url (format "https://~a/api/v4/users" .server)))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (when (list? body)
			 (for (user body)
			      (pi user)
			      (let-hash user
					(set! outs (cons [ .?username .?id .?email .?position .?first_name .?last_name .?create_at .?update_at ] outs))))))
		 (style-output outs .?style))))

(def (userinfo user)
     "List users"
     (let-hash (load-config)
	       (let ((outs [[ "Username" "First Name" "Last Name" "Nickname" "Position" "Updated" "Roles" "Id" ]])
		     (url (format "https://~a/api/v4/users/username/~a" .server user)))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (when (table? body)
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
     (let ((user (id->user id)))
       (when (table? user)
	 (let-hash user
		   (or .?username .email)))))

(def (id->channel id)
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
		     (data (json-object->string
			    (hash
			     ("channel_id" (channel->id channel))
			     ("message" message)
			     ))))
		 (with ([ status body ] (rest-call 'post url (auth-headers) data))
		       (unless status
			 (error body))
		       (when (table? body)
			 (let-hash body
				   (set! outs (cons [
						     .message
						     (hash->list .metadata)
						     ] outs))))))))

(def (posts channel)
     "Get posts for a channel"
     (let-hash (load-config)
	       (let* ((outs [[ "User" "Message" ]])
		      (channel-id (channel->id channel))
		      (url (format "https://~a/api/v4/channels/~a/posts" .server channel-id)))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (when (table? body)
			 (let-hash body
				   (let ((posts (hash-keys .?posts)))
		  		     (for (item (reverse .?order))
					  (let ((item-sym (string->symbol item)))
					    (when (member item-sym posts)
					      (let-hash (hash-ref .?posts item-sym)
							(set! outs (cons [ (id->username .?user_id) .message ] outs))))))))))
		 (style-output outs .?style))))

(def (whisper channel user message)
     "Post a message to a channel"
     (let-hash (load-config)
	       (let ((outs [[ "Message" "Reply Count" "Channel" "User Id" "Pinned?" ]])
		     (url (format "https://~a/api/v4/posts" .server))
		     (data (json-object->string
			    (hash
			     ("user_id" (user->id user))
			     ("post" (hash
				      ("channel_id" (channel->id channel))
				      ("message" message)))))))
		 (with ([ status body ] (rest-call 'post url (auth-headers) data))
		       (unless status
			 (error body))
		       (when (table? body)
			 (let-hash body
				   (set! outs (cons [
						     .message
						     (hash->list .metadata)
						     ] outs))))))))

(def (search pattern)
     "Search for users named pattern"
     (let-hash (load-config)
	       (let ((outs [[ "Message" "Reply Count" "Channel" "User Id" "Pinned?" ]])
		     (url (format "https://~a/api/v4/teams/~a/posts/search" .server .team_id))
		     (data (json-object->string
			    (hash
			     ("terms" pattern)
			     ("include_deleted_channels" #f)
			     ("is_or_search" #f)
			     ("per_page" 100)
			     ))))
		 (with ([ status body ] (rest-call 'post url (auth-headers) data))
		       (unless status
			 (error body))
		       (when (table? body)
			 (let-hash body
				   (when (list? .order)
				     (for (order .order)
					  (let-hash (id->post order)
						    (set! outs (cons [
								      .?message
								      .?reply_count
								      (let-hash (id->channel .?channel_id) .?display_name)
								      (let-hash (id->user .?user_id) (or .?username .email))
								      .?is_pinned
								      ] outs))))))))
		 (style-output outs .?style))))


(def (user pattern)
     "Search for users named pattern"
     (let-hash (load-config)
	       (let ((outs [[ "username" "id" "email" "position" "First Name" "Last Name" "created_at" "updated_at" ]])
		     (url (format "https://~a/api/v4/users/search" .server))
		     (data (json-object->string
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
		     (url (format "https://~a/api/v4/teams/~a/channels/name/~a" .server .team_id name)))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (when (table? body)
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
		       (when (table? body)
			 (let-hash body
				   .id))))))

;; teams


(def (teams)
     "List all teams"
     (let-hash (load-config)
	       (let ((outs [[ "Name" "Display" "Open Invite?" "Id" ]])
		     (url (format "https://~a/api/v4/teams" .server)))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (when (list? body)
			 (for (team body)
			      (let-hash team
					(set! outs (cons [ .?name .?display_name .?allow_open_invite .?id ] outs))))))
		 (style-output outs .?style))))


(def (channels)
     "List all channels"
     (let-hash (load-config)
	       (let ((outs [[ "Name" "Display" "Purpose" "Msg Count" "Updated at" ]])
		     (url (format "https://~a/api/v4/users/~a/teams/~a/channels" .server .user_id .team_id)))
		 (with ([ status body ] (rest-call 'get url (auth-headers)))
		       (unless status
			 (error body))
		       (when (list? body)
			 (for (channel body)
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
                    (table? (car config-data)))
	 (displayln (format "Could not parse your config ~a list:~a length:~a table:~a"
                            config-file
                            (list? config-data)
                            (length>n? config-data 0)
                            (table? (car config-data))))
	 (exit 2))

       (hash-for-each
	(lambda (k v)
	  (hash-put! config (string->symbol k) v))
	(car config-data))

       (let-hash config
		 (when (and .?key .?iv .?password)
		   (hash-put! config 'password (get-password-from-config .key .iv .password)))
		 (hash-put! config 'style (or .?style "org-mode"))
		 (when .?secrets
		   (let-hash (u8vector->object (base64-decode .secrets))
			     (let ((password (get-password-from-config .key .iv .password)))
			       (hash-put! config 'token password)
			       config))))))

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
