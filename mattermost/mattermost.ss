;;; -*- Gerbil -*-
;;; © ober
;;; mattermost client binary

(import
  :gerbil/gambit
  :std/coroutine
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
  :clan/text/yaml
  :std/getopt
  :ober/oberlib
  :ober/mattermost/client)

(export main)
(def program-name "mattermost")

(def (main . args)
  (def channels
    (command 'channels help: "List All Channels."))
  (def config
    (command 'config help: "Set your password."))
  (def get-token
    (command 'get-token help: "Get token."))
  (def emojis
    (command 'emojis help: "List custom emojis."))
  (def groups
    (command 'groups help: "List all groups your user is in."))
  (def posts
    (command 'posts help: "Fetch posts for channel."
	     (argument 'channel help: "Fetch posts from channel")
	     (argument 'page help: "Initial page to display")
         (argument 'recur help: "How many pages to fetch")))
  (def post
    (command 'post help: "Post a message to a channel."
	     (argument 'channel help: "channel")
	     (argument 'message help: "Message to send")))
  (def plugins
    (command 'plugins help: "Plugin Statuses."))

  (def privates
    (command 'privates help: "List Private channels."))
  (def search
    (command 'search help: "Search for term in all of team."
	     (argument 'term help: "Term to search for")))
  (def teams
    (command 'teams help: "List all your teams."))
  (def unread
    (command 'unread help: "Show unread messages."
	     (argument 'channel help: "Channel")))
  (def unreads
    (command 'unreads help: "Show unread stats."))
  (def user
    (command 'user help: "Search for user matching term."
	     (argument 'term help: "User pattern")))
  (def userinfo
    (command 'userinfo help: "Return user info."
	     (argument 'term help: "User to return info on")))
  (def users
    (command 'users help: "List all the users."))
  (def whisper
    (command 'whisper help: "Whisper to user in channel."
	     (argument 'user)
	     (argument 'channel)
	     (argument 'messsage)))

  (def get-id-from-email
    (command 'get-id-from-email help: "Get id info for email"
        (argument 'email)))

  (call-with-getopt process-args args
		    program: program-name
		    help: "Mattermost cli"
		    channels
		    config
		    get-token
		    emojis
		    groups
		    posts
		    post
		    plugins
		    privates
		    search
		    teams
		    unread
		    unreads
		    user
		    userinfo
		    users
            get-id-from-email
		    whisper
		    ))

(def (process-args cmd opt)
  (let-hash opt
    (case cmd
      ((channels)
       (channels))
      ((config)
       (config))
      ((get-token)
       (get-token))
      ((emojis)
       (emojis))
      ((groups)
       (groups))
      ((posts)
       (posts .channel .page .recur))
      ((post)
       (post .channel .message))
      ((plugins)
       (plugins))
      ((privates)
       (privates))
      ((search)
       (search .term))
      ((teams)
       (teams))
      ((unread)
       (unread .channel))
      ((unreads)
       (unreads))
      ((user)
       (user .term))
      ((userinfo)
       (userinfo .term))
      ((users)
       (users))
      ((get-id-from-email)
       (get-id-from-email .email))
      ((whisper)
       (whisper .user .channel .message)))))
