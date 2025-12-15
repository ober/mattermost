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
  ;; Existing commands
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
  (def get-team-id
    (command 'get-team-id help: "Get id info for email"))

  ;; Bot commands
  (def create-bot
    (command 'create-bot help: "Create a bot"
             (argument 'username help: "Bot username")
             (argument 'display-name help: "Bot display name")
             (optional-argument 'description help: "Bot description")))
  (def get-bots
    (command 'get-bots help: "List bots"
             (optional-argument 'page help: "Page number")
             (optional-argument 'per-page help: "Per page")))
  (def get-bot
    (command 'get-bot help: "Get a bot"
             (argument 'bot-id help: "Bot ID")))
  (def disable-bot
    (command 'disable-bot help: "Disable a bot"
             (argument 'bot-id help: "Bot ID")))
  (def enable-bot
    (command 'enable-bot help: "Enable a bot"
             (argument 'bot-id help: "Bot ID")))

  ;; File commands
  (def get-file
    (command 'get-file help: "Get a file"
             (argument 'file-id help: "File ID")))
  (def get-file-info
    (command 'get-file-info help: "Get file metadata"
             (argument 'file-id help: "File ID")))
  (def get-file-link
    (command 'get-file-link help: "Get public file link"
             (argument 'file-id help: "File ID")))

  ;; Reaction commands
  (def create-reaction
    (command 'create-reaction help: "Create a reaction"
             (argument 'user-id help: "User ID")
             (argument 'post-id help: "Post ID")
             (argument 'emoji-name help: "Emoji name")))
  (def get-reactions
    (command 'get-reactions help: "Get reactions for a post"
             (argument 'post-id help: "Post ID")))
  (def delete-reaction
    (command 'delete-reaction help: "Delete a reaction"
             (argument 'user-id help: "User ID")
             (argument 'post-id help: "Post ID")
             (argument 'emoji-name help: "Emoji name")))

  ;; Status commands
  (def get-user-status
    (command 'get-user-status help: "Get user status"
             (argument 'user-id help: "User ID")))
  (def update-user-status
    (command 'update-user-status help: "Update user status"
             (argument 'user-id help: "User ID")
             (argument 'status help: "Status (online/away/dnd/offline)")))
  (def set-custom-status
    (command 'set-custom-status help: "Set custom status"
             (argument 'user-id help: "User ID")
             (argument 'emoji help: "Status emoji")
             (argument 'text help: "Status text")))

  ;; Preference commands
  (def get-preferences
    (command 'get-preferences help: "Get user preferences"
             (argument 'user-id help: "User ID")))

  ;; Webhook commands
  (def get-incoming-webhooks
    (command 'get-incoming-webhooks help: "List incoming webhooks"))
  (def create-incoming-webhook
    (command 'create-incoming-webhook help: "Create incoming webhook"
             (argument 'channel-id help: "Channel ID")
             (argument 'display-name help: "Display name")))
  (def get-outgoing-webhooks
    (command 'get-outgoing-webhooks help: "List outgoing webhooks"))

  ;; Command commands
  (def list-commands
    (command 'list-commands help: "List slash commands"))
  (def execute-command
    (command 'execute-command help: "Execute a slash command"
             (argument 'channel-id help: "Channel ID")
             (argument 'cmd help: "Command to execute")))

  ;; Extended user commands
  (def create-user
    (command 'create-user help: "Create a user"
             (argument 'email help: "Email")
             (argument 'username help: "Username")
             (argument 'password help: "Password")))
  (def delete-user
    (command 'delete-user help: "Delete a user"
             (argument 'user-id help: "User ID")))
  (def update-user-active
    (command 'update-user-active help: "Activate/deactivate user"
             (argument 'user-id help: "User ID")
             (argument 'active help: "true or false")))

  ;; Extended team commands
  (def create-team
    (command 'create-team help: "Create a team"
             (argument 'name help: "Team name")
             (argument 'display-name help: "Display name")
             (argument 'type help: "Team type (O=open, I=invite)")))
  (def delete-team
    (command 'delete-team help: "Delete a team"
             (argument 'team-id help: "Team ID")))
  (def get-team-stats
    (command 'get-team-stats help: "Get team statistics"
             (argument 'team-id help: "Team ID")))
  (def get-team-members
    (command 'get-team-members help: "Get team members"
             (argument 'team-id help: "Team ID")))
  (def add-team-member
    (command 'add-team-member help: "Add team member"
             (argument 'team-id help: "Team ID")
             (argument 'user-id help: "User ID")))

  ;; Extended channel commands
  (def create-channel
    (command 'create-channel help: "Create a channel"
             (argument 'team-id help: "Team ID")
             (argument 'name help: "Channel name")
             (argument 'display-name help: "Display name")
             (argument 'type help: "O=open, P=private")))
  (def delete-channel
    (command 'delete-channel help: "Delete a channel"
             (argument 'channel-id help: "Channel ID")))
  (def get-channel-stats
    (command 'get-channel-stats help: "Get channel statistics"
             (argument 'channel-id help: "Channel ID")))
  (def get-channel-members
    (command 'get-channel-members help: "Get channel members"
             (argument 'channel-id help: "Channel ID")))
  (def add-channel-member
    (command 'add-channel-member help: "Add channel member"
             (argument 'channel-id help: "Channel ID")
             (argument 'user-id help: "User ID")))
  (def search-channels
    (command 'search-channels help: "Search channels"
             (argument 'team-id help: "Team ID")
             (argument 'term help: "Search term")))

  ;; Extended post commands
  (def create-post
    (command 'create-post help: "Create a post"
             (argument 'channel-id help: "Channel ID")
             (argument 'message help: "Message")))
  (def delete-post
    (command 'delete-post help: "Delete a post"
             (argument 'post-id help: "Post ID")))
  (def update-post
    (command 'update-post help: "Update a post"
             (argument 'post-id help: "Post ID")
             (argument 'message help: "New message")))
  (def pin-post
    (command 'pin-post help: "Pin a post"
             (argument 'post-id help: "Post ID")))
  (def unpin-post
    (command 'unpin-post help: "Unpin a post"
             (argument 'post-id help: "Post ID")))
  (def get-post-thread
    (command 'get-post-thread help: "Get a post thread"
             (argument 'post-id help: "Post ID")))

  (call-with-getopt process-args args
		    program: program-name
		    help: "Mattermost cli"
		    ;; Existing
		    channels config get-token emojis groups posts post
		    plugins privates search teams unread unreads
		    user userinfo users get-team-id whisper
		    ;; Bots
		    create-bot get-bots get-bot disable-bot enable-bot
		    ;; Files
		    get-file get-file-info get-file-link
		    ;; Reactions
		    create-reaction get-reactions delete-reaction
		    ;; Status
		    get-user-status update-user-status set-custom-status
		    ;; Preferences
		    get-preferences
		    ;; Webhooks
		    get-incoming-webhooks create-incoming-webhook get-outgoing-webhooks
		    ;; Commands
		    list-commands execute-command
		    ;; Extended Users
		    create-user delete-user update-user-active
		    ;; Extended Teams
		    create-team delete-team get-team-stats get-team-members add-team-member
		    ;; Extended Channels
		    create-channel delete-channel get-channel-stats get-channel-members 
		    add-channel-member search-channels
		    ;; Extended Posts
		    create-post delete-post update-post pin-post unpin-post get-post-thread
		    ))

(def (process-args cmd opt)
  (let-hash opt
    (case cmd
      ;; Existing commands
      ((channels) (channels))
      ((config) (config))
      ((get-token) (get-token))
      ((emojis) (emojis))
      ((groups) (groups))
      ((posts) (posts .channel .page .recur))
      ((post) (post .channel .message))
      ((plugins) (plugins))
      ((privates) (privates))
      ((search) (search .term))
      ((teams) (teams))
      ((unread) (unread .channel))
      ((unreads) (unreads))
      ((user) (user .term))
      ((userinfo) (userinfo .term))
      ((users) (users))
      ((get-team-id) (get-team-id))
      ((whisper) (whisper .user .channel .message))
      
      ;; Bot commands
      ((create-bot) (create-bot .username .display-name (or .?description "")))
      ((get-bots) (get-bots (or .?page 0) (or .?per-page 60)))
      ((get-bot) (get-bot .bot-id))
      ((disable-bot) (disable-bot .bot-id))
      ((enable-bot) (enable-bot .bot-id))
      
      ;; File commands
      ((get-file) (get-file .file-id))
      ((get-file-info) (get-file-info .file-id))
      ((get-file-link) (get-file-link .file-id))
      
      ;; Reaction commands
      ((create-reaction) (create-reaction .user-id .post-id .emoji-name))
      ((get-reactions) (get-reactions .post-id))
      ((delete-reaction) (delete-reaction .user-id .post-id .emoji-name))
      
      ;; Status commands
      ((get-user-status) (get-user-status .user-id))
      ((update-user-status) (update-user-status .user-id .status))
      ((set-custom-status) (set-custom-status .user-id .emoji .text))
      
      ;; Preference commands
      ((get-preferences) (get-preferences .user-id))
      
      ;; Webhook commands
      ((get-incoming-webhooks) (get-incoming-webhooks))
      ((create-incoming-webhook) (create-incoming-webhook .channel-id .display-name))
      ((get-outgoing-webhooks) (get-outgoing-webhooks))
      
      ;; Command commands
      ((list-commands) (list-commands))
      ((execute-command) (execute-command .channel-id .cmd))
      
      ;; Extended user commands
      ((create-user) (create-user .email .username .password))
      ((delete-user) (delete-user .user-id))
      ((update-user-active) (update-user-active .user-id .active))
      
      ;; Extended team commands
      ((create-team) (create-team .name .display-name .type))
      ((delete-team) (delete-team .team-id))
      ((get-team-stats) (get-team-stats .team-id))
      ((get-team-members) (get-team-members .team-id))
      ((add-team-member) (add-team-member .team-id .user-id))
      
      ;; Extended channel commands
      ((create-channel) (create-channel .team-id .name .display-name .type))
      ((delete-channel) (delete-channel .channel-id))
      ((get-channel-stats) (get-channel-stats .channel-id))
      ((get-channel-members) (get-channel-members .channel-id))
      ((add-channel-member) (add-channel-member .channel-id .user-id))
      ((search-channels) (search-channels .team-id .term))
      
      ;; Extended post commands
      ((create-post) (create-post .channel-id .message))
      ((delete-post) (delete-post .post-id))
      ((update-post) (update-post .post-id .message))
      ((pin-post) (pin-post .post-id))
      ((unpin-post) (unpin-post .post-id))
      ((get-post-thread) (get-post-thread .post-id)))))
