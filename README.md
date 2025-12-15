# Mattermost client in Gerbil

A comprehensive Mattermost API client library and CLI tool written in Gerbil Scheme.

## Setup

### 1. Create configuration file

Create a `~/.mattermost.yaml` file with your server information:

```yaml
---
server: mattermost.example.com
email: you@example.com
```

### 2. Configure password

Run `config` to setup your encrypted password:

```sh
./mattermost config
```

It will prompt you for your password, and then output the encrypted key.
Append this to your `~/.mattermost.yaml` as a single line.

Your final config should look like:

```yaml
---
server: mattermost.example.com
email: you@example.com
secrets: UGRkrgByZGSxAHFycHBklgByIgdfX2NsYXNzAdgAN5wALxDYAFFoCF9fb2JqZWN0UXt7e3t7e3t7e3t7e1JWqQCrAFBkZK4AZNgAcmRkaA1kaXJlY3Qtc3VwZXJzcmRkzwBxcnBwLxgGY2xlY...
```

## Usage

The Mattermost CLI provides comprehensive coverage of the Mattermost API v4. Below are all available commands organized by category.

### Authentication & Configuration

- `config` - Set your password
- `get-token` - Get authentication token

### Users

**List and Search:**
- `users` - List all users in your team
- `user <term>` - Search for users matching term
- `userinfo <username>` - Get detailed user information

**User Management:**
- `create-user <email> <username> <password>` - Create a new user
- `delete-user <user-id>` - Delete a user
- `update-user-active <user-id> <true|false>` - Activate or deactivate a user

**User Status:**
- `get-user-status <user-id>` - Get user's current status
- `update-user-status <user-id> <status>` - Update status (online/away/dnd/offline)
- `set-custom-status <user-id> <emoji> <text>` - Set custom status

**User Preferences:**
- `get-preferences <user-id>` - Get user's preferences

### Teams

**List and Info:**
- `teams` - List all your teams
- `get-team-id` - Get your primary team ID
- `get-team-stats <team-id>` - Get team statistics

**Team Management:**
- `create-team <name> <display-name> <type>` - Create a team (type: O=open, I=invite)
- `delete-team <team-id>` - Delete a team

**Team Members:**
- `get-team-members <team-id>` - List team members
- `add-team-member <team-id> <user-id>` - Add a member to team

### Channels

**List and Search:**
- `channels` - List all channels you're in
- `privates` - List private channels
- `search-channels <team-id> <term>` - Search for channels

**Channel Management:**
- `create-channel <team-id> <name> <display-name> <type>` - Create channel (type: O=open, P=private)
- `delete-channel <channel-id>` - Delete a channel
- `get-channel-stats <channel-id>` - Get channel statistics

**Channel Members:**
- `get-channel-members <channel-id>` - List channel members
- `add-channel-member <channel-id> <user-id>` - Add member to channel

**Channel Messages:**
- `unread <channel>` - Show unread messages in channel
- `unreads` - Show unread message counts across all channels

### Posts (Messages)

**Read Posts:**
- `posts <channel> <page> <recur>` - Fetch posts from channel
- `search <term>` - Search for posts containing term
- `get-post-thread <post-id>` - Get a complete post thread

**Create and Modify Posts:**
- `post <channel> <message>` - Post a message to channel
- `create-post <channel-id> <message>` - Create a post
- `update-post <post-id> <message>` - Update/edit a post
- `delete-post <post-id>` - Delete a post
- `whisper <user> <channel> <message>` - Send a direct message

**Post Actions:**
- `pin-post <post-id>` - Pin a post to channel
- `unpin-post <post-id>` - Unpin a post

### Reactions

- `create-reaction <user-id> <post-id> <emoji-name>` - Add reaction to post
- `get-reactions <post-id>` - Get all reactions on a post
- `delete-reaction <user-id> <post-id> <emoji-name>` - Remove a reaction

### Bots

**Bot Management:**
- `create-bot <username> <display-name> [description]` - Create a bot
- `get-bots [page] [per-page]` - List all bots
- `get-bot <bot-id>` - Get bot information
- `enable-bot <bot-id>` - Enable a bot
- `disable-bot <bot-id>` - Disable a bot

### Files

- `get-file <file-id>` - Download a file
- `get-file-info <file-id>` - Get file metadata
- `get-file-link <file-id>` - Get public file link

### Webhooks

**Incoming Webhooks:**
- `get-incoming-webhooks` - List incoming webhooks
- `create-incoming-webhook <channel-id> <display-name>` - Create incoming webhook

**Outgoing Webhooks:**
- `get-outgoing-webhooks` - List outgoing webhooks

### Slash Commands

- `list-commands` - List all slash commands
- `execute-command <channel-id> <command>` - Execute a slash command

### Emojis, Groups, and Plugins

- `emojis` - List custom emojis
- `groups` - List all groups
- `plugins` - Show plugin statuses

## Examples

### Basic Usage

```sh
# List all users
./mattermost users

# Search for a user
./mattermost user john

# Post a message
./mattermost post general "Hello, world!"

# Search for messages
./mattermost search "important announcement"

# Get unread messages
./mattermost unread general
```

### User Management

```sh
# Create a new user
./mattermost create-user alice@example.com alice SecurePass123

# Update user status
./mattermost update-user-status <user-id> away

# Set custom status
./mattermost set-custom-status <user-id> "🏖️" "On vacation"
```

### Team and Channel Management

```sh
# Create a new team
./mattermost create-team myteam "My Team" O

# Create a channel
./mattermost create-channel <team-id> engineering "Engineering" O

# Add member to channel
./mattermost add-channel-member <channel-id> <user-id>
```

### Bot Management

```sh
# Create a bot
./mattermost create-bot mybot "My Bot" "Automated assistant"

# List all bots
./mattermost get-bots

# Disable a bot
./mattermost disable-bot <bot-id>
```

### Reactions and Interactions

```sh
# Add a reaction
./mattermost create-reaction <user-id> <post-id> thumbsup

# Get all reactions on a post
./mattermost get-reactions <post-id>

# Pin an important post
./mattermost pin-post <post-id>
```

### Webhooks

```sh
# Create an incoming webhook
./mattermost create-incoming-webhook <channel-id> "GitHub Notifications"

# List webhooks
./mattermost get-incoming-webhooks
```

## API Coverage

This client implements comprehensive coverage of the Mattermost API v4, including:

- **Users API** - User management, authentication, sessions, preferences, status
- **Teams API** - Team creation, management, members, statistics
- **Channels API** - Channel CRUD operations, members, search
- **Posts API** - Message posting, threads, search, pinning
- **Reactions API** - Emoji reactions on posts
- **Files API** - File upload, download, metadata
- **Bots API** - Bot creation and management
- **Webhooks API** - Incoming and outgoing webhook management
- **Commands API** - Slash command execution and management
- **Emojis, Groups, Plugins** - Custom emojis, group management, plugin status

## Library Usage

You can also use this as a Gerbil library in your own code:

```scheme
(import :ober/mattermost/client)

;; Search for messages
(search "important")

;; Post a message
(post "general" "Hello from Gerbil!")

;; Get user info
(userinfo "alice")

;; Create a bot
(create-bot "mybot" "My Bot" "Automated helper")
```

## Building

```sh
make build
```

## License

See LICENSE file for details.
