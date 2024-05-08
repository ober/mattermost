# Mattermost client in Gerbil


## Minimal setup for use.

Create a `~/.mattermost.yaml` file with your information plugged in
``` yaml
---
server: mattermost.example.com
email: you@example.com
```

Run `config` to setup password.

``` sh
./mattermost config
```

It will prompt you for your password, and then output the encrypted key.
Append this to your `~/.mattermost.yaml` as a single line.

So it should look like

``` yaml
---
server: mattermost.example.com
email: you@example.com
secrets: UGRkrgByZGSxAHFycHBklgByIgdfX2NsYXNzAdgAN5wALxDYAFFoCF9fb2JqZWN0UXt7e3t7e3t7e3t7e1JWqQCrAFBkZK4AZNgAcmRkaA1kaXJlY3Qtc3VwZXJzcmRkzwBxcnBwLxgGY2xlY...
```
