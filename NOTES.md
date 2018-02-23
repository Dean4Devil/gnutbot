# Dev Notes

## Initial ToDo

- Make it work.

Stuff I want for v0.1.0.0:
- [ ] Global FRP based event tree
- [ ] RBAC based authorization
- [ ]   YAML backend authorization
- [ ] Gracefully degradation if now allowed to access JIDs.
- [ ] Initial Commands
- [ ]   ping/pong command with latency check
- [ ]   echo command
- [ ]   quotes
- [ ]   polls

Stuff I want for v1.0.0.0:
- [ ] Global and Per-Channel FRP event tree
- [ ] Plugin system
- [ ]   Global un-/loadable plugins
- [ ]   Per channel un-/loadable plugins
- [ ] Join Channels on invite
- [ ]   only if invited by channel admin/owner
- [ ]   only if invited by specific Gnut group
- [ ] Control program to allow sending commands to gnut via console
- [ ]   system administration (shutdown, restart)
- [ ] Allow external (push-based) triggers for plugins
- [ ]   Message Queue
- [ ]   UNIX Socket
- [ ] Asyncronous (pull-based) triggers for plugins
- [ ]   Atom Feeds
- [ ] Everything that is possible to write as plugin has been written as Plugin.


Stuff I want for v9999:
- [ ] Dynamically (GHCi) un-/loadable plugins
