# Dev Notes

## Initial ToDo

- Make it work.

Stuff I want for v0.1.0.0:
- [x] Global FRP based event tree
- [ ] RBAC based authorization
- [ ]   YAML backend authorization
- [ ] Gracefully degradation if not allowed to access JIDs.
- [ ] Initial Commands
- [ ]   ping/pong command with latency check
- [/]   echo command
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
- [ ] Per Channel Roles so a gnut-admin in one channel is not automatically an admin in another channel
- [ ] Dynamically (GHCi) un-/loadable plugins


## Event network

```haskell
data Message = Chat | GroupChat | Headline | Normal
getIM :: Message -> Maybe InstantMessage
```

GroupChat ist auch eine InstantMessage => EventNetwork ueber IMs.

ignored wird vor su gecheckt.

Wenn ein nutzer ignoriert ist dropped Gnut alle weitere behandlung von events von diesem Nutzer.
su wir *nach* dem ignore-check durchgefuert, also kann ein admin eine su-zeile fuer ignorierte Nutzer durchfuehren

su kann nur auf einem level Durchgefuehrt werden - `!su jid su jid2 command` ist ungueltig.



ignore_handler :: Handler Ignore -> Gnut (IO ())
ignore_handler ignore_event_sink = do
    check_if_valid
    check_if_permitted
    ignore_event_sink who
