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
