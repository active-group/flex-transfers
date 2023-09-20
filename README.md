# Erlbank - Transfer Service

## Interface Definition

Consumers receive the following information with each transfer event:
```
-record(transfer_event, {
    source :: transfer_service,
    eventId :: non_neg_integer(),
    accountIdSender :: non_neg_integer(),
    accountIdReceiver :: non_neg_integer(),
    amount :: number(),
    timestamp :: erlang:timestamp()}).
```

The service will always send the latest event to the statement-service.

**Event Id**:

Chronologically ordered, counting up from 0.

Receivers can ask for events starting from an eventId, and will receive all following events
``` 
-record(get_transfer_events_since, {
    since :: non_neg_integer(),
    receiver_pid :: pid()
}).
```
