# Erlbank - Transfer Service

## Interface Definition

Consumers receive the following information with each transfer event:
```
    source :: transfer_service,
    eventId :: integer(),
    accountIdSender :: integer(),
    accountIdReceiver :: integer(),
    timestamp :: erlang:timestamp(),
```

The service will always send the latest event to the statement-service.

**Event Id**:

Chronologically ordered, counting up from 0.

Receivers can ask for events starting from an eventId, and will receive all following events.
