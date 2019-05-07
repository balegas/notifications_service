-record(subscription, {key, props}).
-record(event, {source, payload, meta}).

-record(mgrState, {sub, con}).
-record(workerState, {sub, con}).

-record(fanoutState, {event_source, endpoints}).