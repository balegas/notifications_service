-record(subscription, {key, props}).
-record(updateEvent, {source, payload, meta}).

-record(mgrState, {sub, con, router}).
-record(workerState, {sub, con}).

-record(fanoutState, {event_source, endpoints}).