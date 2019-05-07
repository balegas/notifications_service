-record(subscription, {key, props}).
-record(notification, {source, payload, meta}).

-record(mgrState, {sub, con}).
-record(workerState, {sub, con}).
