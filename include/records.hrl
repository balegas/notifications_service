-record(subscription, {key, props}).
-record(notification, {source, destination, payload, meta}).

-record(mgrState, {sub, con}).
-record(workerState, {sub, con}).
