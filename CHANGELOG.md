VERSION 1.0.0

* deprecates the use of single app_token for authentication of requests and uses only the multi-app URLs. Thus this will be a breaking change and a major version bump. All client code will need to use `set.app.tokens()` or pass vector of app tokens on the `adjust.*` accessors.
* it fixes a bug which prevented the use of `adjust.cohorts` for some cases
* Impose checks on the arguments passed on `adjust.*` calls to ensure that eventually passed useless arguments raise an error and don't silently get ignored;
* Introduce `utc_offset` param for Timezones support

