# R Client for the Adjust Statistics API

This is an R client for the [Adjust KPI service](https://docs.adjust.com/en/kpi-service/). It supports all API
functionalities through well documented functions.

## Installation

The client is currently best installed installed via GitHub (using the `devtools` package):

    > library(devtools)
    > devtools::install_github('adjust/api-client-r');

## Usage

Let's have a walk through the functionalities through a couple of examples. Start by loading the library.

    > library(adjust)

### Setup

You're requered to enter your Adjust `user_token` for authentication that will be used over the entire R session. Here
you could optionally give an `app_token` too, in case you plan to focus on one app. Alternatively, an `app_token` could
be given with every request.

    > adjust.setup(user.token='aYSsuEVhAMDQDyZ8kj2K', app.token='abcdefg')

You can also set multiple apps up.

    > adjust.setup(user.token='aYSsuEVhAMDQDyZ8kj2K', app.tokens=c('abcdefg', 'gfedcba'))

Note that if `app.tokens` variable has been set, it'll take precedence over `app.token` for API calls that support
multiple apps, such as deliverables.

### Statistics API calls

Now you're fully setup to start requesting some data from the Adjust API. You can start with the simplest query:

    > adjust.deliverables()

That will return the default API response for the setup app token. You can, more interestingly, also specify parameters:

    > adjust.deliverables(start_date='2015-01-01', end_date='2015-01-10',
        countries=c('us', 'de'), kpis=c('clicks', 'sessions', 'installs'))

      tracker_token   tracker_name   clicks sessions installs
    1        2yakhy   Twitter        19     369      0
    2        26kvyi   Facebook       857    26251    311
    3        3d7ly6   Adwords        1      0        0
    ...

A more complete example:

    > adjust.deliverables(
        countries=c('us', 'de'),
        kpis=c('sessions', 'installs'),
        start_date='2015-05-01',
        end_date='2015-05-10',
        sandbox=TRUE,
        grouping=c('trackers', 'countries'),
        tracker.token='26kvyi')
      )

For a full list of options and supported KPIs, see `?adjust.deliverables` from your R session.

Similarly you have `adjust.events` for your custom Adjust-Events queries and `adjust.cohorts` for Cohorts. The R help
for these functions has more usage details.

---

For facilitated working with user tokens and app tokens, you can use the `set.user.token`, `set.app.token` and
`set.app.tokens` functions. Note again that if `app.tokens` has been set, it'll take precedence over
`app.token` for the API calls that support multiple apps.

For full details, see the R manuals.

### Debugging

The functions `adjust.enable.verbose` and `adjust.disable.verbose` will trigger verbose mode, which will output details
on each API call, such as exact API URL used.

## Contributions and bug reports.

Running the tests for development relies on the `testthat` package:

    > library(testthat);
    > test_file('tests/adjust.R');

Contributions and bug reports are only acceptable as GitHub Pull Requests and issues. Thanks!
