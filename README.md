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

### Setup and Authentication

In order for you to access the KPI Service, you need to authenticate with `user_token`. Your `user_token`, once you supply it, will be used throughout the entire R session. Furthermore, you need to provide `app_tokens` for the requests.

You have three ways to authenticate and provide these settings.

#### Config File for UNIX Users

Since version 0.0.5, UNIX users can save their configurations in a file and the `adjust` package will automatically use
these settings without having to explicitly authenticate in every R session. Using this feature will save you a lot of
function calls that would otherwise have to be explicitly issued, as you see below.

The config file should be placed in your `HOME` (`~/`) directory and be named `.adjustrc`. The expected format is:

```
user_token: aYSsuEVhAMDQDyZ8kj2K
app_tokens: abcdefg,gfedcba
```

You can skip any of these settings and still use the rest. For example, you can only keep your `user_token` there for
authentication, and provide `app_tokens` in a different way.

#### Using `adjust.setup()` Function

Even if you use the config file, you could still overwrite all settings there in an R session. To do this, you can use
the `adjust.setup()` function or the `set.user.token()`, `set.app.tokens()` functions. See the help pages for each of
those for more details and here is the example:

    > adjust.setup(user.token='aYSsuEVhAMDQDyZ8kj2K', app.tokens=c('abcdefg', 'gfedcba'))

#### Provide `app.tokens` as Function Arguments.

Even if you use the config file, call `adjust.setup()` in your script, you can still pass `app.tokens`
arguments to individual API calls. See the function descriptions for more details.

### Statistics API calls

There are three functions that return KPI data from the API:

    > adjust.deliverables()
    > adjust.events()
    > adjust.cohorts()

Once you've setup your user token and app tokens, then each of these calls will produce data for you straight away.

For details on any of those, you should refer to the help pages for example by `?adjust.deliverables` as well as the KPI
service documentation, cited above.

You can, more interestingly, also specify parameters:

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
        tracker_filter=c('26kvyi'))
      )

For this function, you can issue calls for multiple app tokens at the same time and group for example by `grouping=c('app', 'network')`.

Note that this function supports multiple `app_tokens`. This means that you can make calls like so:

    > adjust.deliverables(app.tokens=c('abcd', 'efgh'), grouping=('app', 'networks'))

This will return KPI data for the two apps, grouped by the app and all its network trackers.

### Custom API instances

For most users this functionality wouldn't be necessary, but the adjust R client
allows for custom API hosts to be used. Check the function `adjust.set.host()`
for more details.

### Debugging

The functions `adjust.enable.verbose()` and `adjust.disable.verbose()` will trigger verbose mode, which will output
details on each API call, such as exact API URL used.

## Contributions and bug reports.

Running the tests for development relies on the `testthat` package:

    > library(testthat);
    > test_file('tests/adjust.R');

Contributions and bug reports are only acceptable as GitHub Pull Requests and issues. Thanks!
