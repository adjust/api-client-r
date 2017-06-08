library(testthat)
library(adjust)

source.path <- file.path('..', 'R')
for (f in list.files(source.path, '[.][rR]')) { source(file.path(source.path, f)) }

test_that('.api.path() composes API paths correctly', {
  expect_that('kpis/v1/ABCDEF.csv', is_identical_to(.api.path(app.token='ABCDEF')))
  expect_that('kpis/v1/ABCDEF/events.csv', is_identical_to(.api.path(app.token='ABCDEF', resource='events')))
  expect_that('kpis/v1/ABCDEF/cohorts.csv', is_identical_to(.api.path(app.token='ABCDEF', resource='cohorts')))
})

test_that('.query.list() returns correct list of query params', {
  expect_that(list(countries='us,de'), is_identical_to(.query.list(countries=c('US', 'de'))))
  expect_that(list(device_types='phone,tablet'), is_identical_to(.query.list(device_types=c('Phone', 'Tablet'))))
  expect_that(list(countries='us,de', device_types='phone,tablet', sandbox='true'),
    is_identical_to(.query.list(countries=c('us', 'de'), device_types=c('phone', 'tablet'), sandbox=TRUE)))
})

test_that('.config() loads the right settings', {
  expect_that(.config('adjustrc.test')$user_token, is_identical_to('my-token'))
  expect_that(.config('adjustrc.test')$app_tokens, is_identical_to(c('app1', 'app2')))
  expect_that(.config('adjustrc.test')$app_token, is_identical_to('app3'))
})

test_that('.config() loads the right settings', {
  expect_that(.config('adjustrc.test')$user_token, is_identical_to('my-token'))
  expect_that(.config('adjustrc.test')$app_tokens, is_identical_to(c('app1', 'app2')))
  expect_that(.config('adjustrc.test')$app_token, is_identical_to('app3'))
})

test_that('.config() runs for a default config file', {
  expect_that(.config(), not(throws_error()))
})

test_that('.config() raises an error if no file found', {
  expect_error(.config('no-file'))
})

test_that('.parse.config() raises an error if incorrect config data given', {
  config <- data.table(setting=rep('user_token', 2), value=1:2)
  expect_that(.parse.config(config), throws_error('must only appear once'))

  config <- data.table(setting=rep('app_tokens', 2), value=1:2)
  expect_that(.parse.config(config), throws_error('must only appear once'))

  config <- data.table(setting=rep('app_token', 2), value=1:2)
  expect_that(.parse.config(config), throws_error('must only appear once'))
})

test_that('.parse.config() treats incomplete config data correctly', {
  config <- data.table(setting=c('app_token'), value=c('my-app'))
  expect_that(.parse.config(config), is_identical_to(list(app_token='my-app')))

  config <- data.table(setting=c('app_tokens', 'user_token'), value=c('my-app1 my-app2', 'my-user'))
  expect_that(.parse.config(config), is_identical_to(list(user_token='my-user', app_tokens=c('my-app1', 'my-app2'))))
})

test_that('adjust.deliverables() throws an informative error for wrong app.token', {
  expect_that(adjust.deliverables(c('abc', 'cba')), throws_error('`app.token` cannot be a vector'))
})
