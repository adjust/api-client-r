library(testthat)

source.path <- file.path('..', 'R')
for (f in list.files(source.path, '[.][rR]')) { source(file.path(source.path, f)) }

test_that('.api.path() composes API paths correctly', {
  expect_that('apps/ABCDEF.csv', is_identical_to(.api.path(app.token='ABCDEF')))
  expect_that('apps/ABCDEF/trackers/12345.csv', is_identical_to(.api.path(app.token='ABCDEF', tracker.token=12345)))
  expect_that('apps/ABCDEF/trackers/12345/cohorts.csv',
    is_identical_to(.api.path(app.token='ABCDEF', tracker.token=12345, resource='cohorts')))
  expect_that('apps/ABCDEF/events.csv', is_identical_to(.api.path(app.token='ABCDEF', resource='events')))
})

test_that('.query.list() returns correct list of query params', {
  expect_that(list(countries='us,de'), is_identical_to(.query.list(countries=c('us', 'de'))))
  expect_that(list(device_types='phone,tablet'), is_identical_to(.query.list(device_types=c('phone', 'tablet'))))
})
