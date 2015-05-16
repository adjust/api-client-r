.ADJUST.HOST <- 'https://api.adjust.com'
.ADJUST.HOST <- 'http://localhost:3035'
.ROOT.PATH <- 'apps'
.TRACKERS.ROUTE <- 'trackers'
.ACCEPT.HEADER <- 'application/json'
.AUTHORIZATION.HEADER <- 'Token token=%s'

.AdjustRuntimeEnv <- new.env()

#' @export
adjust.setup <- function(user.token, app.token=NULL) {
  set.user.token(user.token)

  if (!is.null(app.token)) { set.app.token(app.token) }
}

#' @export
set.app.token <- function(app.token) {
  assign('app.token', app.token, envir=.AdjustRuntimeEnv)
}

#' @export
app.token <- function() {
  if (!exists('app.token', envir=.AdjustRuntimeEnv)) {
    stop('App token needs to be setup first through set.app.token()')
  }

  get('app.token', envir=.AdjustRuntimeEnv)
}

#' @export
set.user.token <- function(user.token) {
  assign('user.token', user.token, envir=.AdjustRuntimeEnv)
}

#' @export
user.token <- function() {
  get('user.token', envir=.AdjustRuntimeEnv)
}

#' @export
adjust.deliverables <- function(app.token=NULL, ...) {
  if (is.null(app.token) && !exists('app.token', envir=.AdjustRuntimeEnv)) {
    stop('You need to pass an app.token or set one using set.app.token()')
  }

  if (is.null(app.token)) { app.token <- app.token() }

  .get.request(path=.api.path(app.token), query=.query.list(...))
}

#' @export
adjust.events <- function(app.token=NULL, ...) {
  if (is.null(app.token) && !exists('app.token', envir=.AdjustRuntimeEnv)) {
    stop('You need to pass an app.token or set one using set.app.token()')
  }

  if (is.null(app.token)) { app.token <- app.token() }

  .get.request(path=.api.path(app.token, resource='events'), query=.query.list(...))
}

#' @export
adjust.cohorts <- function(app.token=NULL, ...) {
  if (is.null(app.token) && !exists('app.token', envir=.AdjustRuntimeEnv)) {
    stop('You need to pass an app.token or set one using set.app.token()')
  }

  if (is.null(app.token)) { app.token <- app.token() }

  .get.request(path=.api.path(app.token, resource='cohorts'), query=.query.list(...))
}

.get.request <- function(...) {
  resp <- GET(.ADJUST.HOST, ..., add_headers(
    'Accept'=.ACCEPT.HEADER,
    'Authorization'=sprintf(.AUTHORIZATION.HEADER, user.token()))
  )

  if (status_code(resp) != 200) {
    stop(content(resp))
  }

  data.frame(content(resp))
}

.api.path <- function(app.token, resource=NULL, tracker.token=NULL) {
  components <- c(.ROOT.PATH, app.token)

  if (! is.null(tracker.token)) { components <- c(components, .TRACKERS.ROUTE, tracker.token) }

  if (! is.null(resource)) { components <- c(components, resource) }

  sprintf('%s.csv', paste(components, collapse='/'))
}

.query.list <- function(...) {
  args <- list(...)

  query <- list()

  if (length(args$countries) > 0) { query$countries <- paste(args$countries, collapse=',') }

  query
}
