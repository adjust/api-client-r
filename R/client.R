.ADJUST.HOST <- 'https://api.adjust.com'
.ROOT.PATH <- 'kpis/v1'
.TRACKERS.ROUTE <- 'trackers'
.ACCEPT.HEADER <- 'text/csv'
.AUTHORIZATION.HEADER <- 'Token token=%s'
.LIST.QUERY.PARAMS <- c('kpis', 'countries', 'os_names', 'device_types', 'grouping', 'events', 'tracker_filter')
.VALUE.QUERY.PARAMS <- c('start_date', 'end_date', 'sandbox', 'period')

.AdjustRuntimeEnv <- new.env()

#' Convenience function for initiating a session with a user token and app token, generally required for the start of an
#' Adjust API session. Note that this function gives you the possibility to setup both an app.token and app.tokens. This is
#' useful if you're interested in deliverable KPIs for all of your apps and for cohort or event-based KPIs you're only
#' interested in a particular app. These settings could also be overwritten by the `adjust.cohorts`,
#' `adjust.deliverables`, etc. function arguments.
#' @seealso \code{\link{set.user.token}}, \code{\link{user.token}}, \code{\link{set.app.token}},
#' \code{\link{app.token}}, \code{\link{set.app.tokens}}, \code{\link{app.tokens}}
#' @export
adjust.setup <- function(user.token, app.token=NULL, app.tokens=NULL) {
  set.user.token(user.token)

  if (!is.null(app.token)) { set.app.token(app.token) }
  if (!is.null(app.tokens)) { set.app.tokens(app.tokens) }
}

#' Set an Adjust app.token once using this function and you can issue multiple API requests saving yourself having to
#' pass the token every time.
#' @seealso \code{\link{set.user.token}}, \code{\link{user.token}}, \code{\link{adjust.setup}}, \code{\link{app.token}}
#' @export
set.app.token <- function(app.token) {
  assign('app.token', app.token, envir=.AdjustRuntimeEnv)
}

#' Get the currently set app.token.
#' @seealso \code{\link{set.user.token}}, \code{\link{user.token}}, \code{\link{set.app.token}}, \code{\link{adjust.setup}}
#' @export
app.token <- function() {
  if (!.exists('app.token')) {
    stop('App token needs to be setup first through set.app.token()')
  }

  .get('app.token')
}

#' Set Adjust app.tokens once using this function and you can issue subsequent API requests for multiple apps saving
#' yourself having to pass the tokens every time. @seealso \code{\link{set.user.token}}, \code{\link{user.token}},
#' \code{\link{adjust.setup}}, \code{\link{app.token}}, \code{\link{app.tokens}} @export
set.app.tokens <- function(app.tokens) { assign('app.tokens', app.tokens, envir=.AdjustRuntimeEnv) }

#' Get the currently set app.tokens.
#' @seealso \code{\link{set.user.token}}, \code{\link{user.token}}, \code{\link{set.app.token}},
#' \code{\link{set.app.tokens}}, \code{\link{adjust.setup}}
#' @export
app.tokens <- function() {
  if (!.exists('app.tokens')) {
    stop('The multiple App tokens variable `app.tokens` needs to be setup first through set.app.tokens()')
  }

  .get('app.tokens')
}

#' Set an Adjust user.token, required for authorization.
#' @seealso \code{\link{adjust.setup}}, \code{\link{user.token}}, \code{\link{set.app.token}}, \code{\link{app.token}}
#' @export
set.user.token <- function(user.token) {
  assign('user.token', user.token, envir=.AdjustRuntimeEnv)
}

#' Get the currently set authorization user.token.
#' @seealso \code{\link{set.user.token}}, \code{\link{adjust.setup}}, \code{\link{set.app.token}}, \code{\link{app.token}}
#' @export
user.token <- function() {
  if (!.exists('user.token')) {
    stop('The user.token needs to be setup first using set.user.token("abcdefg").')
  }
  .get('user.token')
}

#' Enable the verbose setting. Doing this will print out additional meta data on the API requests.
#' @seealso \code{\link{adjust.disable.verbose}}
#' @export
adjust.enable.verbose <- function() {
  assign('adjust.verbose', TRUE, envir=.AdjustRuntimeEnv)
}

#' Disable the verbose setting. Doing this will stop printing out additional meta data on the API requests.
#' @seealso \code{\link{adjust.enable.verbose}}
#' @export
adjust.disable.verbose <- function() {
  assign('adjust.verbose', FALSE, envir=.AdjustRuntimeEnv)
}

#' Delivers data for Adjust App KPIs. Refer to the KPI service docs under https://docs.adjust.com/en/kpi-service/
#' together with this help entry. For this function, if an `app.tokens` variable has been setup or is given as an
#' argument to the function, it'll take precedence over any setup or pass of a single `app.token` variable.
#' @param app.token pass it here or set it up once with \code{\link{set.app.token}}
#' @param app.tokens pass it here or set it up once with \code{\link{set.app.tokens}}
#' @param tracker.token If you want data for a specific parent tracker, pass its token.
#' @param start_date YYYY-MM-DD The start date of the selected period.
#' @param end_date YYYY-MM-DD The end date of the selected period.
#' @param kpis A vector of App KPIs. See KPI service docs for more.
#' @param sandbox Boolean request only sandbox data
#' @param countries A vector of ISO 3166 alpha-2 country names.
#' @param os_names A vector of OS names. See KPI service docs for more.
#' @param device_types A vector of supported device types.
#' @param grouping A vector of supported grouping. E.g. \code{c('trackers', 'countries')}. For more on grouping, see the
#' KPI service docs.
#' @export
#' @examples
#' adjust.deliverables() # perhaps the simplest query, it uses the default request parameters on the setup app token.
#' adjust.deliverables(countries=c('us', 'de')) # scope by countries.
adjust.deliverables <- function(app.token=NULL, tracker.token=NULL, ..., app.tokens=NULL) {
  if (is.null(app.token) && is.null(app.tokens) && length(objects(pattern='^app.tokens?$', envir=.AdjustRuntimeEnv)) < 1)
    stop('You need to pass an app.token or app.tokens or use set.app.token() or set.app.tokens() to set them up.')

  if (is.null(app.tokens) && !is.null(app.token))
    return(.api.query(NULL, app.token=app.token, tracker.token=tracker.token, ...))

  if (!is.null(app.tokens) || .exists('app.tokens'))
    return(.api.multiple.apps.query(app.tokens=app.tokens, ...))

  .api.query(NULL, app.token=app.token, tracker.token=tracker.token, ...)
}

#' Delivers data for Adjust Event KPIs. Refer to the KPI service docs under https://docs.adjust.com/en/kpi-service/
#' together with this help entry.
#' @param app.token pass it here or set it up once with \code{\link{set.app.token}}
#' @param tracker.token If you want data for a given parent tracker, pass its token.
#' @param start_date YYYY-MM-DD The start date of the selected period.
#' @param end_date YYYY-MM-DD The end date of the selected period.
#' @param kpis A vector of App KPIs. See KPI service documentation https://docs.adjust.com/en/kpi-service/ for a
#' list of supported App KPIs.
#' @param events A vector of event tokens for Event-specific queries.
#' @param sandbox Boolean request only sandbox data
#' @param countries A vector of ISO 3166 alpha-2 country names.
#' @param os_names A vector of OS names. See KPI service documentation https://docs.adjust.com/en/kpi-service/ for a
#' list of supported OS names.
#' @param device_types A vector of supported device types.
#' @param grouping A vector of supported grouping. E.g. \code{c('trackers', 'countries')}. For more on grouping, see the
#' KPI service docs.
#' @export
#' @examples
#' adjust.events() # perhaps the simplest query, it uses the default request parameters on the setup app token.
#' adjust.events(countries=c('us', 'de')) # scope by countries.
adjust.events <- function(app.token=NULL, tracker.token=NULL, ...) {
  .api.query('events', app.token=app.token, tracker.token=tracker.token, ...)
}

#' Delivers data for Adjust Cohorts. Refer to the KPI service docs under https://docs.adjust.com/en/kpi-service/
#' together with this help entry.
#' @param app.token pass it here or set it up once with \code{\link{set.app.token}}
#' @param tracker.token If you want data for a given parent tracker, pass its token.
#' @param start_date YYYY-MM-DD The start date of the selected period.
#' @param end_date YYYY-MM-DD The end date of the selected period.
#' @param kpis A vector of App KPIs. See KPI service documentation https://docs.adjust.com/en/kpi-service/ for a
#' list of supported App KPIs.
#' @param period The cohort period - one of day/week/month.
#' @param events A vector of event tokens for Event-specific queries.
#' @param sandbox Boolean request only sandbox data
#' @param countries A vector of ISO 3166 alpha-2 country names.
#' @param os_names A vector of OS names. See KPI service documentation https://docs.adjust.com/en/kpi-service/ for a
#' list of supported OS names.
#' @param device_types A vector of supported device types.
#' @param grouping A vector of supported grouping. E.g. \code{c('trackers', 'countries')}. For more on grouping, see the
#' KPI service docs.
#' @export
adjust.cohorts <- function(app.token=NULL, tracker.token=NULL, ...) {
  .api.query('cohorts', app.token=app.token, tracker.token=tracker.token, ...)
}

.api.query <- function (resource, app.token, tracker.token, ...) {
  if (is.null(app.token) && !.exists('app.token')) {
    stop('You need to pass an app.token or set one using set.app.token()')
  }

  if (is.null(app.token)) { app.token <- app.token() }

  .get.request(path=.api.path(app.token, tracker.token=tracker.token, resource=resource),
               query=.query.list(...))
}

.api.multiple.apps.query <- function (app.tokens, ...) {
  if (is.null(app.tokens) && !.exists('app.tokens')) {
    stop('You need to pass app.tokens or set one using set.app.tokens()')
  }

  if (is.null(app.tokens)) { app.tokens <- app.tokens() }

  app.tokens.string <- paste(app.tokens, collapse=',')

  query.list = .query.list(..., app_tokens=app.tokens.string)
  query.list$app_tokens = app.tokens.string

  .get.request(path=.multiple.apps.api.path(), query=query.list)
}

.get.request <- function(...) {
  resp <- GET(.ADJUST.HOST, ..., add_headers(
    'Accept'=.ACCEPT.HEADER,
    'Authorization'=sprintf(.AUTHORIZATION.HEADER, user.token())
  ))

  if (.verbose()) {
    cat(sprintf("Request URL:\n%s\n", URLdecode(resp$url)))
  }

  if (status_code(resp) != 200) {
    stop(content(resp))
  }

  data.table(content(resp, type='text/csv', encoding="UTF-8", col_names=TRUE, col_types=NULL))
}

.api.path <- function(app.token, resource=NULL, tracker.token=NULL) {
  components <- c(.ROOT.PATH, app.token)

  if (! is.null(tracker.token)) { components <- c(components, .TRACKERS.ROUTE, tracker.token) }

  if (! is.null(resource)) { components <- c(components, resource) }

  sprintf('%s.csv', paste(components, collapse='/'))
}

.multiple.apps.api.path <- function() {
  sprintf('%s.csv', .ROOT.PATH)
}

.query.list <- function(...) {
  args <- list(...)
  arg.names <- names(args)

  res <- list()

  for(param in .LIST.QUERY.PARAMS) {
    if (param %in% arg.names) {
      res[param] <- tolower(paste(args[[param]], collapse=','))
    }
  }

  for(param in .VALUE.QUERY.PARAMS) {
    if (param %in% arg.names) {
      res[param] <- tolower(toString(args[[param]]))
    }
  }

  res
}

.verbose <- function() {
  .exists('adjust.verbose') && .get('adjust.verbose') || FALSE
}

.exists <- function(variable) {
  exists(variable, envir=.AdjustRuntimeEnv, inherits=FALSE)
}

.get <- function(variable) {
  get(variable, envir=.AdjustRuntimeEnv, inherits=FALSE)
}
