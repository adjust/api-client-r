.ADJUST.HOST <- 'https://api.adjust.com'
.ROOT.PATH <- 'kpis/v1'
.ACCEPT.HEADER <- 'text/csv'
.AUTHORIZATION.HEADER <- 'Token token=%s'
.LIST.QUERY.PARAMS <- c('kpis', 'countries', 'os_names', 'device_types', 'grouping', 'events', 'tracker_filter')
.VALUE.QUERY.PARAMS <- c('start_date', 'end_date', 'sandbox', 'period', 'impression_based', 'reattributed','utc_offset', 'day_def')

.AdjustRuntimeEnv <- new.env()

#' Convenience function for initiating a session with a user token and app tokens, generally required for the start of
#' an Adjust API session. These settings could also be overwritten by the `adjust.cohorts`, `adjust.deliverables`, etc.
#' function arguments.
#' @seealso \code{\link{set.user.token}}, \code{\link{user.token}}, \code{\link{set.app.tokens}}, \code{\link{app.tokens}}
#' @export
adjust.setup <- function(user.token=NULL, app.tokens=NULL) {
  if (!is.null(user.token)) set.user.token(user.token)
  if (!is.null(app.tokens)) set.app.tokens(app.tokens)
}

#' Set Adjust app.tokens once using this function and you can issue subsequent API requests for multiple apps saving
#' yourself having to pass the tokens every time.
#' @seealso \code{\link{set.user.token}}, \code{\link{user.token}}, \code{\link{adjust.setup}}, \code{\link{app.tokens}}
#' @export
set.app.tokens <- function(app.tokens) { .assign('app.tokens', app.tokens) }

#' Get the currently set app.tokens.
#' @seealso \code{\link{set.user.token}}, \code{\link{user.token}},
#' \code{\link{set.app.tokens}}, \code{\link{adjust.setup}}
#' @export
app.tokens <- function() {
  if (! .exists('app.tokens')) {
    if (! is.null(.config()$app_tokens)) {
      set.app.tokens(.config()$app_tokens)
      return(.config()$app_tokens)
    }

    stop('The multiple App tokens variable `app.tokens` needs to be setup first through set.app.tokens()')
  }

  .get('app.tokens')
}

#' Set an Adjust user.token, required for authorization.
#' @seealso \code{\link{adjust.setup}}, \code{\link{user.token}}
#' @export
set.user.token <- function(user.token) {
  .assign('user.token', user.token)
}

#' Get the currently set authorization user.token.
#' @seealso \code{\link{set.user.token}}, \code{\link{adjust.setup}}
#' @export
user.token <- function() {
  if (! .exists('user.token')) {
    if (! is.null(.config()$user_token)) {
      set.user.token(.config()$user_token)
      return(.config()$user_token)
    }

    stop('The user.token needs to be setup first using set.user.token("abcdefg").')
  }

  .get('user.token')
}

#' Enable the verbose setting. Doing this will print out additional meta data on the API requests.
#' @seealso \code{\link{adjust.disable.verbose}}
#' @export
adjust.enable.verbose <- function() {
  .assign('adjust.verbose', TRUE)
}

#' Disable the verbose setting. Doing this will stop printing out additional meta data on the API requests.
#' @seealso \code{\link{adjust.enable.verbose}}
#' @export
adjust.disable.verbose <- function() {
  .assign('adjust.verbose', FALSE)
}

#' Delivers data for Adjust App KPIs. Refer to the KPI service docs under https://docs.adjust.com/en/kpi-service/
#' together with this help entry.
#' @param app.tokens pass it here or set it up once with \code{\link{set.app.tokens}}
#' @param start_date YYYY-MM-DD The start date of the selected period.
#' @param end_date YYYY-MM-DD The end date of the selected period.
#' @param kpis A vector of App KPIs. See KPI service docs for more.
#' @param sandbox Boolean request only sandbox data
#' @param countries A vector of ISO 3166 alpha-2 country names.
#' @param os_names A vector of OS names. See KPI service docs for more.
#' @param device_types A vector of supported device types.
#' @param grouping A vector of supported grouping. E.g. \code{c('countries')}. For more on grouping, see the
#' KPI service docs.
#' @export
#' @examples
#' adjust.deliverables() # perhaps the simplest query, it uses the default request parameters.
#' adjust.deliverables(countries=c('us', 'de')) # scope by countries.
adjust.deliverables <- function(app.tokens=NULL, ...) {
  .api.query(NULL, app.tokens, ...)
}

#' Delivers data for Adjust Event KPIs. Refer to the KPI service docs under https://docs.adjust.com/en/kpi-service/
#' together with this help entry.
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
#' adjust.events() # perhaps the simplest query, it uses the default request parameters.
#' adjust.events(countries=c('us', 'de')) # scope by countries.
adjust.events <- function(app.tokens=NULL, ...) {
  .api.query('events', app.tokens, ...)
}

#' Delivers data for Adjust Cohorts. Refer to the KPI service docs under https://docs.adjust.com/en/kpi-service/
#' together with this help entry.
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
adjust.cohorts <- function(app.tokens=NULL, ...) {
  .api.query('cohorts', app.tokens, ...)
}

.api.query <- function (resource, app.tokens, ...) {
  if (is.null(app.tokens)) { app.tokens <- app.tokens() }

  if (length(app.tokens) < 1) {
    stop('You need to pass app.tokens or use set.app.tokens() to set them up.')
  }

  .get.request(
    path  = .api.path(resource, app.tokens),
    query = .query.list(...)
  )
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
    warning('Unexpected HTTP response code from the KPI service: ', status_code(resp))
    stop(content(resp))
  }

  if (resp$headers$`content-type` != 'text/csv') {
    stop('Unexpected response format was received by the KPI service. Please rerun with `adjust.enable.verbose()` and
         create a GitHub issue with the problem.')
  }

  # We choose to parse the response using data.table::fread instead of readr::read_csv, which is the default in httr.
  res <- data.table::fread(content(resp, as='text', encoding='UTF-8'), integer64='numeric')

  # Perform some parsing of the data.table columns
  for (col in colnames(res)) {
    # We treat NAs as 0 except in non-empty names/tokens
    if (class(res[[col]]) != 'character') {
      val <- res[[col]]
      val[is.na(val)] <- 0
      set(res, j=col, value=val)
    }

    # For consistency all integer class columns are converted to Numeric
    if (class(res[[col]]) == 'integer') {
      set(res, j=col, value=as.numeric(res[[col]]))
    }
  }

  res
}

.api.path <- function(resource=NULL, app.tokens) {
  app.tokens.string <- paste(app.tokens, collapse=',')

  if (is.null(resource))
    sprintf('%s/%s.csv', .ROOT.PATH, app.tokens.string)
  else
    sprintf('%s/%s/%s.csv', .ROOT.PATH, app.tokens.string, resource)
}

.query.list <- function(...) {
  args <- list(...)
  arg.names <- names(args)

  res <- list()

  supported.args <- c(.LIST.QUERY.PARAMS, .VALUE.QUERY.PARAMS)
  if (length(setdiff(arg.names, supported.args)) > 0) {
    stop('Unsupported query parameters passed: ', paste(setdiff(arg.names, supported.args), collapse=','))
  }

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

.assign <- function(variable, value) {
  assign(variable, value, envir=.AdjustRuntimeEnv)
}

.config <- function(file.name=NULL) {
  if (.exists('adjust.config')) return(.get('adjust.config'))

  if (!is.null(file.name) && !file.exists(file.name)) stop('File `', file.name, '` not found')

  if (is.null(file.name)) file.name <- file.path(Sys.getenv('HOME'), '.adjustrc')

  if (.no.config.support() || !file.exists(file.name)) {
    .assign('adjust.config', list())
    return(.get('adjust.config'))
  }

  config <- data.table(read.table(file.name, sep=":", col.names=c('setting', 'value'), stringsAsFactors=FALSE))

  .parse.config(config)
}

.parse.config <- function(config) {
  .value <- function(key) {
    if (nrow(config[setting==key]) == 0) return(NULL)

    val <- config[setting==key, gsub('(^[[:space:]]*|[[:space:]]*$)', '', value)]
    if (length(val) > 1) stop('The `', key, ':` entry in the .adjustrc file must only appear once.')
    val
  }

  result <- list()

  result$user_token <- .value('user_token')
  app.tokens <- .value('app_tokens')
  if (! is.null(app.tokens)) result$app_tokens <- strsplit(app.tokens, ',')[[1]]

  return(result)
}

.no.config.support <- function() {
  .Platform$OS.type != 'unix'
}
