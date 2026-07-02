# bspm1d-summarise.R
# functions to calculate summary statistics
# mean, SD, SE, (1-alpha)*100% CI



# -------------------------------------------------------------------------
# summary statistics ------------------------------------------------------

#' @title bspm1dSummary
#' @description An S4 class bspm1dSummary
#' @exportClass bspm1dSummary
setClass(
  "bspm1dSummary",
  slots = c(
    summary = "data.frame",
    stats   = "character",
    dimname = "character",
    outcome = "character",
    paired  = "logical",
    group   = "character"
  ),
  prototype = list(
    summary = NULL,
    stats   = NA_character_,
    dimname = NA_character_,
    outcome = NA_character_,
    paired  = NA,
    group   = NA_character_
  )
)

#' bspm_summarise
#'
#' @description
#' This function calculates a mean +/- error cloud for the 1-dimensional dataset.
#'
#' @import dplyr
#'
#' @param object An S4 object of class bspm1dData, result from a call to bspm_data().
#' @param err String, specifying the type of error-cloud to calculate (required). Should be either `sd`, `se` or `ci`. In case of `ci`, a level of confidence should be defined.
#' @param level Numeric, level of confidence for the confidence interval (default .95).
#'
#' @return Tibble containing summary statistics of the outcome variable at each point in the 1-dimensional domain (stratified per grouping factor). For an outcome variable called `Y`, the summary statistics are called `Y_m` (mean), `Y_sd` (standard deviation of the data), `Y_se` (standard error of the mean), `Y_cil, Y_ciu` (lower and upper limits of the confidence interval).
#'
#' @export
bspm_summarise <- function(object, err = "sd", level = .95) {
  stopifnot(
    "object should be an S4 object"          = isS4(object),
    "object should be of class 'bspm1dData'" = "bspm1dData" %in% class(object),
    "err should be one of sd/se/ci"          = err %in% c("sd", "se", "ci")
  )
  data    <- object@data
  dimname <- object@dimname
  outcome <- object@outcome
  id      <- object@id
  group   <- object@group
  paired  <- object@paired

  df_summary <- data %>%
    summarise(
      .by = all_of(c(dimname, group)),
      across(all_of(outcome), .bspm_stat_fns(err, level))
    )

  new("bspm1dSummary", summary = df_summary,
      stats = paste0("mean, ", err), paired = paired,
      dimname = dimname, outcome = outcome, group = group)
}


#' .bspm_stat_fns
#'
#' @description
#' Internal helper: returns the named list of summary functions to pass to
#' across(), based on the requested error type.
#'
#' @param err    One of "sd", "se", or "ci".
#' @param level  Confidence level (only used when err = "ci").
#' @return A named list of functions suitable for across(..., list(...)).
#'
.bspm_stat_fns <- function(err, level) {
  se <- function(x) sd(x) / sqrt(length(x))

  # Claude-AI:
  # force() eagerly evaluates its argument, which pins sign and level as
  # concrete values in the closure's own environment rather than leaving
  # them as promises to be resolved later — at which point dplyr's data-masked
  # environment can no longer find them. This is the standard pattern any time
  # you build closures inside functions that will be called by dplyr.
  ci_bound <- function(sign) {
    force(sign)
    force(level)
    function(x) {
      crit_t <- qt(1 - (1 - level) / 2, df = length(x) - 1)
      mean(x) + sign * crit_t * se(x)
    }
  }

  switch(err,
         sd = list(m = mean, sd = sd),
         se = list(m = mean, se = se),
         ci = list(m = mean, cil = ci_bound(-1),
                             ciu = ci_bound(+1))
  )
}



# -------------------------------------------------------------------------
# show/print object to console --------------------------------------------

#' @export
setMethod("show", "bspm1dSummary", function(object) {

    # number of groups (paired?)
  is_grouped <- !is.na(object@group)
  ngroups    <- ifelse(is_grouped,
                       length(unique(object@summary[[object@group]])),
                       1)

  # print to console:
  cat(
    "################## bspm1dSummary object ##################\n",
    "summary statistics: ", object@stats, "\n",
    "groups: K = ", ngroups, " (paired: ", object@paired,")","\n",
    "first 6 rows of tibble with summary statistics: \n",
    sep = ""
  )
  print(head(object@summary, n = 6))
  cat("########################################################")
})

#' @export
print.bspm1dSummary <- function(x, ...) {
  show(x)
  invisible(x)
}
