# bspm1d-summarise.R
# functions to calculate summary statistics
# mean, SD, SE, (1-alpha)*100% CI



# -------------------------------------------------------------------------
# summary statistics ------------------------------------------------------

#' @title bspm1dSummary
#'
#' @description An S4 class bspm1dSummary
#'
#' @exportClass bspm1dSummary
setClass(
  "bspm1dSummary",
  slots = c(
    summary = "data.frame"
  ),
  prototype = list(
    summary = NULL
  )
)


#' bspm_summarise1d
#'
#' @description
#' This function calculates a mean +/- error cloud for the 1-dimensional dataset. The error cloud can be either based on the SD or a confidence interval. Grouping factors can be specified to calculate group-specific summaries.
#'
#' @import dplyr
#'
#' @param data R data.frame containing the individual-level 1-dimensional data in long format.
#' @param outcome String, specifying the name of the outcome variable (required).
#' @param dimension String, specifyng the name of the 1-dimensional domain (required).
#' @param grp_factors String, specifying the name of grouping factors in the data (optional).
#' @param err String, specifying the type of error-cloud to calculate (required). Should be either `sd`, `se` or `ci`. In case of `ci`, a level of confidence should be defined (default .95).
#' @param level Numeric, level of confidence for the confidence interval (default .95).
#'
#' @return Tibble containing summary statistics of the outcome variable at each point in the 1-dimensional domain (stratified per grouping factor). For an outcome variable called `Y`, the summary statistics are called `Y_m` (mean), `Y_sd` (standard deviation of the data), `Y_se` (standard error of the mean), `Y_cil, Y_ciu` (lower and upper limits of the confidence interval).
#'
#' @export
bspm_summarise1d <- function(data, outcome = NULL,
                             dimension = NULL, grp_factors = NULL,
                             err = "sd", level = .95) {
  # checks on the arguments
  stopifnot(
    !is.null(outcome),
    !is.null(dimension),
    err %in% c("sd", "se", "ci")
  )
  data %>%
    summarise(
      .by = all_of(c(dimension, grp_factors)),
      across(all_of(outcome), .bspm_stat_fns(err, level))
    )
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

  # force() eagerly evaluates its argument, which pins sign and level as
  # concrete values in the closure's own environment rather than leaving
  # them as promises to be resolved later — at which point dplyr's data-masked
  # environment can no longer find them. This is the standard pattern any time
  # you build closures inside functions that will be called by dplyr.
  ci_bound <- function(sign) {
    force(sign)
    force(level)
    function(x) {
      mean(x) + sign * qt(1 - (1 - level) / 2, df = length(x) - 1) * se(x)
    }
  }

  switch(err,
         sd = list(m = mean, sd = sd),
         se = list(m = mean, se = se),
         ci = list(m = mean, cil = ci_bound(-1),
                             ciu = ci_bound(+1))
  )
}
