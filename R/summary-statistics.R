# summary-statistics.R
# functions to calculate summary statistics (mean, SD, (1-alpha)*100% CI)



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
    err %in% c("sd","se","ci")
  )

  # mean & SD
  if (err == "sd") {
    summary_stat <- data %>%
      summarise(
        .by = all_of(c(dimension, grp_factors)),
        across(all_of(outcome), list(m = mean, sd = sd))
      )
  } else if (err == "se") {
    se <- function(x) sd(x)/sqrt(length(x))
    summary_stat <- data %>%
      summarise(
        .by = all_of(c(dimension, grp_factors)),
        across(all_of(outcome), list(m = mean, se = se))
      )
  } else if (err == "ci") {
    # confidence interval calculation using t-distribution
    ci <- function(x, sign) {
      stopifnot(sign %in% c(-1, 1))
      dof <- length(x) - 1
      alpha <- 1 - level
      mean(x) + sign * qt(1 - alpha/2, dof) * sd(x)/sqrt(length(x))
    }
    summary_stat <- data %>%
      summarise(
        .by = all_of(c(dimension, grp_factors)),
        across(all_of(outcome), list(m = mean,
                                     cil = ~ ci(.x, sign = -1),
                                     ciu = ~ ci(.x, sign = +1)))
      )
  }
  return(summary_stat)
}
