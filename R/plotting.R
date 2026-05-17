# plotting.R


#' bspm_plot1d
#'
#' @description
#' This function calculates a mean +/- error cloud for the 1-dimensional dataset. The error cloud can be either based on the SD or a confidence interval. Grouping factors can be specified.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @param data R data.frame containing the individual-level 1-dimensional data in long format.
#' @param outcome String, specifying the name of the outcome variable (required).
#' @param dimension String, specifyng the name of the 1-dimensional domain (required).
#' @param grp_factors String, specifying the name of grouping factors in the data (optional).
#' @param err String, specifying the type of error-cloud to calculate (required). Should be either `sd` or `ci`. In case of `ci`, a level of confidence should be defined (default .95).
#' @param level Numeric, level of confidence for the confidence interval (default .95).
#'
#' @return A ggplot figure is returned.
#'
#' @export
bspm_plot1d <- function(data, outcome = NULL,
                            dimension = NULL, grp_factors = NULL,
                            err = "sd", level = .95) {
  stopifnot(
    !is.null(outcome),
    !is.null(dimension)
  )

  # SD-error cloud
  if (err == "sd") {
    summary_stat <- bspm_summarise1d(data, outcome, dimension,
                                     grp_factors, err = "sd")
    summary_stat %>%
      mutate(
        ymin = .data[[paste0(outcome,"_m")]] - .data[[paste0(outcome,"_sd")]],
        ymax = .data[[paste0(outcome,"_m")]] + .data[[paste0(outcome,"_sd")]]
      ) %>%
      ggplot(aes(x = .data[[dimension]],
                 y = .data[[paste0(outcome,"_m")]],
                 color = factor(.data[[grp_factors]]),
                 fill = factor(.data[[grp_factors]]),
                 ymin = ymin, ymax = ymax)) +
      geom_line() +
      geom_ribbon(alpha = .2)
  } else if (err == "se") {
    summary_stat <- bspm_summarise1d(data, outcome, dimension,
                                     grp_factors, err = "se")
    summary_stat %>%
      mutate(
        ymin = .data[[paste0(outcome,"_m")]] - .data[[paste0(outcome,"_se")]],
        ymax = .data[[paste0(outcome,"_m")]] + .data[[paste0(outcome,"_se")]]
      ) %>%
      ggplot(aes(x = .data[[dimension]],
                 y = .data[[paste0(outcome,"_m")]],
                 color = factor(.data[[grp_factors]]),
                 fill = factor(.data[[grp_factors]]),
                 ymin = ymin, ymax = ymax)) +
      geom_line() +
      geom_ribbon(alpha = .2)
  } else if (err == "ci") {
    summary_stat <- bspm_summarise1d(data, outcome, dimension,
                                     grp_factors, err = "ci", level)
    summary_stat %>%
      mutate(
        ymin = .data[[paste0(outcome,"_cil")]],
        ymax = .data[[paste0(outcome,"_ciu")]]
      ) %>%
      ggplot(aes(x = .data[[dimension]],
                 y = .data[[paste0(outcome,"_m")]],
                 color = factor(.data[[grp_factors]]),
                 fill = factor(.data[[grp_factors]]),
                 ymin = ymin, ymax = ymax)) +
      geom_line() +
      geom_ribbon(alpha = .2)
  }
}

