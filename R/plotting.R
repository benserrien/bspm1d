# plotting.R


#' plot_meanerr_1d
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
#' @export
plot_meanerr_1d <- function(data, outcome = NULL,
                            dimension = NULL, grp_factors = NULL,
                            err = "sd", level = .95) {
  stopifnot(
    !is.null(outcome),
    !is.null(dimension)
  )

  # SD-error cloud
  if (err == "sd") {
    data %>%
      summarise(
        .by = all_of(c(dimension, grp_factors)),
        across(all_of(outcome), list(m = mean, sd = sd))
      ) %>%
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
  }

}

