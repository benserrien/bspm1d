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

  bspm_summarise1d(data, outcome, dimension, grp_factors, err = err, level) %>%
    .bspm_add_bounds(outcome, err) %>%
    ggplot(.bspm_build_aes(outcome, dimension, grp_factors)) +
    geom_line() +
    geom_ribbon(alpha = .2)
}


#' .bspm_add_bounds
#'
#' @description
#' Internal helper: appends ymin/ymax columns to a summarised data frame
#' based on the error type requested.
#'
#' @param summary_stat  Data frame returned by bspm_summarise1d().
#' @param outcome       String, name of the outcome variable.
#' @param err           One of "sd", "se", or "ci".
#' @return The input data frame with ymin and ymax columns added.
#'
.bspm_add_bounds <- function(summary_stat, outcome, err) {
  m_col  <- paste0(outcome, "_m")
  switch(err,
         sd = mutate(summary_stat,
                     ymin = .data[[m_col]] - .data[[paste0(outcome, "_sd")]],
                     ymax = .data[[m_col]] + .data[[paste0(outcome, "_sd")]]
         ),
         se = mutate(summary_stat,
                     ymin = .data[[m_col]] - .data[[paste0(outcome, "_se")]],
                     ymax = .data[[m_col]] + .data[[paste0(outcome, "_se")]]
         ),
         ci = mutate(summary_stat,
                     ymin = .data[[paste0(outcome, "_cil")]],
                     ymax = .data[[paste0(outcome, "_ciu")]]
         ),
         stop("err should be one of 'sd', 'se' or 'ci'")
  )
}


#' .bspm_build_aes
#'
#' @description
#' Internal helper: builds the ggplot aes(), optionally adding color/fill
#' when a grouping factor is supplied.
#'
#' @param outcome      String, name of the outcome variable.
#' @param dimension    String, name of the x-axis variable.
#' @param grp_factors  String or NULL, name of the grouping variable.
#' @return An aes() object.
#'
.bspm_build_aes <- function(outcome, dimension, grp_factors) {
  base_aes <- aes(
    x    = .data[[dimension]],
    y    = .data[[paste0(outcome, "_m")]],
    ymin = ymin,
    ymax = ymax
  )
  if (!is.null(grp_factors)) {
    base_aes <- modifyList(base_aes, aes(
      color = factor(.data[[grp_factors]]),
      fill  = factor(.data[[grp_factors]])
    ))
  }
  base_aes
}
