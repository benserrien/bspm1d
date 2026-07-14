# bspm1d-plot.R
# several 1-dimensional plots

#' @include bspm1d-summarise.R
#' @include bspm1d-bfttest.R
#' @export
setGeneric("bspm_plot", function(object) {
  standardGeneric("bspm_plot")
})


# -------------------------------------------------------------------------
# plot individual data ----------------------------------------------------

setMethod("bspm_plot", "bspm1dData", function(object) {
  plot_aes <- .bspm_build_aes1(object)
  object@data %>%
    ggplot(plot_aes) +
    geom_line()
})

#' .bspm_build_aes1
#'
#' @description
#' Internal helper: builds the ggplot aes(), optionally adding color/fill
#' when a grouping factor is supplied.
#' @return An aes() object.
.bspm_build_aes1 <- function(object) {
  base_aes <- aes(
    x     = .data[[object@dimname]],
    y     = .data[[object@outcome]],
    group = .data[[object@id]]
  )
  if (!is.na(object@group)) {
    base_aes <- modifyList(base_aes, aes(
      group = interaction(.data[[object@group]], .data[[object@id]]),
      color = .data[[object@group]]
    ))
  }
  base_aes
}



# -------------------------------------------------------------------------
# plot summary data -------------------------------------------------------

setMethod("bspm_plot", "bspm1dSummary", function(object) {
  plot_aes <- .bspm_build_aes2(object)
  ylabel <- paste0(object@outcome,": ", object@stats)
  object@summary %>%
    ggplot(plot_aes) +
    geom_line() +
    geom_ribbon(alpha = .2) +
    labs(y = ylabel)
})

#' .bspm_build_aes2
#'
#' @description
#' Internal helper: builds the ggplot aes(), optionally adding color/fill
#' when a grouping factor is supplied.
#' @return An aes() object.
.bspm_build_aes2 <- function(object) {
  base_aes <- aes(
    x     = .data[[object@dimname]],
    y     = .data[[paste0(object@outcome,"_m")]],
    ymin  = switch(object@stats,
                   "mean, sd" = .data[[paste0(object@outcome,"_m")]] -
                     .data[[paste0(object@outcome,"_sd")]],
                   "mean, se" = .data[[paste0(object@outcome,"_m")]] -
                     .data[[paste0(object@outcome,"_se")]],
                   "mean, ci" = .data[[paste0(object@outcome,"_cil")]]),
    ymax  = switch(object@stats,
                   "mean, sd" = .data[[paste0(object@outcome,"_m")]] +
                     .data[[paste0(object@outcome,"_sd")]],
                   "mean, se" = .data[[paste0(object@outcome,"_m")]] +
                     .data[[paste0(object@outcome,"_se")]],
                   "mean, ci" = .data[[paste0(object@outcome,"_cil")]])
  )
  if (!is.na(object@group)) {
    base_aes <- modifyList(base_aes, aes(
      group = .data[[object@group]],
      color = .data[[object@group]],
      fill  = .data[[object@group]]
    ))
  }
  base_aes
}



# -------------------------------------------------------------------------
# plot hypothesis object --------------------------------------------------

setMethod("bspm_plot", "bspm1dHypothesis", function(object) {
  if (length(object@nullInterval) == 1) {
    plotH0 <- function() geom_vline(xintercept = 0, lty = 1)
  } else {
    plotH0 <- function()
      annotate("rect",
               xmin = object@nullInterval[1],
               xmax = object@nullInterval[2],
               ymin = 0, ymax = Inf, alpha = 0.2, fill = "grey50")
  }
  ggplot() +
    geom_function(fun = dcauchy,
                  args = list(location = 0, scale = object@prior_rscale)) +
    plotH0() +
    xlim(-3, 3) +
    ylim(c(0, NA)) +
    theme_minimal() +
    labs(x = expression(delta), y = "prior PDF",
         title = "prior & null hypothesis")
})
