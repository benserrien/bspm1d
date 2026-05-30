# initiatilization of a bspm1d workflow:
# creation of a data object of class S4 (bspm1dData)

# load("~/Documents/Ben/bspm1d/data/gait_symmetry.RData")
# gait_symmetry_long <- gait_symmetry %>%
#   pivot_longer(c(right_leg, left_leg),
#                values_to = "data", names_to = "leg") %>%
#   unnest(data)

#' bspm1dData
#'
#' @description An S4 class bspm1dData
#'
#' @slot data An object of class data.frame containing the data in long format.
#' @slot dimname Character with the name of the column in `data` specifying the 1-dimensional domain.
#'
#' @exportClass
setClass(
  "bspm1dData",
  slots = c(
    data    = "data.frame",
    dimname = "character",
    outcome = "character",
    id      = "character",
    group   = "character"
  ),
  prototype = list(
    data    = NULL,
    dimname = NA_character_,
    outcome = NA_character_,
    id      = NA_character_,
    group   = NA_character_
  )
)

setValidity(
  "bspm1dData",
  function(object) {

    sum(c(object@dimname, object@outcome, object@id) %in%
          colnames(object@data)) == 3

    if (!(object@dimname %in% colnames(object@data))) {
      "@dimname does not match to any of the columns in @data"
    } else if (!(object@outcome %in% colnames(object@data))) {
      "@outcome does not match to any of the columns in @data"
    } else if (!(object@id %in% colnames(object@data))) {
      "@id does not match to any of the columns in @data"
    } else if (!is.na(object@group) &
               !(object@group %in% colnames(object@data))) {
      "@group does not match to any of the columns in @data"
    } else {
      TRUE
    }
  }
)

bspm1dData <- function(data, dimname, outcome, id, group = NA_character_) {
  new("bspm1dData", data = data,
      dimname = dimname, outcome = outcome, id = id, group = group)
}




# x <- bspm1dData(gait_symmetry_long, "time", "Y", "gait_cycle")
# data(x)
# str(x)



#' check_args
#'
#' @param data description
#'
#' @export
check_args <- function() {
  # arguments: data, group, dimension, outcome, paired, nullInterval, rscale
  stopifnot(
    is.data.frame(data),
    sum(c(group, dimension, outcome) %in% colnames(data)) == 3,
    is.logical(paired),
    length(paired) == 1,
    length(nullInterval) == 2,
    rscale %in% c("medium", "wide", "ultrawide"),
    length(unique(data[[group]])) == 2
  )
  # the data.frame should not contain missing values
  stopifnot(
    sum(is.na(data)) == 0
  )
  # at least 3 observations per group per time point (dimension)
  data_check <- data %>%
    summarise(.by = c(dimension, group), N = n()) %>%
    filter(N < 3)
  if (nrow(data_check) != 0) {
    stop("Some groups have less than 3 observations on at least 1 element of the 1-dimensional domain.")
  }
  # for paired data: same number of observations per group/dimension
  if (paired) {
    data_check_paired <- data %>%
      summarise(.by = c(dimension, group), N = n()) %>%
      summarise(.by = dimension, N2 = n_distinct(N)) %>%
      filter(N2 != 1)
    if (nrow(data_check_paired) != 0) {
      stop("Paired data require the same number of observations at each element of the 1-dimensional domain.")
    }
  }
}
