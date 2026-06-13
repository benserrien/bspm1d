# initiatilization of a bspm1d workflow:
# creation of a data object of class S4 (bspm1dData)



# -------------------------------------------------------------------------
# object initialization ---------------------------------------------------

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

bspm1dData <- function(data, dimname, outcome, id, group = NA_character_) {
  new("bspm1dData", data = data,
      dimname = dimname, outcome = outcome, id = id, group = group)
}


# -------------------------------------------------------------------------
# checking object validity ------------------------------------------------

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
    # at least 3 observations per group per time point (dimension)
    # the data.frame should not contain missing values
    # for paired data: same number of observations per group/dimension
  }
)


# -------------------------------------------------------------------------
# show/print object to console --------------------------------------------

setGeneric("show", function(object) {
  standardGeneric("show")
})
setMethod("show", "bspm1dData", function(object) {
  object@data
})
setGeneric("print", function(object) {
  standardGeneric("print")
})
setMethod("print", "bspm1dData", function(object) {
  object@data
})

