# initiatilization of a bspm1d workflow:
# creation of a data object of class S4 (bspm1dData)



# -------------------------------------------------------------------------
# object initialization ---------------------------------------------------

#' @title bspm1dData
#'
#' @description An S4 class bspm1dData
#'
#' @slot data An object of class data.frame containing the data in long format.
#' @slot dimname Character with the name of the column in `data` specifying the 1-dimensional domain.
#'
#' @exportClass bspm1dData
setClass(
  "bspm1dData",
  slots = c(
    data    = "data.frame",
    dimname = "character",
    outcome = "character",
    id      = "character",
    paired  = "logical",
    group   = "character"
  ),
  prototype = list(
    data    = NULL,
    dimname = NA_character_,
    outcome = NA_character_,
    id      = NA_character_,
    paired  = NA,
    group   = NA_character_
  )
)

#' @title bspm1dData
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param data data.frame that contains the 1-dimensional data; the outcome vector mist be a list-column where each row contains a 1xD row vector (D elements in the 1-dimensional domain)
#' @param dimension 1-dimensional domain in which the outcome is measured, can be a simple vector like 0:100 or can contain units like units::set_units(0:100, "%")
#' @param outcome character with name of the outcome variable
#' @param id ID-variable, must be able to identify paired data
#' @param paired logical, must be TRUE when 2 paired observations per `id` are present (1 in each level of `group`)
#' @param group optional vector that defines a grouping factor (2-levels)
#'
#' @export
bspm1dData <- function(data, dimension, outcome,
                       id, paired = FALSE, group = NA_character_) {
  # check input data
  dimname <- deparse(substitute(dimension))
  dimcheck <- sapply(data[[outcome]], dim)
  stopifnot(
    sum(dimcheck[1,] == 1) == nrow(data),
    sum(dimcheck[2,] == length(dimension)) == nrow(data)
  )

  # transform to long format (add dimension variable)
  myt <- function(x) {
    x %>%
      pivot_longer(everything(),
                   names_to = NULL,
                   values_to = outcome) %>%
      mutate(!!dimname := dimension, .before = outcome)
  }
  data_long <- data %>%
    mutate(!!outcome := map(.data[[outcome]], myt)) %>%
    unnest(.data[[outcome]])

  new("bspm1dData", data = data_long,
      dimname = dimname, outcome = outcome,
      id = id, paired = paired, group = group)
}


# -------------------------------------------------------------------------
# checking object validity ------------------------------------------------

setValidity(
  "bspm1dData",
  function(object) {

    if (!(object@dimname %in% colnames(object@data))) {
      "@dimname does not match to any of the columns in @data"
    } else if (!(object@outcome %in% colnames(object@data))) {
      "@outcome does not match to any of the columns in @data"
    } else if (!is.na(object@id) &
               !(object@id %in% colnames(object@data))) {
      "@id does not match to any of the columns in @data"
    } else if (!is.na(object@group) &
               !(object@group %in% colnames(object@data))) {
      "@group does not match to any of the columns in @data"
    } else if (sum(is.na(object@data)) > 0) {
      "the data should not contain missing values"
    } else {
      TRUE
    }
    # to do:
    # at least 3 observations per group per time point (dimension)
    # for paired data: same number of observations per group/dimension
  }
)


# -------------------------------------------------------------------------
# show/print object to console --------------------------------------------


#' @export
setMethod("show", "bspm1dData", function(object) {
  # print 1-dimensional domain
  # time [units] = size, head(time)
  dimname   <- object@dimname
  has_units <- "units" %in% attributes(object@data[[dimname]])
  units <- ifelse(has_units,
                  units(object@data[[dimname]])$numerator,
                  NA_character_)
  dimsize <- length(unique(object@data[[dimname]]))
  dimprint <- object@data %>%
    select(all_of(dimname)) %>%
    slice_head(n = 6) %>%
    pull() %>%
    paste(collapse = ", ") %>%
    paste0(", .... [length: ", dimsize,", units: ",units,"]")

  # number of groups (paired?)
  is_grouped <- !is.na(object@group)
  ngroups    <- ifelse(is_grouped,
                       length(unique(object@data[[object@group]])),
                       1)

  # number of observations per group
  if (is_grouped) {
    nobs <- object@data %>%
      summarise(.by = object@group, N = n_distinct(.data[[object@id]]))
  } else {
    nobs <- object@data %>%
      summarise(N = n_distinct(.data[[object@id]]))
  }

  # print to console:
  cat(
    "################## bspm1dData object ##################\n",
    dimname, ": ", dimprint, "\n",
    "groups: K = ", ngroups, " (paired: ", object@paired,")", "\n",
    "observations: \n",
    sep = ""
  )
  print(nobs)
  cat("#######################################################")
})

#' @export
print.bspm1dData <- function(x, ...) {
  show(x)
  invisible(x)
}
