# bspm1d-bfttest.R
# set of R functions to perform a Bayesian SPM{t}-test for 1-dimensional data



# -------------------------------------------------------------------------
# S4 class for bspm1dHypothesis -------------------------------------------

#' @title bspm1dHypothesis
#' @description An S4 class bspm1dHypothesis
#' @exportClass bspm1dHypothesis
setClass(
  "bspm1dHypothesis",
  slots = c(
    prior_rscale = "numeric",
    nullInterval = "vector"
  ),
  prototype = list(
    prior_rscale = 1,
    nullInterval = 0
  )
)

#' @title bspm_hyp
#' @description
#' Function to define null and alternative hypotheses to be tested
#' @param rscale see `?BayesFactor::ttestBF`, defaults to 1 if not specified (wide default setting in BayesFactor package)
#' @param nullIntervale see `?BayesFactor::ttestBF`, can be either empty (defaults to testing the point-null hypothesis), a single value which will be interpreted as testing a symmetric nullInterval (e.g. 0.2) or a vector of size two optionally asymmetric (eg. c(-0.1, 0.2))
#' @importFrom assertthat assert_that
#' @export
bspm_hyp <- function(prior_rscale, nullInterval = NULL) {
  assert_that(is.numeric(prior_rscale),
              msg = "argument prior_rscale should be numeric" )
  assert_that(length(prior_rscale) == 1,
              msg = "argument prior_rscale should be a single number")
  if (!is.null(nullInterval)) {
    assert_that(is.numeric(nullInterval),
                msg = "argument nullInterval should be numeric")
    assert_that(length(nullInterval) %in% c(1, 2),
                msg = "argument nullInterval should be of length 1 or 2 (or left unspecified)")
  }

  H0 <- switch(
    as.character(length(nullInterval)),
    "0" = 0, # point-null
    "1" = c(-abs(nullInterval), abs(nullInterval)),
    "2" = nullInterval
  )

  new("bspm1dHypothesis", prior_rscale = prior_rscale, nullInterval = H0)
}


# -------------------------------------------------------------------------
# S4 class for bspm1dBFttest ----------------------------------------------

#' @title bspm1dBFttest
#' @description An S4 class bspm1dBFttest
#' @exportClass bspm1dBFttest
setClass(
  "bspm1dBFttest",
  slots = c(
    ttest   = "data.frame",
    dimname = "character",
    outcome = "character",
    paired  = "logical",
    group   = "character"
  ),
  prototype = list(
    ttest   = NULL,
    dimname = NA_character_,
    outcome = NA_character_,
    paired  = NA,
    group   = NA_character_
  )
)


#' @title bspm_bfttest
#'
#' @description
#' This function calculates a Bayes-Factor version of the SPM{t}-test for 1-dimensional data (two-sample or paired sample t-test) as described in Serrien et al. (2019).
#'
#' @param bspm1dData S4 object of class bspm1dData as created with `bspm_data`
#' @param bspm1dHypothesis S4 object of class bspm1dHypothesis as created with `bspm_hyp`
#'
#' @import BayesFactor
#'
#' @references Serrien, B., Goossens, M., & Baeyens, J. P. (2019). Statistical parametric mapping of biomechanical one-dimensional data with Bayesian inference. International Biomechanics, vol. 6 (1), 9-18
#'
#' @export
bspm_ttest <- function(bspm1dData, bspm1dHypothesis) {
  stopifnot(
    "bspm1dData should be an S4 object"          = isS4(bspm1dData),
    "bspm1dData should be of class 'bspm1dData'" = "bspm1dData" %in% class(bspm1dData)
  )

  new("bspm1dBFttest", ttest = ttest,
      dimname = dimname, outcome = outcome,
      paired = paired, group = group)
}


#' #' bspm_bfttest
#' #'
#' #' @description
#' #' This function calculates a Bayes-Factor version of the SPM{t}-test for 1-dimensional data (two-sample or paired sample t-test) as described in Serrien et al. (2019).
#' #'
#' #' @param data data.frame in long format containing the dataset
#' #' @param group name of the grouping variable (paired or independent groups) (column in `data`)
#' #' @param dimension name of the variable with the 1-dimensional domain (column in `data`)
#' #' @param outcome name of the outcome variable (column in `data`)
#' #' @param paired logical, are the data from a paired (TRUE) or independent group (FALSE) design?, see BayesFactor::ttestBF()
#' #' @param nullInterval see BayesFactor::ttestBF()
#' #' @param rscale see BayesFactor::ttestBF()
#' #'
#' #' @references Serrien, B., Goossens, M., & Baeyens, J. P. (2019). Statistical parametric mapping of biomechanical one-dimensional data with Bayesian inference. International Biomechanics, vol. 6 (1), 9-18
#' #'
#' #' @return description
#' #'
#' #' @export
#' bspm_bfttest <- function(data, group, dimension, outcome, paired,
#'                          nullInterval = c(-0.2, 0.2),
#'                          rscale = "medium") {
#'   # checking input arguments
#'   check_args()
#'
#'   # point-wise calculations: Bayes-Factor & posterior probability
#'   bf_pp <- bspm_bfpp(data, group, dimension, outcome, paired,
#'                      nullInterval, rscale)
#'   # Q-value calculation
#'
#'   # identification of supra-threshold clusters
#'
#'   return(ppt)
#' }
#'
#'
#' #' bspm_bf
#' #'
#' #' @description
#' #' A short description...
#' #'
#' #' @import BayesFactor
#' #' @export
#' bspm_bf <- function(data1, data2, ...) {
#'   bfi <- BayesFactor::ttestBF(
#'     data1, data2,
#'     nullInterval = nullInterval, rscale = rscale, paired = paired
#'   )
#'   return(bfi[2] / bfi[1])
#' }
#'
#' #' bspm_bf2pp
#' #'
#' #' @description
#' #' Transform the Bayes Factor to a posterior probability, assuming equal prior odds for the competing hypotheses. See BayesFactor::newPriorOdds() and BayesFactor::as.BFprobability().
#' #'
#' #' @param bf an object of class `BFBayesFactor`
#' #' @import BayesFactor
#' #' @export
#' bspm_bf2pp <- function(bf) {
#'   prior.odds <- BayesFactor::newPriorOdds(bf, type = "equal")
#'   post.odds  <- prior.odds * bf
#'   return( BayesFactor::as.BFprobability(post.odds) )
#' }
#'
#' #' bspm_bfpp
#' #'
#' #' @description
#' #' Pointwise calculation of the BayesFactor and transformation to posterior probabilities.
#' #'
#' #' @seealso [bspm_bfttest(), bspm_bf(), bspm_bf2pp()]
#' #'
#' #' @export
#' bspm_bfpp <- function(data, group, dimension, outcome, paired,
#'                       nullInterval, rscale) {
#'   # make numeric group names
#'   data[[group]] <- as.numeric(as.factor(data[[group]]))
#'   pw <- data %>%
#'     select(all_of(c(dimension, group, outcome))) %>%
#'     pivot_wider(values_from = outcome, names_from = group,
#'                 names_prefix = "Y_", values_fn = list) %>%
#'     mutate(
#'       bft = map2(Y_1, Y_2, .f = bspm_bf,
#'                  paired = paired,
#'                  nullInterval = nullInterval,
#'                  rscale = rscale),
#'       ppt = map(bft, .f = bspm_bf2pp)
#'     )
#'   bf_10 <- sapply(pw$bft, FUN = function(x) as.numeric(as.vector(x)))
#'   pp_h1 <- sapply(pw$ppt, FUN = function(x) as.numeric(as.vector(x)))[1,]
#'   pp_h0 <- sapply(pw$ppt, FUN = function(x) as.numeric(as.vector(x)))[2,]
#'   df <- data.frame(bf_10, pp_h1, pp_h0)
#'   df %>%
#'     mutate(!!dimension := unique(data[[dimension]]),
#'            .before = bf_10)
#' }
#'
#' #' bspm_pp2qv
#' #' @description
#' #' A short description...
#' #'
#' bspm_pp2qv <- function() {
#'
#' }
#'
