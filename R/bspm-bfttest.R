# bspm_bfttest.R
# set of R functions to perform a Bayesian SPM{t}-test for 1-dimensional data

#' bspm_bfttest
#'
#' @description
#' This function calculates a Bayes-Factor version of the SPM{t}-test for 1-dimensional data (two-sample or paired sample t-test) as described in Serrien et al. (2019).
#'
#' @param data data.frame in long format containing the dataset
#' @param group name of the grouping variable (paired or independent groups) (column in `data`)
#' @param dimension name of the variable with the 1-dimensional domain (column in `data`)
#' @param outcome name of the outcome variable (column in `data`)
#' @param paired logical, are the data from a paired (TRUE) or independent group (FALSE) design?, see BayesFactor::ttestBF()
#' @param nullInterval see BayesFactor::ttestBF()
#' @param rscale see BayesFactor::ttestBF()
#'
#' @references Serrien, B., Goossens, M., & Baeyens, J. P. (2019). Statistical parametric mapping of biomechanical one-dimensional data with Bayesian inference. International Biomechanics, vol. 6 (1), 9-18
#'
#' @return description
#'
#' @export
bspm_bfttest <- function(data, group, dimension, outcome, paired,
                         nullInterval = c(-0.20, 0.20),
                         rscale = "medium") {
  # checking arguments
  # => move to a separate function
  stopifnot(
    !is.null(data1),
    !is.null(data2),
    length(data1) >= 3,
    length(data2) >= 3,
    ifelse(paired, length(data1) == length(data2), TRUE),
    length(nullInterval) == 2,
    rscale %in% c("medium", "wide", "ultrawide")
  )

  # point-wise calculations: Bayes-Factor & posterior probability
  bft <- bspm_bf(data1, data2, nullInterval, rscale)
  ppt <- bspm_bf2pp(bft)

  # Q-value calculation

  # identification of supra-threshold clusters

  return(ppt)
}

#' check_data
#'
#' @param data description
#'
#' @export
check_data <- function() {

}

#' bspm_bf
#'
#' @description
#' A short description...
#'
bspm_bf <- function(data1, data2, nullInterval, rscale) {
  bfi <- ttestBF(data1, data2,
                 nullInterval = nullInterval,
                 rscale = rscale)
  return(bfi[2] / bfi[1])
}

#' bspm_bf2pp
#'
#' @description
#' Transform the Bayes Factor to a posterior probability, assuming equal prior odds for the competing hypotheses. See BayesFactor::newPriorOdds() and BayesFactor::as.BFprobability().
#'
#' @param bf an object of class `BFBayesFactor`
#' @import BayesFactor
#' @export
bspm_bf2pp <- function(bf) {
  prior.odds <- BayesFactor::newPriorOdds(bf, type = "equal")
  post.odds  <- prior.odds * bf
  return( BayesFactor::as.BFprobability(post.odds) )
}

#' bspm_pp2qv
#' @description
#' A short description...
#'
bspm_pp2qv <- function() {

}

