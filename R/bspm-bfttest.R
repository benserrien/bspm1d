#' # bspm_bfttest.R
#' # set of R functions to perform a Bayesian SPM{t}-test for 1-dimensional data
#'
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
