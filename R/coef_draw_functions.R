#' coefDrawH
#'
#' Given a regression (linmod), this function takes a random draw for the coefficients.
#'
#' @param linmod Linear regression model.
#'
#' @return mydraw A random draw for the coefficients.
#' @export
coefDrawH <- function(linmod) {
  # Extract matrix containing coefficients and Covariance matrix
  mycovmat <- coefCovH(linmod)
  # Extract vector containing coefficients
  mycoefs <- stats::coefficients(linmod)
  # Transposing mycoefs twice returns a column vector
  mycoefs <- t(mycoefs)
  mycoefs <- t(mycoefs)
  # Extract covariance matris
  mycov <- mycovmat[, 2:NCOL(mycovmat)]
  # Take a draw from the multivariate normal
  mydraw <- MASS::mvrnorm(1, mycoefs, mycov)
  return(mydraw)
}

#' coefDrawHAC
#'
#' Tthis function takes a random draw for the coefficients of a time series regression.
#'
#' @param linmod Linear regression model.
#'
#' @return mydraw A random draw for the coefficients.
#' @export
coefDrawHAC <- function(linmod) {
  mycovmat <- coefCovHAC(linmod)
  # Extract vector containing coefficient
  mycoefs <- stats::coefficients(linmod)
  # Transposing mycoefs twice returns a column vector
  mycoefs <- t(mycoefs)
  mycoefs <- t(mycoefs)
  # Extract covariance matris
  mycov <- mycovmat[, 2:NCOL(mycovmat)]
  # Take a draw from the multivariate normal
  mydraw <- MASS::mvrnorm(1, mycoefs, mycov)
  return(mydraw)
}
