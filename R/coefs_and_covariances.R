#' Coefficients and Covariance Matrix (Cross Section Data)
#'
#' Provides coefficients of a regression model and the covariance matrix of the coefficients. The calculation of the covariance matrix corrects for possible presence of heteroskedasticity using the method developed by White. This method is appropriate for cross-section data.
#'
#' @param linmod The regression model.
#' @section Details:
#' Let the regression model be: linmod=lm(y~x1+x2)
#'
#' The function reports the 3 coefficients from the regression in the first column and the 3x3 covariance matrix of the coefficients in the next three columns.
#' @export

coefCovH <- function(linmod) {
    coefs = stats::coefficients(linmod)
    covmatH = sandwich::vcovHC(linmod, type = "HC1")
    cbind(coefs, covmatH)
}

#' Coefficients Covariance Matrix (Time Series Data)
#'
#' Provides coefficients of a regression model and the covariance matrix of the coefficients. The calculation of the covariance matrix corrects for possible presence of heteroskedasticity and autocorrelation using the method developed by Newey and West. This method is appropriate for time series data.
#'
#' @param linmod The regression model.
#'
#' @section Details:
#' Let the regression model be: linmod=lm(y~x1+x2)
#'
#' The function reports the 3 coefficients from the regression in the first column and the 3x3 covariance matrix of the coefficients in the next three columns.
#' @export

coefCovHAC <- function(linmod) {
    coefs = stats::coefficients(linmod)
    covmatHAC = sandwich::vcovHAC.default(linmod)
    cbind(coefs, covmatHAC)
}

