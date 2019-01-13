#' Scaled Coefficients
#'
#' Calculates standardized coefficients (also known as beta weights) and elasticities. (A standardized coefficient is the coefficient obtained from a regression in which both the independent variable and the dependent variable are standardized to have mean equal to zero and standard deviation equal to one.) The function reports three columns which contain the coefficients, the standardized coefficients, and the elasticities evaluated at the means of the variables.
#'
#' @param linmod The linear regression equation for which scaled coefficients are to be calculated.
#' @export
#'

scaledCoefficients <- function(linmod) {
    coefficients <- (data.frame(stats::coef(linmod)))
    names(coefficients) <- c("Coefficients")
    stdCoefficients <- tprstats::stdcoefs(linmod)
    elasticities <- tprstats::elasticities(linmod)
    round(cbind(coefficients, stdCoefficients, elasticities), 4)
}
