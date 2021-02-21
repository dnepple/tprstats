#' Linear Hypothesis Test (Cross Section Data)
#'
#' Tests a linear hypothesis about coefficients. The calculation corrects for possible presence of heteroskedasticity using the method developed by White. This method is appropriate for cross-section data.
#'
#' @param model Regression model being tested.
#' @param conditions Hypothesis being tested.
#'
#' @section Details:
#' Let the regression model be: linmod=lm(y~x1+x2+x3+x4)
#'
#' To test the hypothesis that the coefficient of x1 equals the coefficient of x2 and that the coefficient of x3 equals twice the coefficient of x4
#'
#' coefTestH(linmod,c("x1=x2","x3=2*x4"))
#'
#' The F-statistic and p-value for the hypothesis test are reported.
#' @export

coefTestH <- function(model, conditions){
  car::linearHypothesis(model, hypothesis.matrix = c(conditions), white.adjust = "hc1")
}

#' Linear Hypothesis Test (Time Series Data)
#'
#' Tests a linear hypothesis about coefficients. The calculation corrects for possible presence of heteroskedasticity and autocorrelation using the method developed by Newey and West. This method is appropriate for time series data.
#'
#' @param model Regression model being tested.
#' @param conditions Hypothesis being tested.
#' @section Details:
#' Let the regression model be: linmod=lm(y~x1+x2+x3+x4)
#'
#' To test the hypothesis that the coefficient of x1 equals the coefficient of x2 and that the coefficient of x3 equals twice the coefficient of x4
#'
#' coefTestHAC(linmod,c("x1=x2","x3=2*x4"))
#'
#' The F-statistic and p-value for the hypothesis test are reported.
#' @export

coefTestHAC <- function(model,conditions){
  car::linearHypothesis(model,vcov=sandwich::vcovHAC.default(model),c(conditions))
}
