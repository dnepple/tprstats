#' Ramsey Specification Test
#'
#' This function conducts the Ramsey Regression Specification Error Test (RESET) for a regression model. The function conducts the Ramsey test with one fitted term and then with two fitted terms. The p-values for both tests are reported.
#'
#' @param linmod Linear model.
#' @export

ramseyTest <- function(linmod) {
    One_Fitted_Term <- lmtest::resettest(linmod, power = 2, type = "fitted")[4]
    Two_Fitted_Terms <- lmtest::resettest(linmod, power = 2:3, type = "fitted")[4]
    cat("Ramsey Specification Test \n")
    rbind(One_Fitted_Term, Two_Fitted_Terms)
}
