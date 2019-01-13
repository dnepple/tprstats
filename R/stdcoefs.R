#' Standardized Regression Coefficients
#'
#' Calculates standardized regression coefficients, also known as beta weights. A standardized coefficient is the coefficient obtained from a regression in which both the independent variable and the dependent variable are standardized to have mean equal to zero and standard deviation equal to one.
#'
#' @param linmod The linear regression model.
#' @export


stdcoefs <- function(linmod) {
    Ncoef <- nrow(data.frame(linmod$coefficients))
    for (i in 1:Ncoef) {
        el <- as.numeric((linmod$coefficients[i]) * (apply(stats::model.matrix(linmod),
            2, stats::sd)[i]))/stats::sd(stats::fitted(linmod) + stats::residuals(linmod))
        ifelse(i == 1, stdcoef <- NA, stdcoef <- rbind(stdcoef, el))
    }
    rownames(stdcoef) <- names(stats::coef(linmod))
    colnames(stdcoef) <- "stdcoefs"

    return(data.frame(stdcoef))
}
