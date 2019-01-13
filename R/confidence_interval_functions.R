#' Confidence Intervals for Coefficients (Cross Section Data)
#'
#' Calculates confidence intervals for coefficients of a regression model. The calculation corrects for possible presence of heteroskedasticity using the method developed by White. This method is appropriate for cross-section data.
#'
#' @param linmod The regression for which confidence intervals of coefficients are to be calculated.
#' @export

CIcoefH <- function(linmod) {
    alpha <- 0.05
    cov <- sandwich::vcovHC(linmod, type = "HC1")
    robust.se <- sqrt(diag(cov))
    numobs <- NROW(linmod$residuals)
    numcoef <- NROW(robust.se)
    df <- numobs - numcoef
    coefs <- summary(linmod)$coefficients[1:numcoef]
    tcrit <- stats::qt(1 - alpha/2, df)
    lci <- coefs - robust.se * tcrit
    uci <- coefs + robust.se * tcrit
    ourci <- cbind(coefs, lci, uci)
    print("Coefficients and Heteroskedasticity Robust 95% Confidence Bounds")
    print(ourci)
}


#' Confidence Intervals for Coefficients (Time Series Data)
#'
#' Calculates confidence intervals for coefficients of a regression model. The calculation corrects for possible presence of heteroskedasticity and autocorrelation using the method developed by Newey and West. This method is appropriate for time series data.
#'
#' @param linmod The regression for which confidence intervals of coefficients are to be calculated.
#' @export

CIcoefHAC <- function(linmod) {
    alpha <- 0.05
    cov <- sandwich::vcovHAC.default(linmod)
    robust.se <- sqrt(diag(cov))
    numobs <- NROW(linmod$residuals)
    numcoef <- NROW(robust.se)
    df <- numobs - numcoef
    coefs <- summary(linmod)$coefficients[1:numcoef]
    tcrit <- stats::qt(1 - alpha/2, df)
    lci <- coefs - robust.se * tcrit
    uci <- coefs + robust.se * tcrit
    ourci <- cbind(coefs, lci, uci)
    print("Coefficients and HAC Robust 95% Confidence Bounds")
    print(ourci)
}
