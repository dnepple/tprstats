#' Summary of Regression (Cross Section Data)
#'
#' This function displays regression results with standard errors adjusted for possible presence of heteroskedasticity using the method developed by White. This method is appropriate for cross-section data.
#'
#'@param model The linear regression model.
#'@export

summaryH <- function(model) {

    type <- "hc1"
    V <- car::hccm(model, type = type)
    sumry <- summary(model)
    table <- stats::coef(sumry)
    table[, 2] <- sqrt(diag(V))
    table[, 3] <- table[, 1]/table[, 2]
    table[, 4] <- 2 * stats::pt(abs(table[, 3]), stats::df.residual(model),
        lower.tail = FALSE)

    sumry$coefficients <- table
    p <- nrow(table)
    hyp <- cbind(0, diag(p - 1))
    sumry$fstatistic[1] <- car::linearHypothesis(model, hyp, white.adjust = type)[2,
        "F"]

    print(sumry, signif.stars = FALSE)
    cat("Note: Heteroscedasticity-consistent standard errors using adjustment",
        type, "\n")

}

#' Summary of Regression (Time Series Data)
#'
#' This function displays regression results with standard errors adjusted for possible presence of heteroskedasticity and autocorrelation using the method developed by Newey and West.  This method is appropriate for time series data.
#'
#'@param model The linear regression model.
#'@export

summaryHAC <- function(model) {

    V <- sandwich::vcovHAC.default(model)
    sumry <- summary(model)
    table <- stats::coef(sumry)
    table[, 2] <- sqrt(diag(V))
    table[, 3] <- table[, 1]/table[, 2]
    table[, 4] <- 2 * stats::pt(abs(table[, 3]), stats::df.residual(model),
        lower.tail = FALSE)

    sumry$coefficients <- table
    p <- nrow(table)
    hyp <- cbind(0, diag(p - 1))
    sumry$fstatistic[1] <- car::linearHypothesis(model, hyp, white.adjust = FALSE)[2,
        "F"]

    print(sumry, signif.stars = FALSE)
    cat("Note: Newey-West standard errors")
}

#' Summary for Regression with Autoregressive Errors
#'
#' Displays results of regression estimated with autoregressive errors. The function will also provide a summary for a regression with both autoregressive and moving average terms.
#'
#'@param arima_mod The regression model with autoregressive errors.
#'
#'@section Details:
#'Consider a regression of y on x1, x2, and x3 that is estimated with second-order autoregressive errors as follows: \cr
#'ar2Reg=with(mydata, Arima(y, xreg=cbind(x1,x2,x3), order=c(2,0,0), include.constant = TRUE))
#'
#'The results of this regression are then reported by the following command: \cr
#'summaryArima(ar2Reg)
#'
#'Consider a regression of y on x1, x2, and x3 that is estimated with both first-order autoregressive and first-order moving average errors as follows: \cr
#'armaReg=with(mydata, Arima(y, xreg=cbind(x1,x2,x3), order=c(1,0,1), include.constant = TRUE))
#'
#'The results of this regression are then reported by the following command: \cr
#'summaryArima(armaReg)
#'@export

summaryArima <- function(arima_mod) {
    Coefficients <- arima_mod$coef
    t_stats <- abs(arima_mod$coef/sqrt(diag(arima_mod$var.coef)))
    pvals = round(2 * stats::pt((t_stats), arima_mod$nobs - NROW(t_stats),
        lower = FALSE), 4)
    Rsq <- 1 - (arima_mod$sigma2)/stats::var(arima_mod$fitted + arima_mod$residuals)
    log_likelihood <- arima_mod$loglik
    akaike <- arima_mod$aicc
    std_error <- (arima_mod$sigma2)^0.5
    # Print Results
    print(round(cbind(Coefficients, t_stats, pvals), 4))
    cat("\n")
    print(cbind(Rsq, log_likelihood, akaike, std_error))
    cat("\n Regression with Autoregressive Errors")
}

#' Summary Statistics
#'
#' Returns mean, median, max, min and std. deviation for each variable in the data.
#'
#' @param .data The data.
#' @export

summaryStats = function(.data){
    Mean=round(sapply(.data,mean),4)
    Stdev=round(sapply(.data,stats::sd),4)
    Min=round(sapply(.data,min),4)
    Median=round(sapply(.data,stats::median),4)
    Max=round(sapply(.data,max),4)
    sumst=rbind(Mean,Median,Max,Min,Stdev)
    sumst
}
