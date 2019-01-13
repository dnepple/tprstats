#' Coefficient Elasticities
#'
#' Elasticities evaluated at the means of the variables are calculated for the coefficients of a linear regression model.
#'
#' @param linmod The linear regression model.
#' @export


elasticities <- function(linmod) {
    Ncoef <- nrow(data.frame(linmod$coefficients))
    for (i in 1:Ncoef) {
        el <- as.numeric(linmod$coefficients[i] * colMeans(stats::model.matrix(linmod))[i]/mean(stats::fitted(linmod)))
        ifelse(i == 1, elasticity <- NA, elasticity <- rbind(elasticity, el))
    }
    rownames(elasticity) <- names(stats::coef(linmod))
    colnames(elasticity) <- "elasticities"

    return(data.frame(elasticity))
}


