#' Control Chart
#'
#' This function plots a control chart for monitoring a production process for a product with a continuous measure (e.g., weight of a package).
#'
#'@param chartdata The name of the variable to be plotted.
#'@param mu The anticipated mean when the production process is working properly.
#'@param sig The anticipated standard deviation when the process is working properly.
#'@param n The number of observations per sample.
#'@param alpha The significance level.
#'@export

controlChart <- function(chartdata, mu, sig, n, alpha) {
    m = seq(1, NROW(chartdata))
    ymax = max(chartdata, mu + stats::qnorm(1 - alpha/2) * 1.1 * sig/n^0.5)
    ymin = min(chartdata, mu - stats::qnorm(1 - alpha/2) * 1.1 * sig/n^0.5)
    graphics::plot(chartdata ~ m, ylim = c(ymin, ymax), xlab = "Observation Number", ylab = "Sample Means")
    graphics::lines(chartdata ~ m, )
    graphics::abline(h = mu, col = "Red")
    graphics::abline(h = mu - stats::qnorm(1 - alpha/2) * sig/n^0.5, col = "Blue")
    graphics::abline(h = mu + stats::qnorm(1 - alpha/2) * sig/n^0.5, col = "Blue")
}

#' Control Chart Binary
#'
#' This function plots a control chart for monitoring a production process for binary outcomes.
#'
#'@param chartdata The name of the variable to be plotted.
#'@param p The target probability for the outcome expressed as a value between 0 and 1.
#'@param n The number of observations per sample.
#'@param alpha The significance level.
#'@export

controlChartBinary <- function(chartdata, p, n, alpha) {
    sig = (p * (1 - p))^0.5
    m = seq(1, NROW(chartdata))
    ymax = max(chartdata, p + stats::qnorm(1 - alpha/2) * 1.1 * sig/n^0.5)
    graphics::plot(chartdata ~ m, ylim = c(0, ymax), xlab = "Observation Number", ylab = "Sample Means of Defect Rates")
    graphics::lines(chartdata ~ m)
    graphics::abline(h = p, col = "Red")
    graphics::abline(h = p + stats::qnorm(1 - alpha/2) * sig/n^0.5, col = "Blue")
    graphics::abline(h = max(0, p - stats::qnorm(1 - alpha/2)* sig/n^0.5), col = "Blue")
}



