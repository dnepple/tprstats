#' Plot Log Actual Fitted
#'
#' @param linmod Linear model.
#' @param MyData The data.
#' @param col_name Name of column variable as a string (use quotes).
#'
#' @export
plotLogActualFitted <- function(linmod, MyData, col_name) {
  Y <- unlist(MyData[, col_name])
  Observation <- seq(1, NROW(MyData))
  Pred_and_PI <- exp(stats::predict(linmod, MyData, interval = "predict"))
  lyhat <- Pred_and_PI[, 1]
  Lower <- Pred_and_PI[, 2]
  Upper <- Pred_and_PI[, 3]
  graphics::plot(Y ~ Observation, pch = 20, main = "Actual (black), Predicted (red), and 95% PI (blue)")
  graphics::lines(lyhat ~ Observation, lw = 1, col = "red")
  graphics::lines(Upper ~ Observation, col = "blue", lw = 1)
  graphics::lines(Lower ~ Observation, col = "blue", lw = 1)
  graphics::grid()
}
