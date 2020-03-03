#' Plot Objective Function
#'
#' Plots x to mean of the input function over the input range.
#'
#' @param fn A function.
#' @param from Minimum x value.
#' @param to Maximum x value.
#'
#' @return results A table.
#' @export
plotObjectiveFunction <- function(fn, from, to) {
  results <- data.frame("xvar" = from:to)
  # fn is assumed to return a vector and the mean is taken
  results$yvar <- sapply(X = results$xvar, FUN = function(x) {
    mean(fn(x))
  })

  graphics::plot(yvar~xvar, data = results, xlab = "x", ylab = "y", pch = 16)

  return(results)
}
