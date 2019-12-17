#' Histogram with Confidence Interval
#'
#' Plots a histogram with confidence interval.
#'
#' @param .data The data.
#' @param alpha Alpha value.
#' @param main Graph title.
#' @param xlab Graph x label.
#' @param breaks Histogram breaks. Defaults to 20.
#'
#' @export

hist_CI <- function(.data, alpha, main = "Histogram", xlab = "Data and (1-alpha)% Confidence Interval", breaks = 20) {
  graphics::hist(.data,
    breaks = 20, main = main, xlab = xlab,
    col = "light blue"
  )

  if(is.na(.data)){
    print("Data has missing values. Missing values will be omitted.")
    .data <- stats::na.omit(.data)
  }
  CIlow <- round(stats::quantile(.data, alpha / 2), 2)
  CIhi <- round(stats::quantile(.data, 1 - alpha / 2), 2)
  graphics::abline(v = CIlow, col = "red")
  graphics::abline(v = CIhi, col = "red")
  adjlow <- 0.01 + (CIlow - min(.data)) / (max(.data) - min(.data))
  adjhi <- -0.009 + (CIhi - min(.data)) / (max(.data) - min(.data))
  graphics::mtext(CIlow, 1, adj = adjlow, 0.2, col = "red")
  graphics::mtext(CIhi, 1, adj = adjhi, 0.2, col = "red")
  graphics::mtext("alpha = ", 3, adj = 0.45, col = "red")
  graphics::mtext(alpha, 3, adj = 0.6, col = "red")
  ave <- round(mean(.data), 2)
  graphics::mtext("mean = ", 1, adj = .42, 1.8, col = "red")
  graphics::mtext(ave, 1, adj = 0.61, 1.8, col = "red")
}
