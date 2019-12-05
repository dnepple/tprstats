#' Histogram with Confidence Interval
#'
#' Plots a histogram with confidence interval.
#'
#' @param .data The data.
#' @param alpha Alpha
#' @param title Graph title.
#' @param xlabel Graph x label.
#' @param breaks Histogram breaks. Defaults to 20.
#'
#' @export

hist_CI <- function(.data, alpha, title, xlabel, breaks = 20) {
  graphics::hist(.data,
    breaks = breaks, main = title,
    xlab = xlabel, col = "light blue"
  )

  CIlow <- round(stats::quantile(.data, alpha / 2), 2)
  CIhi <- round(stats::quantile(.data, 1 - alpha / 2), 2)
  graphics::abline(v = CIlow, col = "red")
  graphics::abline(v = CIhi, col = "red")
  adjlow <- .01 + (CIlow - min(.data)) / (max(.data) - min(.data))
  adjhi <- -.009 + (CIhi - min(.data)) / (max(.data) - min(.data))
  graphics::mtext(CIlow, 1, adj = adjlow, .2, col = "red")
  graphics::mtext(CIhi, 1, adj = adjhi, .2, col = "red")
  graphics::mtext("alpha = ", 3, adj = .45, col = "red")
  graphics::mtext(alpha, 3, adj = .60, col = "red")
}
