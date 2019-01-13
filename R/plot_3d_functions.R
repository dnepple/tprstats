#' 3D Plot
#'
#' 3D scatter plot with regression plane.

#' @param varx name of variable on one horizontal axis. Use quotes.
#' @param vary name of variable on other horizontal axis. Use quotes.
#' @param varz name of variable on vertical axis. Use quotes.
#' @param theTheta Theta value for viewing angle.
#' @param thePhi Phi value for viewing angle.
#' @param data The dataframe containing data.
#'
#' @section Details:
#' Each of the commands below produces a plot of the data and regression plane with x1 and x2 on the horizontal axes and y on the vertical axis. The first command produces a plot with viewing angle theta=50 and phi=15. The second produces a plot with viewing angle theta=60 and phi=20.
#'
#' go3DPlot("x1","x2","y",50,15,data=Plot_3D_Example)
#'
#' go3DPlot("x1","x2","y",60,20,data=Plot_3D_Example)
#' @export


go3DPlot <- function(varx, vary, varz, theTheta, thePhi, data) {

    if(!requireNamespace("plot3D", quietly = TRUE)){
        stop("Package \"plot3D\" needed for this function to work. Please install it.")
    }

    x <- data[[varx]]
    y <- data[[vary]]
    z <- data[[varz]]
    # Compute the linear regression (z = ax + by + d)
    fit <- stats::lm(z ~ x + y)
    # predict values on regular xy grid
    grid.lines = 26
    x.pred <- seq(min(x), max(x), length.out = grid.lines)
    y.pred <- seq(min(y), max(y), length.out = grid.lines)
    xy <- expand.grid(x = x.pred, y = y.pred)
    z.pred <- matrix(stats::predict(fit, newdata = xy), nrow = grid.lines, ncol = grid.lines)
    # fitted points for droplines to surface
    fitpoints <- stats::predict(fit)
    # scatter plot with regression plane
    plot3D::scatter3D(x, y, z, pch = 19, cex = 0.7, col = "black", theta = theTheta, phi = thePhi,
        ticktype = "detailed", xlab = varx, ylab = vary, zlab = varz, surf = list(x = x.pred,
            y = y.pred, z = z.pred, facets = NA, fit = fitpoints, col = "blue"), main = paste(varz,
            "as a function of", varx, "and", vary, sep = " "))
}
