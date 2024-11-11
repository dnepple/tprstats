#' tprstats setup
#'
#' Loads additional packages(tprstats "Suggested" packages) used in the course. Load order matters to prevent naming conflicts.
#'
#' @param load Determines whether packages should be loaded. Defaults to TRUE.
#'
#' @export

setup <- function(load = TRUE) {
  # order matters when loading pkgs to prevent naming conflicts
  pkg <- c("EnvStats", "sandwich", "MASS", "pwr", "stargazer", "pwrAB", "ggplot2", "plot3D", "margins", "forecast", "e1071", "rgenoud", "fitdistrplus", "fastDummies", "alabama", "sn", "ramify", "car", "grid", "ivreg", "readxl", "tidyverse", "tprstats")

  # copies pkg list to package DESCRIPTION as Suggests
  # sapply(pkg, usethis::use_package, type = "Suggests")

  if (load) {
    sapply(pkg, require, character.only = TRUE)
  }
}

#' Deprecated Setup for tprstats
#'
#' This command is now identical to the tprstats::setup() command. This command just calls tprstats::setup().
#'
#' @param load Indicates of package should be loaded (load = TRUE) or if setup should just check the packages have been installed (load = FALSE)
#' @export
#'
setup_tprstats <- function(load = TRUE) {
  tprstats::setup(load)
}
