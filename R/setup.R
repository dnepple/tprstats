#' tprstats setup
#'
#' Loads additional packages(tprstats "Suggested" packages) used in the course. Load order matters to prevent naming conflicts.
#'
#' @export
setup <- function() {
  # order matters when loading pkgs to prevent naming conflicts
  pkg <- c("EnvStats", "sandwich", "MASS", "pwr", "stargazer", "pwrAB", "ggplot2", "plot3D", "margins", "forecast", "e1071", "rgenoud", "fitdistrplus", "fastDummies", "alabama", "sn", "ramify", "car", "grid", "ivreg", "readxl", "tidyverse", "tprstats")

  # copy pkg list to package DESCRIPTION under Suggests
  # sapply(pkg, usethis::use_package, type = "Suggests")

  sapply(pkg, require, character.only = TRUE)
}

#' Deprecated Setup for tprstats
#'
#' This command is now identical to the tprstats::setup() command. This command just calls tprstats::setup().
#'
#' @export
#'
setup_tprstats <- function() {
  tprstats::setup()
}
