#' tprstats setup
#'
#' Installs additional course packages not already installed as tprstats dependencies. Loads these packages if load = TRUE.
#'
#' @param load Determines whether packages should be loaded. Defaults to TRUE.
#'
#' @export

setup <- function(load = TRUE) {
  pkg <- c("EnvStats", "sandwich", "MASS", "pwr", "stargazer", "pwrAB", "ggplot2", "plot3D", "margins", "forecast", "e1071", "rgenoud", "fitdistrplus", "fastDummies", "alabama", "sn", "ramify", "car", "grid", "ivreg", "readxl", "tidyverse", "tprstats")
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    utils::install.packages(new.pkg, dependencies = TRUE)
  }
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
