#' tprstats setup
#'
#' Installs additional course packages not already installed as tprstats dependencies. Loads these packages if load = TRUE.
#'
#'@param load Determines whether packages should be loaded. Defaults to TRUE.
#'
#' @export

setup <- function(load = TRUE){
  pkg<-c("pwr", "ISLR", "plm", "stargazer", "DAAG","nlme", "pwrAB","scales","ggplot2","readxl","plot3D", "margins", "forecast", "lift","tidyverse","StatMeasures","MASS","e1071","OOR","rgenoud","fitdistrplus","fastDummies", "alabama", "sn", "ramify", "readxl","tprstats")
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    utils::install.packages(new.pkg, dependencies = TRUE)
  if (load)
    sapply(pkg, require, character.only = TRUE)
}
