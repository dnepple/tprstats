#' tprstats setup
#'
#' Installs course packages not already installed as tprstats dependencies.
#'
#'@param suppress Suppresses warnings for students by default. Suppress can be set to false for troubleshooting.
#'
#' @export

setup_tprstats = function(suppress = TRUE){

  packages<-c("pwr", "ISLR", "plm", "stargazer", "DAAG")

  if(suppress)
    suppressWarnings(check.packages(packages))
  else
    check.packages(packages)
}

#' Check Packages
#'
#' Check to see if packages are installed and installs them if they are not. Verifies install using require.
#'
#' @param pkg List of packages to install.

check.packages <- function(pkg){
  # get list of uninstalled packages in list pkg
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  # install any uninstalled packages
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  # check whether all packages can be successfully loaded
  installed.pkg <- sapply(pkg, require, character.only = TRUE)

  if(all(installed.pkg))
    print("Setup was successful.")
  else
    print("Setup failed. One or more packages could not be installed.")
}
