% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence_interval_functions.R
\name{CIcoefHAC}
\alias{CIcoefHAC}
\title{Confidence Intervals for Coefficients (Time Series Data)}
\usage{
CIcoefHAC(linmod, alpha = 0.05)
}
\arguments{
\item{linmod}{The regression for which confidence intervals of coefficients are to be calculated.}

\item{alpha}{Significance level. Defaults to 0.05.}
}
\description{
Calculates confidence intervals for coefficients of a regression model. The calculation corrects for possible presence of heteroskedasticity and autocorrelation using the method developed by Newey and West. This method is appropriate for time series data.
}
