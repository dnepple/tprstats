
# Best-fitting Distribution -----------------------------------------------

#' Select Distribution
#'
#' Fits up to 8 different distrubtions to a set of data and recommends the best-fitting distribution, where best-fitting is considered to be the distribution with the smallest AIC value. Prints the distribution parameters and R instructions for sampling the recommended distribution. Outputs a histogram of the data overlayed by the density function of the recommended distribution.
#'
#' @param .data Data.
#' @export
select_distribution <- function(.data) {
  if (anyNA(.data)) {
    print("Data has missing values. These values will be omitted.")
  }
  .data <- as.numeric(stats::na.omit(.data))
  dist <- get_recommended_distribution(.data)
  cat("Recommended distribution:", dist$distname, "\n")
  print_distribution_parameters(dist)
  print_distribution_instructions(dist)
  make_histogram(.data, dist)
}

#' Recommend Distribution
#'
#' Fits up to 8 different distrubtions to a set of data and recommends the best-fitting distribution, where best-fitting is considered to be the distribution with the smallest AIC value.
#'
#' @param .data Data.
#'
#' @return List of parameters for the recommended distribution.
get_recommended_distribution <- function(.data) {
  # this function can also return a list of all distribution sorted in ascending order of AIC value
  dists <- list(
    fitdistrplus::fitdist(.data, "norm"),
    fitdist_parameters_uniform(.data),
    suppressWarnings(fitdist_parameters_t(.data)),
    fitdist_parameters_skew_normal(.data)
  )

  if (min(.data) > 0) {
    dists_positive <- list(
      fitdistrplus::fitdist(.data, "weibull"),
      fitdist_parameters_gamma(.data),
      fitdistrplus::fitdist(.data, "lnorm"),
      fitdist_parameters_exponential(.data)
    )
    dists <- c(dists, dists_positive)
  }

  # sorts in ascending order (ie. first value is smallest aic)
  sorted_dists <- rlist::list.sort(dists, aic)
  # returns the distribution with the smallest aic
  min_aic_dist <- sorted_dists[[1]]
  min_aic_dist
}


# Fit Distribution to Data --------------------------------------------------------

#' Fit Distribution
#'
#' Fits a distribution to data.The abbreviatinsn for distribution names are: "norm", "unif", "t", "snorm", "weibull", "gamma", "lnorm", "exp"
#'
#' @param .data The data.
#' @param distname The distribution name.
#'
#' @export
fit_distribution <- function(.data, distname = c("norm", "unif", "t", "snorm", "weibull", "gamma", "lnorm", "exp")) {
  if (anyNA(.data)) {
    print("Data has missing values. These values will be omitted.")
  }
  .data <- as.numeric(stats::na.omit(.data))

  positive_only_distributions <- c("weibull", "gamma", "lnorm", "exp")
  if (min(.data) < 0 & distname %in% positive_only_distributions) {
    stop("This distribution cannot be fitted to data with negative values.")
  }

  if (distname == "norm") {
    dist <- fitdistrplus::fitdist(.data, "norm")
  } else if (distname == "unif") {
    dist <- fitdist_parameters_uniform(.data)
  } else if (distname == "t") {
    dist <- suppressWarnings(fitdist_parameters_t(.data))
  } else if (distname == "snorm") {
    dist <- fitdist_parameters_skew_normal(.data)
  } else if (distname == "weibull") {
    dist <- fitdistrplus::fitdist(.data, "weibull")
  } else if (distname == "gamma") {
    dist <- fitdist_parameters_gamma(.data)
  } else if (distname == "lnorm") {
    dist <- fitdistrplus::fitdist(.data, "lnorm")
  } else if (distname == "exp") {
    dist <- fitdist_parameters_exponential(.data)
  } else {
    stop("Incorrect distribution name.")
  }

  print_distribution_parameters(dist)
  print_distribution_instructions(dist)
  make_histogram(.data, dist)
}


# Print Functions ---------------------------------------------------------

#' Print Distribution Parameters
#'
#' Prints the name of the recommended distribution and the distribution parameters.
#'
#' @param dist List of parameters from the fitted distribution.
print_distribution_parameters <- function(dist) {
  cat("Estimated parameters for the",dist$distname,":\n")
  print(dist$estimate)
}

#' Print Distribution Instructions
#'
#' Prints R instructions for sampling the recommended distribution.
#'
#' @param dist List of parameters from the fitted distribution.
print_distribution_instructions <- function(dist) {
  dist_command <- list(
    weibull = "rweibull(n,shape,scale)",
    gamma = "rgamma(n,shape,rate)",
    lnorm = "rlnorm(n,meanlog,sdlog)",
    exp = "rexp(n,rate)",
    norm = "rnorm(n,mean,sd)",
    unif = "runif(n,min,max)",
    t = "m+s*rt(n, df)",
    snorm = "rsn(n,xi,omg,alpha)"
  )

  if (dist$distname %in% names(dist_command)) {
    cat("Format of the command to sample from the", dist$distname, "distribution:\n", dist_command[[dist$distname]], "\n")
  } else {
    print("Command not found.")
  }
}

# Histogram Function -----------------------------------------------------

#' Make Histogram
#'
#' Outputs a histogram of the data overlayed by the density function of the recommended distribution.
#'
#' @param .data The data.
#' @param dist List of distribution parameters.
make_histogram <- function(.data, dist) {
  densityfn <- list(
    weibull = function(x) {
      stats::dweibull(x, dist$estimate["shape"], dist$estimate["scale"])
    },
    gamma = function(x) {
      my_shape <- as.numeric(dist$estimate[1])
      my_scale <- as.numeric(dist$estimate[2])
      stats::dgamma(x, my_shape, my_scale)
    },
    lnorm = function(x) {
      stats::dlnorm(x, dist$estimate["meanlog"], dist$estimate["sdlog"], log = FALSE)
    },
    exp = function(x) {
      stats::dexp(x, dist$estimate["rate"])
    },
    norm = function(x) {
      stats::dnorm(x, dist$estimate["mean"], dist$estimate["sd"])
    },
    unif = function(x) {
      stats::dunif(x, dist$estimate["min"], dist$estimate["max"])
    },
    t = function(x) {
      my_m <- dist$estimate[1]
      my_s <- dist$estimate[2]
      my_df <- dist$estimate[3]
      stats::dt(((x - my_m) / my_s), my_df, log = FALSE) / my_s
    },
    snorm = function(x) {
      xi <- dist$par[1]
      omg <- dist$par[2]
      alpha <- dist$par[3]
      sn::dsn(x, xi, omg, alpha)
    }
  )

  graphics::hist(.data, breaks = 20, freq = FALSE, main = dist$distname)

  if (dist$distname %in% names(densityfn)) {
    graphics::curve(sapply(x, densityfn[[dist$distname]]), from = min(.data), to = max(.data), add = TRUE, col = "red")
  } else {
    print("Density function not found.")
  }
}


# Fit Distribution Parameters -------------------------------------------------------

#' Fit Distribution Parameters Exponential
#'
#' Fits the exponentional distribution to the given data. Adjusts the scaling for estimation and returns the estimate parameters at the original scaling.
#'
#' @param my_data The data.
#'
#' @return List of the fitted distribution parameters.
fitdist_parameters_exponential <- function(my_data) {
  rate_scale <- 1
  if (max(my_data) > 100) {
    rate_scale <- max(.1 * my_data)
  }
  my_scaled_data <- my_data / rate_scale
  fitted_e <- fitdistrplus::fitdist(my_scaled_data, "exp")
  fitted_e$estimate["rate"] <- fitted_e$estimate["rate"] / rate_scale
  fitted_e$loglik <- sum(log(stats::dexp(my_data, rate = fitted_e$estimate["rate"], log = FALSE)))
  num_parameters <- 1
  fitted_e$aic <- 2 * num_parameters - 2 * fitted_e$loglik
  return(fitted_e)
}


#' Fit Distribution Parameters Gamma
#'
#' Fits the uniform distribution to the given data. Adjusts the scaling for estimation and returns the estimate parameters at the original scaling.
#'
#' @param my_data The data.
#'
#' @return List of the fitted distribution parameters.
fitdist_parameters_gamma <- function(my_data) {
  rate_scale <- 1
  if (max(my_data) > 100) {
    rate_scale <- max(.1 * my_data)
  }
  my_scaled_data <- my_data / rate_scale
  fitted_g <- fitdistrplus::fitdist(my_scaled_data, "gamma")
  fitted_g$estimate["rate"] <- fitted_g$estimate["rate"] / rate_scale
  fitted_g$loglik <- sum(log(stats::dgamma(my_data, shape = fitted_g$estimate["shape"], rate = fitted_g$estimate["rate"], log = FALSE)))
  fitted_g$aic <- 2 * 2 - 2 * fitted_g$loglik
  return(fitted_g)
}

#' Fit Distribution Parameters Uniform
#'
#' Fits the uniform distribution to the given data.
#'
#' @param .data The data.
#'
#' @return List of parameters from the uniform distribution.
fitdist_parameters_uniform <- function(.data) {
  fit_u <- fitdistrplus::fitdist(.data, "unif")
  # calculate loglik
  params_u <- data.frame(fit_u[1])
  umin <- params_u$estimate[1]
  umax <- params_u$estimate[2]
  loglik_u <- -NROW(.data) * log(umax - umin)
  fit_u["loglik"] <- loglik_u
  num_parameters <- 2
  fit_u["aic"] <- 2 * num_parameters - 2 * fit_u$loglik
  fit_u
}

#' Fit Distribution Parameters t
#'
#' Fits the t distribution to the given data.
#'
#' @param my_data The data.
#'
#' @return List of the distribution parameters.
fitdist_parameters_t <- function(my_data) {
  # find t starting values
  mean_mydata <- mean(my_data)
  sd_mydata <- stats::sd(my_data)
  df_values <- c(3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25, 30, 35, 40, 45, 50, 55)
  logl_save <- -1e9
  df_save <- 0
  s_save <- 0
  for (val in df_values) {
    m <- mean_mydata
    s <- sd_mydata * ((val - 2) / val)^.5
    tval <- stats::dt(((my_data - m) / s), val)
    logl_t <- sum(log(tval / s))
    if (logl_t > logl_save) {
      df_save <- val
      s_save <- s
      logl_save <- logl_t
    }
  }

  # Starting values for t-distribution fit
  m_start <- mean_mydata
  s_start <- s_save
  df_start <- df_save
  # print(c(m_start, s_start, df_start))
  # Fit the t-distribution using starting values
  if (df_start >= 45) {
    # for large df, use normal distribution
    fitted_t <- fitdistrplus::fitdist(my_data, "norm")
    fitted_t$estimate["df"] <- 45
  } else {
    fitted_t <- MASS::fitdistr(my_data, "t", start = list(m = m_start, s = s_start, df = df_start))
  }
  # distname needs to be declared because it is not named by fitdstr
  # also renames the distname t, even if it might be norm
  fitted_t$distname <- "t"
  num_parameters <- 3
  fitted_t["aic"] <- 2 * num_parameters - 2 * fitted_t$loglik
  return(fitted_t)
}

#' Fit Distribution Parameters Skew Normal
#'
#' Fits the skew normal distribution to the data.
#'
#' @param my_data The data.
#'
#' @return List of distribution parameters.
fitdist_parameters_skew_normal <- function(my_data) {
  # Initialize variables used in the function
  alpha_exog <- 0
  logl_save <- -1e9

  # The function first searches for starting variables by varying the skew parameter
  # in increments of 1. ISkSt=1 tells the function to do a search for starting values
  ISkSt <- 1

  # Define likelihood function. Optimizer auglag minimizes, so this function
  # returns the negative of the logarithm of the likelihood function.
  SkewNormal <- function(x) {
    x[2] = abs(x[2])
    if (ISkSt == 1) {
      x[3] <- alpha_exog
    }
    my_term1 <- stats::dnorm((my_data - x[1]) / x[2])
    my_term2 <- stats::pnorm(x[3] * (my_data - x[1]) / x[2])
    my_log_pdf <- log((2 / x[2]) * my_term1 * my_term2)
    my_LogL <- sum(my_log_pdf)
    return(-my_LogL)
  }

  # Preparing to use auglag in package alabama
  # Setting limits for parameters for auglag
  limits <- function(x) {
    c(
      x[1] - min(my_data), # x[1] >= min of data
      max(my_data) - x[1], # x[1] <= max of data
      x[2] - .1 * stats::sd(my_data), #  x[2]>.1 std devs of data
      3 * stats::sd(my_data) - x[2], # x[2]<3 std of data
      x[3] + 20, # x[3]>=-20 # skew parameter >-20
      20 - x[3] # skew parameter < 20
    )
  }


  # Search for best starting values for skewed normal distribution
  # The skewness is fixed at one of the skew_values and the other two parameters are
  # chosen to maximize the likelihood function.
  # Starting values are the highest likelihood function obtained in the grid search
  # over skew_values
  skew_values <- c(0, seq(1:19))

  # If val=0, the skew normal is the same as the normal. Hence, a natural starting point.
  # alpha_exog is a value of the skew parameter in the grid search.
  # opt_snorm is the object produced when auglag finds an optimum
  for (val in skew_values) {
    if (val == 0) {
      xi <- mean(my_data)
      omg <- stats::sd(my_data)
      p0 <- c(xi, omg, 0)
      opt_snorm <- alabama::auglag(
        par = p0, fn = SkewNormal, hin = limits,
        control.outer = list(trace = FALSE)
      )
    }
    alpha_exog <- ifelse((stats::median(my_data) < mean(my_data)), val, -val)
    if (abs(val) > 0) {
      xi <- as.numeric(opt_snorm$par[1])
      omg <- as.numeric(opt_snorm$par[2])
      p0 <- c(xi, omg, alpha_exog)
      opt_snorm <- alabama::auglag(
        par = p0, fn = SkewNormal, hin = limits,
        control.outer = list(trace = FALSE)
      )
    }
    logl_skn <- -as.numeric(opt_snorm["value"])
    if (logl_save > logl_skn & val > 4) {
      break
    }
    xi <- ifelse(logl_skn > logl_save, p0[1], xi)
    omg <- ifelse(logl_skn > logl_save, p0[2], omg)
    alpha <- ifelse(logl_skn > logl_save, p0[3], alpha)
    best_starting_values <- c(xi, omg, alpha)
    logl_save <- ifelse(logl_skn > logl_save, logl_skn, logl_save)
  }

  # Grid search is completed.
  # Now optimize over all three parameters using best_starting_values
  # Following indicator variable specifies search over all three parameters
  ISkSt <- 0

  opt_snorm <- alabama::auglag(
    par = best_starting_values, fn = SkewNormal, hin = limits,
    control.outer = list(trace = FALSE)
  )
  # Function returns a 4-element vector.
  # The first element is the value of the likelihood function with correct sign
  # The remaining three elements are the estimated parameters of the skew normal.
  # return(c(-opt_snorm$value,opt_snorm$par))
  opt_snorm["distname"] <- "snorm"
  opt_snorm["loglik"] <- -opt_snorm$value
  num_parameters <- 3
  opt_snorm["aic"] <- 2 * num_parameters - 2 * opt_snorm$loglik
  opt_snorm$estimate <- c(xi = opt_snorm$par[1], omg = opt_snorm$par[2], alpha = opt_snorm$par[3])
  return(opt_snorm)
}

# Test -----------------------------------------------------------------


#' Test Best-fitting Distribution
#'
#' Tests fit distribution against a variety of generated data.
test_select_distribution <- function() {
  set.seed(33)
  cat("\nTesting Gamma\n")
  select_distribution(stats::rgamma(1000, 6, 2))
  cat("\nTesting Weibull\n")
  select_distribution(stats::rweibull(1000, 6, 2))
  cat("\nTesting lnorm\n")
  select_distribution(stats::rlnorm(1000, 2, .5))
  cat("\nTesting exp\n")
  select_distribution(stats::rexp(1000, .3))
  cat("\nTesting norm\n")
  select_distribution(stats::rnorm(1000, 2, 3))
  cat("\nTesting unif\n")
  select_distribution(stats::runif(1000, 2, 4))
  cat("\nTesting t\n")
  t_data <- 2 + 1.2 * stats::rt(1000, 6)
  select_distribution(t_data)
  cat("\nTesting snorm\n")
  snorm_data <- as.numeric(sn::rsn(1000, 32, 20, -5))
  select_distribution(snorm_data)
}
