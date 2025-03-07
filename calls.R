
library(PCpluS)
library(changepoint)
source(paste0(mainPath, "JIC.R"))
library(jra)
library(SCHACE)

callMethod <- function(y, ...) {
  CV <- cv.pcplus(y)
  est <- pcplus(y, lambda = CV$lambda, bandwidth = CV$bandwidth)

  list(est = est$est, cps = est$cps, size = est$cpfun[est$cps] - est$cpfun[est$cps - 1],
       bandwidth = CV$bandwidth, lambda = CV$lambda[length(CV$lambda)])
}

callJIC <- function(y, ...) {
  h <- 0.3 / length(y)^(1 / 5)
  ret <- JIC(y, h = h)
  
  list(est = ret$est, cps = ret$cps, size = ret$cpfun[ret$cps] - ret$cpfun[ret$cps - 1],
       bandwidth = h, lambda = as.numeric(NA))
}

callPelt <- function(y, ...) {
  # little hack since PELT does not scale
  std <- as.numeric(diff(quantile(diff(y), c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75))) / sqrt(2))
  
  pelt <- changepoint::cpt.mean(data = y / std, penalty = "SIC", method = "PELT")
  
  left <- c(1, pelt@cpts[-length(pelt@cpts)] + 1)
  right <- c(left[-1] - 1, length(y))
  values <- pelt@param.est$mean
  est <- numeric(length(y))
  for (segment in seq_along(left)) {
    est[left[segment]:right[segment]] <- values[segment]
  }
  est <- est * std
  
  list(est = est, cps = pelt@cpts[-length(pelt@cpts)] + 1, size = diff(values),
       bandwidth = Inf, lambda = as.numeric(NA))
}

callCOPS <- function(y, ...) {
  n <- length(y)
  ret <- jra::COPS(X = 1:n / n, Y = y)
  
  left <- c(1, as.integer(ret$locations * n + 1e-12))
  right <- c(left[-1] - 1, length(y))
  values <- c(0, as.numeric(ret$sizes))
  
  est <- numeric(length(y))
  for (segment in seq_along(left)) {
    est[left[segment]:right[segment]] <- values[segment]
  }
  
  smooth <- .kernelSmoothingEpanechnikov(y - est, ret$h_opt)
  
  list(est = est + smooth, cps = left[-1], size = values,
       bandwidth = ret$h_opt, lambda = as.numeric(NA))
}

callSOPS <- function(y, ...) {
  n <- length(y)
  ret <- jra::SOPS(X = 1:n / n, Y = y)
  
  left <- c(1, as.integer(ret$locations * n + 1e-12))
  right <- c(left[-1] - 1, length(y))
  values <- c(0, as.numeric(ret$sizes))
  
  est <- numeric(length(y))
  for (segment in seq_along(left)) {
    est[left[segment]:right[segment]] <- values[segment]
  }
  
  smooth <- .kernelSmoothingEpanechnikov(y - est, ret$h_opt)
  
  list(est = est + smooth, cps = left[-1], size = values,
       bandwidth = ret$h_opt, lambda = as.numeric(NA))
}

callSCHACEcv <- function(y, ...) {
  est <- main.SCHACE(
    y,
    folds = 3,
    Bdf_set = seq(3, 15, 1),
    tuning = "crossvalidation",
    methods = "Trim",
    clambda = "lambdamin",
    percent = 0.1
  )
  
  list(est = as.numeric(est$`predicted y`), cps = est$`locations of detected CPs`, size = est$`jump size`)
}

callSCHACEbic <- function(y, ...) {
  est <- main.SCHACE(
    y,
    tuning = "IC",
    IC.method = "BIC.Chen"
  )
  
  list(est = as.numeric(est$`predicted y`), cps = est$`locations of detected CPs`, size = est$`jump size`)
}




callFusedLasso <- function(y, ...) {
  tuning <- gridSearchCV(y = y, post = FALSE, bandwidths = Inf)
  est <- estimateKernel(y = y, lambda = tuning$lambda, bandwidth = tuning$bandwidth, post = FALSE)
  
  list(est = est$est, cps = est$cps, size = est$cpfun[est$cps] - est$cpfun[est$cps - 1],
       bandwidth = Inf, lambda = tuning$lambda)
}

callKernelSmoothingVfold <- function(y, ...) {
  bandwidth <- gridSearchSmoothKernel(y = y)$bandwidth
  est <- kernelSmoothing(y = y, bandwidth = bandwidth)
  
  list(est = est, cps = integer(0), size = numeric(0),
       bandwidth = bandwidth, lambda = as.numeric(NA))
}

callKernelSmoothingLOOCV <- function(y, ...) {
  bandwidth <- CVkernelSmoothing(y = y)
  est <- kernelSmoothing(y = y, bandwidth = bandwidth)
  
  list(est = est, cps = integer(0), size = numeric(0),
       bandwidth = bandwidth, lambda = as.numeric(NA))
}
