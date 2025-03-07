summarise <- function(path, methods) {
  setwd(path)
  
  truth <- readRDS("truth.RDS")
  
  cpFun <- truth$signalCP
  smoothFun <- truth$signalSmooth
  signal <- truth$signal
  n <- truth$n
  jumps <- which(diff(cpFun) != 0) + 1
  njump <- length(jumps)
  tolJumps <- min(3, as.integer(min(diff(c(1, jumps, n + 1))) / 2))
  sizes <- cpFun[jumps] - cpFun[jumps - 1]
  
  summaryList <- vector("list", length(methods))
  names(summaryList) <- methods
  
  for (j in seq_along(methods)) {
    name <- methods[j]
    s <- readRDS(paste0(name, ".RDS"))
    M <- length(s)
    
    njumps <- numeric(M)
    mse <- numeric(M)
    mad <- numeric(M)
    errorLocation <- matrix(0, M, length(jumps))
    errorSize <- matrix(0, M, length(jumps))
    
    # lambdas <- numeric(M)
    # bandwidths <- numeric(M)
    
    for (i in 1:M) {
      njumps[i] <- length(s[[i]]$cps)
      mse[i] <- mean((s[[i]]$est - signal)^2)
      mad[i] <- mean(abs(s[[i]]$est - signal))
      
      for (l in seq_along(jumps)) {
        diffJumps <- jumps[l] - s[[i]]$cps
        index <- which.min(abs(diffJumps))
        
        if (length(index) == 1 && abs(diffJumps[index]) <= tolJumps) {
          errorLocation[i, l] <- diffJumps[index]
          # TODO: introduce estCpfun
          if (sizes[l] > 0) {
            errorSize[i, l] <- (s[[i]]$est[s[[i]]$cps[index]] - s[[i]]$est[s[[i]]$cps[index] - 1]) - sizes[l]
          } else {
            errorSize[i, l] <- sizes[l] - (s[[i]]$est[s[[i]]$cps[index]] - s[[i]]$est[s[[i]]$cps[index] - 1])
          }
        } else {
          errorLocation[i, l] <- NA
          errorSize[i, l] <- NA
        }
      }
      
      # lambdas[i] <- s[[i]]$lambda
      # bandwidths[i] <- s[[i]]$bandwidth
    }
    # lambdas[lambdas == 10^12] <- NA
    
    detected <- numeric(length(jumps))
    errorLocationBias <- numeric(length(jumps))
    errorLocationSd <- numeric(length(jumps))
    errorSizeBias <- numeric(length(jumps))
    errorSizeSd <- numeric(length(jumps))
    
    for (l in seq_along(jumps)) {
      detected[l] <- mean(is.finite(errorLocation[, l]))
      errorLocationBias[l] <- mean(errorLocation[, l], na.rm = TRUE)
      errorLocationSd[l] <- sd(errorLocation[, l], na.rm = TRUE)
      errorSizeBias[l] <- mean(errorSize[, l], na.rm = TRUE)
      errorSizeSd[l] <- sd(errorSize[, l], na.rm = TRUE)
    }
    
    summaryList[[j]] <- list(repetitions = M,
                             njumps = njumps,
                             njumpsMean = mean(njumps) - njump,
                             njumpsMAD = mean(abs(njumps - njump)),
                             njumpsMSE = mean((njumps - njump)^2),
                             njumpsQuantities = c(sum(njumps - njump < -2), sum(njumps - njump == -2),
                                                  sum(njumps - njump == -1), sum(njumps - njump == 0),
                                                  sum(njumps - njump == 1), sum(njumps - njump == 2),
                                                  sum(njumps - njump > 2)) / M,
                             mses = mse,
                             amse = mean(mse),
                             mads = mad,
                             amad = mean(mad),
                             detected = detected * 100,
                             errorLocationBias = errorLocationBias,
                             errorLocationSd = errorLocationSd,
                             errorLocationMSE = errorLocationBias^2 + errorLocationSd^2,
                             errorSizeBias = errorSizeBias,
                             errorSizeSd = errorSizeSd,
                             errorSizeMSE = errorSizeBias^2 + errorSizeSd^2
                             
                             # lambda = list(values = lambdas, mean = mean(lambdas, na.rm = TRUE),
                             #               quantiles = quantile(lambdas[bandwidths < Inf], na.rm = TRUE)),
                             # bandwidth = list(values = bandwidths, freqInf = mean(bandwidths == Inf),
                             #                  mean = mean(bandwidths[bandwidths < Inf]),
                             #                  quantiles = quantile(bandwidths[bandwidths < Inf], na.rm = TRUE))
    )
  }
  summaryList
}

library(xtable)
createTables <- function(ret, path, name, methods) {
  # tabS <- matrix(0, length(methods), 7)
  # for (i in seq_along(methods)) {
  #   tabS[i, ] <- c(ret[[methods[i]]]$bandwidth$mean, as.numeric(ret[[methods[i]]]$bandwidth$quantiles),
  #                  ret[[methods[i]]]$bandwidth$freqInf)
  # }
  # tabS <- cbind(methods, as.data.frame(tabS))
  # colnames(tabS) <- c("method", "mean", "min", "25%", "50%", "75%", "max", "#Inf")
  # dig <- rep(4, 9)
  # dig[9] <- 2
  # tabS <- xtable(tabS, digits = dig, auto = TRUE, display = c("g","s","g","g","g", "g","g","g", "g"),
  #                align = c("l", "l|", "c|", "c", "c", "c", "c", "c|", "c"),
  #                caption = paste0(name, ": bandwidths"), label = paste0("tab:", name, "Bandwidths"))
  # print(tabS, include.rownames = FALSE, file = paste0(path, name, ".tex"), append = FALSE)
  
  # tabC <- matrix(0, length(methods), 6)
  # for (i in seq_along(methods)) {
  #   tabC[i, ] <- c(ret[[methods[i]]]$lambda$mean, as.numeric(ret[[methods[i]]]$lambda$quantiles))
  # }
  # tabC <- cbind(methods, as.data.frame(tabC))
  # colnames(tabC) <- c("method", "mean", "min", "25%", "50%", "75%", "max")
  # dig <- rep(4, 8)
  # tabC <- xtable(tabC, digits = dig, auto = TRUE, display = c("g","s","g","g","g", "g","g","g"),
  #                align = c("l", "l|", "c|", "c", "c", "c", "c", "c"),
  #                caption = paste0(name, ": fused Lasso penalty $\\lambda$"), label = paste0("tab:", name, "Lambdas"))
  # print(tabC, include.rownames = FALSE, file = paste0(path, name, ".tex"), append = TRUE)
  
  
  tabN <- matrix(0, length(methods), 9)
  for (i in seq_along(methods)) {
    tabN[i, ] <- c(ret[[methods[i]]]$njumpsMean, ret[[methods[i]]]$njumpsQuantities, ret[[methods[i]]]$amse)
  }
  tabN <- cbind(methods, as.data.frame(tabN))
  colnames(tabN) <- c("method", "bias", "< -2", "-2", "-1", "0", "1", "2", "> 2", "aMSE")
  dig <- rep(4, 11)
  tabN <- xtable(tabN, digits = dig, auto = TRUE, display = c("g","s","g","g","g","g","g", "g","g","g", "g"),
                 align = c("l", "l|", "c|", "c", "c", "c", "c", "c", "c", "c|", "c"),
                 caption = paste0(name, ": number of detected change-points and averaged MSE"),
                 label = paste0("tab:", name, "Njumps"))
  print(tabN, include.rownames = FALSE, file = paste0(path, name, ".tex"), append = TRUE)
  
  
  tabD <- matrix(0, length(methods), length(ret[[methods[1]]]$detected))
  for (i in seq_along(methods)) {
    tabD[i, ] <- ret[[methods[i]]]$detected
  }
  tabD <- cbind(methods, as.data.frame(tabD))
  colnames(tabD) <- c("method", seq_along(ret[[methods[1]]]$detected))
  dig <- rep(5, length(ret[[methods[1]]]$detected) + 2)
  tabD <- xtable(tabD, digits = dig, auto = TRUE, display = c("g","s", rep("g", length(ret[[methods[1]]]$detected))),
                 align = c("l", "l|", rep("c", length(ret[[methods[1]]]$detected))),
                 caption = paste0(name, ": percentage how often each change-point was detected"),
                 label = paste0("tab:", name, "Detections"))
  print(tabD, include.rownames = FALSE, file = paste0(path, name, ".tex"), append = TRUE)
}
