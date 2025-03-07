
library(PCpluS)
library(PCpluStest)

M <- 100
nfactor <- as.integer(seq(1, 2^6, length.out = 12))

timeGlmnetCV <- matrix(0, M, length(nfactor))
timeOurCV <- matrix(0, M, length(nfactor))
timeGlmnetEst <- matrix(0, M, length(nfactor))
timeOurEst <- matrix(0, M, length(nfactor))

for (j in seq_along(nfactor)) {
  print(j)
  
  a <- 4
  n <- 256 * nfactor[j]
  
  signalCP <- c(rep(-4, 58), rep(4, 18), rep(8, 64), rep(8 + 3.5, 57), rep(2 + 3.5, 59))
  signalCP <- rep(signalCP, each = as.integer(n / 256 + 1e-12))
  
  signalSmooth <- cos(5.5 * pi * 1:n / n)
  signal <- signalCP + signalSmooth
  std <- sd(signal) / a
  
  y <- rnorm(length(signal), mean = signal, sd = std)
  
  print(as.numeric(system.time(CV <- cv.pcplus(y)))[3])
  
  for (i in 1:M) {
    print(c(i, j))
    a <- 4
    n <- 256 * nfactor[j]
    
    signalCP <- c(rep(-4, 58), rep(4, 18), rep(8, 64), rep(8 + 3.5, 57), rep(2 + 3.5, 59))
    signalCP <- rep(signalCP, each = as.integer(n / 256 + 1e-12))
    
    signalSmooth <- cos(5.5 * pi * 1:n / n)
    signal <- signalCP + signalSmooth
    std <- sd(signal) / a
    
    y <- rnorm(length(signal), mean = signal, sd = std)
    
    timeOurEst[i, j] <- as.numeric(system.time(est <- pcplus(y, lambda = CV$lambda, bandwidth = CV$bandwidth)))[3]
    timeGlmnetEst[i, j] <- as.numeric(system.time(est <- estimateKernel(y, lambda = CV$lambda[length(CV$lambda)], bandwidth = CV$bandwidth)))[3]  
  }
}

setwd("~/Desktop/Florian-Rajen/PCpluS")
saveRDS(list(timeOurEst, timeGlmnetEst), file = "timeEst.RDS")

ret <- readRDS("timeEst.RDS")
timeOurEst <- colMeans(ret[[1]])
timeGlmnetEst <- colMeans(ret[[2]])
timeGlmnetEst[1] <- mean(ret[[2]][2:10, 1])


pdf("TimeEst.pdf", height = 3, width = 6)
par(mar = c(4.5, 4.5, 0.5, 0.5))
plot(256 * nfactor, timeGlmnetEst, col = "blue", pch = 16, xlab = "Number of observations", ylab = "Time in s")
points(256 * nfactor, timeOurEst, col = "red", pch = 16)
legend("topleft", legend = c("glmnet", "PCpluS"), col = c("blue", "red"), pch = 16)
dev.off()

