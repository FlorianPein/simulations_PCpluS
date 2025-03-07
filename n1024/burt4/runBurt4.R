
mainPath <- "~/Desktop/simulationPCpluS/"
source(paste0(mainPath, "simul.R"))
source(paste0(mainPath, "calls.R"))

a <- 4
name <- paste0("burt", a)
path <- paste0(mainPath, "n1024/", name, "/")
seed <- 3e5 + 1e4
M <- 1e4

n <- 256
X <- matrix(0, n, n)
X <- lower.tri(X, diag = TRUE)
X[X] <- 1

signalCP <- as.numeric(X %*% c(rep(0, n / 2), 20, rep(0, n / 2 - 1))) - 20
signalCP <- rep(signalCP, each = 4)
n <- 1024
signalSmooth <- 20 * 1:n / n * cos(16 * (1:n / n)^1.2)
signal <- signalCP + signalSmooth
std <- sd(signal) / a

truth <- list(n = n, std = std, signalCP = signalCP, signalSmooth = signalSmooth, signal = signal)
saveRDS(truth, paste0(path, "truth.RDS"))

simul(M = M, call = callMethod, name = "method",
      cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
simul(M = M, call = callJIC, name = "JIC",
      cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
simul(M = M, call = callPelt, name = "PELT",
      cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
simul(M = M, call = callCOPS, name = "COPS",
      cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
simul(M = M, call = callSOPS, name = "SOPS",
      cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
# simul(M = M, call = callSCHACEcv, name = "SCHACEcv",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
# simul(M = M, call = callSCHACEbic, name = "SCHACEbic",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)

# simul(M = M, call = callFusedLasso, name = "FusedLasso",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
# 
# simul(M = M, call = callKernelSmoothingVfold, name = "kernelSmoothingVfold",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
# simul(M = M, call = callKernelSmoothingLOOCV, name = "kernelSmoothingLOOCV",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)

source(paste0(mainPath, "analysis.R"))

methods <- c("method", "JIC", "PELT", "COPS", "SOPS")
# methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
ret <- summarise(path = path, methods = methods)
saveRDS(ret, "summary.RDS")

ret <- readRDS(paste0(path, "/summary.RDS"))

createTables(ret = ret, path = path, name = name, methods = methods)
                   