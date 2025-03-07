
mainPath <- "~/Desktop/simulationPCpluS/"
source(paste0(mainPath, "simul.R"))
source(paste0(mainPath, "calls.R"))

a <- 2
name <- paste0("cosine", a)
path <- paste0(mainPath, "n256/", name, "/")
seed <- 1e5 + 10e4
M <- 100

n <- 256
signalCP <- c(rep(-4, 58), rep(4, 18), rep(8, 64), rep(8 + 3.5, 57), rep(2 + 3.5, 59))
signalSmooth <- cos(5.5 * pi * 1:n / n)
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
simul(M = M, call = callSCHACEcv, name = "SCHACEcv",
      cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
simul(M = M, call = callSCHACEbic, name = "SCHACEbic",
      cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)

# simul(M = M, call = callFusedLasso, name = "FusedLasso",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
# 
# simul(M = M, call = callKernelSmoothingVfold, name = "kernelSmoothingVfold",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)
# simul(M = M, call = callKernelSmoothingLOOCV, name = "kernelSmoothingLOOCV",
#       cpFun = signalCP, smoothFun = signalSmooth, std = std, seed = seed, path = path, verbose = 1)

source(paste0(mainPath, "analysis.R"))

methods <- c("method", "JIC", "PELT", "COPS", "SOPS", "SCHACEcv", "SCHACEbic")
# methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
ret <- summarise(path = path, methods = methods)
saveRDS(ret, "summary.RDS")

ret <- readRDS(paste0(path, "/summary.RDS"))

createTables(ret = ret, path = path, name = name, methods = methods)
