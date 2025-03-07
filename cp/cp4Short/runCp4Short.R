
mainPath <- "~/Desktop/simulationPCpluS/"
source(paste0(mainPath, "simul.R"))
source(paste0(mainPath, "calls.R"))

name <- "cp4Short"
path <- paste0(mainPath, "cp/", name, "/")
seed <- 5e5 + 6e4
M <- 1e4

n <- 497
signalCP <- c(rep(-0.18, 137), rep(0.08, 87), rep(1.07, 17), rep(-0.53, 57),
              rep(0.16, 9), rep(-0.69, 24), rep(-0.16, 166))
signalSmooth <- 0.25 * 0.2 * 4 * sin(0.025 * pi * 1:n)
signal <- signalCP + signalSmooth
std <- 0.2

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
