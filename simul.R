simul <- function(M, call, name, cpFun, smoothFun, std, seed, path, verbose, ...) {
  setwd(path)

  results <- vector("list", M)
  
  for (i in 1:M) {
    if (is.numeric(verbose) && i %% verbose == 0) {
      print(paste0(name, ": ", i))
    }
    
    set.seed(seed + i)
    
    y <- rnorm(length(signal), mean = cpFun + smoothFun, sd = std)
    
    results[[i]] <- call(y, ...)
  }
  
  saveRDS(results, paste0(name, ".RDS"))
}
