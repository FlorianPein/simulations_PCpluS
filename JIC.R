JIC <- function(y, h = 0.3 / length(y)^(1 / 5), kernel = function(x) ifelse(x >= 0 & x <= 0.5, 0.75 * (1 - x^2), 0),
                Pn = sqrt(n * h * log(n)), gamma = 1, epsilon = 0.51, maxJumps = as.integer(length(y) / 10)) {
  n <- length(y)
  hn <- as.integer(n * h + 1e-12)
  
  diffs <- ((0:hn + 0.5) / n / h)[(0:hn + 0.5) / n / h <= 0.5]
  K <- kernel(diffs)
  w0 <- sum(K)
  w1 <- sum(K * diffs)
  w2 <- sum(K * diffs^2)
  K <- K * (w2 - w1 * diffs) / (w0 * w2 - w1^2)
  
  hn <- length(K)
  
  locations <- (hn + 1L):(n - hn + 1L)
  nlocations <- n - 2L * hn + 1L
  
  M <- numeric(nlocations)
  for (j in 1:nlocations) {
    M[j] <- sum(y[j + 1:hn - 1 + hn] * K) - sum(y[j + 1:hn - 1] * rev(K))
  }
  
  # optimal i == 0 etc.
  kernelSmooth <- function(x) kernel(abs(x))
  attr(kernelSmooth, "RK") <- 0.75 * sqrt(2 * (0.5 - 2 / 3 * 0.5^3 + 1/5 * 0.5^5))
  cpfun <- numeric(n)
  smooth <- locpol::locLinSmootherC(x = 1:n / n, y = y - cpfun, xeval = 1:n / n, bw = h,
                                    kernel = kernelSmooth)$beta0
  est <- smooth
  jic <- n * log(mean((y - est)^2))
  optimum <- list(est = est, smooth = smooth, cpfun = cpfun, cps = integer(0), JIC = jic)
  
  jumps <- integer(maxJumps)
  sizes <- numeric(maxJumps)
  

  
  for (i in 1:maxJumps) {
    index <- which.max(abs(M))
    jumps[i] <- locations[index]
    sizes[i] <- M[index]
    
    remove <- which(jumps[i] / n - epsilon * h <= locations / n & locations / n <= jumps[i] / n + epsilon * h)
    locations <- locations[-remove]
    M <- M[-remove]
    
    cpfun[jumps[i]:n] <- cpfun[jumps[i]:n] + sizes[i]
    smooth <- locpol::locLinSmootherC(x = 1:n / n, y = y - cpfun, xeval = 1:n / n, bw = h,
                                      kernel = kernelSmooth)$beta0
    est <- cpfun + smooth
    jic <- n * log(mean((y - est)^2)) + Pn * sum(1 / abs(sizes[1:i])^gamma)
    
    if (jic < optimum$JIC) {
      optimum <- list(est = est, smooth = smooth, cpfun = cpfun, cps = sort(jumps[1:i]), JIC = jic)
    }
    
    if (length(locations) == 0) {
      break
    }
  }
  optimum
}
