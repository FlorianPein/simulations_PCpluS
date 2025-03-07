
library(xtable)
mainPath <- "~/Desktop/simulationPCpluS/"

path <- paste0(mainPath, "cp/")

setwd(path)

set.seed(5e5 + 1e4 + 1)

n <- 497
signalCP <- c(rep(-0.18, 137), rep(0.08, 87), rep(1.07, 17), rep(-0.53, 57),
              rep(0.16, 9), rep(-0.69, 24), rep(-0.16, 166))
signalSmooth <- 0.25 * 0.2 * sin(0.01 * pi * 1:n)
signal <- signalCP + signalSmooth
std <- 0.2

data <- rnorm(n, signal, std)

png("ExampleCpLong.png", width = 660, height = 330)
par(mar = c(2.5, 2.5, 0.5, 0.5))
plot(data, col = "grey30", pch = 16, xlab = "", ylab = "", cex.axis = 1.4)
lines(signal, lwd = 3)
dev.off()

settings <- c("cp", "cpLong", "cpShort", "cp2Long", "cp2Short", "cp4Long", "cp4Short")
methods <- c("method", "JIC", "PELT", "COPS", "SOPS")
# methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
                  "$\\COPS$", "$\\SOPS$")
# abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
#                   "$\\FL$", "$\\kernSmoothVfold$", "$\\kernSmoothLOOCV$")
  
tab <- matrix(0, length(methods), length(settings))
for (i in seq_along(settings)) {
  for (j in seq_along(methods)) {
    name <- paste0(settings[i])
    ret <- readRDS(paste0(path, name, "/summary.RDS"))
    
    tab[j, i] <- ret[[methods[j]]]$amse
  }
}
tab <- cbind(abbreviation, as.data.frame(tab))
colnames(tab) <- c("Method", "$a = 0, b = 0$", "$a = 0.01, b = 0.2$", "$a = 0.025, b = 0.2$",
                   "$a = 0.01, b = 0.4$", "$a = 0.025, b = 0.4$", "$a = 0.01, b = 0.8$", "$a = 0.025, b = 0.8$")
tab <- xtable(tab, digits = 4, display = c("g", "s","g","g","g","g","g","g","g"),
              align = c("c", "l", "c", "c", "c", "c", "c", "c", "c"),
              caption = paste0("Averaged mean square errors for the piecewise constant signal",
                               " plus smooth artifacts for various $a$ and $b$."),
              label = paste0("tab:aMSEcp"))
print(tab, include.rownames = FALSE, file = paste0(path, "aMSEcp.tex"), append = FALSE,
      sanitize.text.function=function(x){x})


settings <- c("cp", "cpLong", "cpShort", "cp2Long", "cp2Short", "cp4Long", "cp4Short")
a <- c(0, 0.01, 0.025, 0.01, 0.025, 0.01, 0.025)
b <- c(0, 0.2, 0.2, 0.4, 0.4, 0.8, 0.8)
methods <- c("method", "JIC", "PELT", "COPS", "SOPS")
# methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
                  "$\\COPS$", "$\\SOPS$")
# abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
#                   "$\\FL$", "$\\kernSmoothVfold$", "$\\kernSmoothLOOCV$")

tab <- matrix(0, length(methods), 9)
for (j in seq_along(methods)) {
  name <- paste0(settings[1])
  ret <- readRDS(paste0(path, name, "/summary.RDS"))
  
  tab[j, ] <- c(ret[[methods[j]]]$njumpsMean, ret[[methods[j]]]$njumpsQuantities * 100,
                mean(ret[[methods[j]]]$detected))
  
}
tab <- cbind(abbreviation, as.data.frame(tab))
tab <- cbind(a = rep(a[1], length(methods)), b = rep(b[1], length(methods)), tab)

methods <- c("method", "JIC", "PELT", "COPS", "SOPS")
# methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
                  "$\\COPS$", "$\\SOPS$")
# abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
#                   "$\\FL$", "$\\kernSmoothVfold$", "$\\kernSmoothLOOCV$")
  
for (i in 2:7) {
  tab2 <- matrix(0, length(methods), 9)
  for (j in seq_along(methods)) {
    name <- paste0(settings[i])
    ret <- readRDS(paste0(path, name, "/summary.RDS"))
    
    tab2[j, ] <- c(ret[[methods[j]]]$njumpsMean, ret[[methods[j]]]$njumpsQuantities * 100,
                  mean(ret[[methods[j]]]$detected))
    
  }
  tab2 <- cbind(abbreviation, as.data.frame(tab2))
  tab2 <- cbind(a = rep(a[i], length(methods)), b = rep(b[i], length(methods)), tab2)
  tab <- rbind(tab, tab2)
}

colnames(tab) <- c("a", "b", "Method", "$\\hat{K} - K$", "$<-2$", "$-2$", "$-1$", "$0$", "$1$", "$2$",
                   "$> 2$", "\\% detected")
tab <- xtable(tab, digits = rep(4, 13), display = c("g", "g", "g", "s","g","g","g", "g","g", "g", "g", "g", "g"),
              align = c("c", "c", "c", "l", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
              caption = paste0("Summary results about the detection of change-points for",
                               " the piecewise constant signal",
                               " plus smooth artifacts for various $a$ and $b$."),
              label = paste0("tab:CPcp"))
print(tab, include.rownames = FALSE, file = paste0(path, "CPcp.tex"), append = FALSE,
      sanitize.text.function=function(x){x}, hline.after = c(-1, 0:7 * 5))


