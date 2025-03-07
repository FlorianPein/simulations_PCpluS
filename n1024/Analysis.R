
library(xtable)
mainPath <- "~/Desktop/simulationPCpluS/"

path <- paste0(mainPath, "n1024/")

for (a in c(4, 2, 1)) {
  settings <- c("blocks", "burt", "cosine", "heavisine")
  methods <- c("method", "JIC", "PELT", "COPS", "SOPS")
  # methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
  abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
                    "$\\COPS$", "$\\SOPS$")
  # abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
  #                   "$\\FL$", "$\\kernSmoothVfold$", "$\\kernSmoothLOOCV$")
  
  tab <- matrix(0, length(methods), length(settings))
  for (i in seq_along(settings)) {
    for (j in seq_along(methods)) {
      name <- paste0(settings[i], a)
      ret <- readRDS(paste0(path, name, "/summary.RDS"))
      
      tab[j, i] <- ret[[methods[j]]]$amse
    }
  }
  tab <- cbind(abbreviation, as.data.frame(tab))
  colnames(tab) <- c("Method", settings)
  tab <- xtable(tab, digits = rep(4, 6), display = c("g", "s","g","g","g", "g"),
                align = c("c", "l", "c", "c", "c", "c"),
                caption = paste0("Averaged mean square errors for $n = 1024$ and $a = ", a, "$."),
                label = paste0("tab:aMSEn1024a", a))
  print(tab, include.rownames = FALSE, file = paste0(path, "aMSEn1024a", a, ".tex"), append = FALSE,
        sanitize.text.function=function(x){x})
}

for (a in c(4, 2, 1)) {
  setting <- "blocks"
  methods <- c("method", "JIC", "PELT", "COPS", "SOPS")
  # methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
  abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
                    "$\\COPS$", "$\\SOPS$")
  # abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
  #                   "$\\FL$", "$\\kernSmoothVfold$", "$\\kernSmoothLOOCV$")
  
  tab <- matrix(0, length(methods), 9)
  for (j in seq_along(methods)) {
    name <- paste0(setting, a)
    ret <- readRDS(paste0(path, name, "/summary.RDS"))
    
    tab[j, ] <- c(ret[[methods[j]]]$njumpsMean, ret[[methods[j]]]$njumpsQuantities,
                  mean(ret[[methods[j]]]$detected))
    
  }
  tab <- cbind(abbreviation, as.data.frame(tab))
  tab <- cbind(rep(setting, length(methods)), tab)
  
  for (setting in c("burt", "cosine", "heavisine")) {
    methods <- c("method", "JIC", "COPS", "SOPS")
    # methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
    abbreviation <- c("$\\pcs$", "$\\JIC$",
                      "$\\COPS$", "$\\SOPS$")
    # abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
    #                   "$\\FL$", "$\\kernSmoothVfold$", "$\\kernSmoothLOOCV$")
    
    tab2 <- matrix(0, length(methods), 9)
    for (j in seq_along(methods)) {
      name <- paste0(setting, a)
      ret <- readRDS(paste0(path, name, "/summary.RDS"))
      
      tab2[j, ] <- c(ret[[methods[j]]]$njumpsMean, ret[[methods[j]]]$njumpsQuantities,
                    mean(ret[[methods[j]]]$detected))
      
    }
    tab2 <- cbind(abbreviation, as.data.frame(tab2))
    tab2 <- cbind(rep(setting, length(methods)), tab2)
    tab <- rbind(tab, tab2)
  }
  colnames(tab) <- c("Setting", "Method", "$\\hat{K} - K$", "$< -2$", "$-2$", "$-1$", "$0$", "$1$", "$2$",
                     "$> 2$", "\\% detected")
  tab <- xtable(tab, digits = rep(4, 12), display = c("g", "s", "s","g","g","g", "g", "g","g","g", "g", "g"),
                align = c("c", "l", "l", "c", "c", "c", "c", "c", "c", "c", "c", "c"),
                caption = paste0("Summary results about the detection of change-points for $n = 1024$ and $a = ", a, "$."),
                label = paste0("tab:CPn1024a", a))
  print(tab, include.rownames = FALSE, file = paste0(path, "CPn1024a", a, ".tex"), append = FALSE,
        sanitize.text.function=function(x){x}, hline.after = c(-1, 0, 4, 6, 8, 10))
}

