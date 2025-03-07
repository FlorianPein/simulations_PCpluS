
library(xtable)
mainPath <- "~/Desktop/simulationPCpluS/"

path <- paste0(mainPath, "n256small/")

for (a in c(6, 4, 2)) {
  settings <- c("blocks", "burt", "cosine", "heavisine")
  methods <- c("method", "JIC", "PELT", "COPS", "SOPS", "SCHACEcv")
  # methods <- c("method", "JIC", "PELT", "FusedLasso", "kernelSmoothingVfold", "kernelSmoothingLOOCV")
  abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
                    "$\\COPS$", "$\\SOPS$", "$\\SCHACEcv$")
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
                caption = paste0("Averaged mean square errors for $n = 256$ and $a = ", a, "$."),
                label = paste0("tab:aMSEn256a", a))
  print(tab, include.rownames = FALSE, file = paste0(path, "aMSEn256a", a, ".tex"), append = FALSE,
        sanitize.text.function=function(x){x})
}

for (a in c(6, 4, 2)) {
  setting <- "blocks"
  methods <- c("method", "JIC", "PELT", "COPS", "SOPS", "SCHACEcv")
  abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", 
                    "$\\COPS$", "$\\SOPS$", "$\\SCHACEcv$")
  
  # methods <- c("method", "JIC", "PELT", "FusedLasso")
  # abbreviation <- c("$\\pcs$", "$\\JIC$", "$\\PELT$", "$\\FL$")
  
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
    methods <- c("method", "JIC", "COPS", "SOPS", "SCHACEcv")
    abbreviation <- c("$\\pcs$", "$\\JIC$",
                      "$\\COPS$", "$\\SOPS$", "$\\SCHACEcv$")
    
    # methods <- c("method", "JIC")
    # abbreviation <- c("$\\pcs$", "$\\JIC$")
    
    tab2 <- matrix(0, length(methods), 9)
    for (j in seq_along(methods)) {
      name <- paste0(setting, a)
      ret <- readRDS(paste0(path, name, "/summary.RDS"))
      
      tab2[j, ] <- c(ret[[methods[j]]]$njumpsMean, ret[[methods[j]]]$njumpsQuantities * 100,
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
                caption = paste0("Summary results about the detection of change-points for $n = 256$ and $a = ", a, "$."),
                label = paste0("tab:CPn256a", a))
  print(tab, include.rownames = FALSE, file = paste0(path, "CPn256a", a, ".tex"), append = FALSE,
        sanitize.text.function=function(x){x}, hline.after = c(4, 6, 8))
}

