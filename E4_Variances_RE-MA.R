### Reliability Generalization - Meta-Analysis of Variance Components ###

## 07/06/2022




###################################################################################################
# This script is used purely for breaking down sample variance into components containing either  #
#  True or Error Variance (according to CTT) using estimates of reliability. Using Bootstrapping, #
#  we can generate standard error estimates for these components, and run a random-effects meta-  #
#  analysis.                                                                                      #
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "psych", "coefficientalpha", "boot")

# check, whether library already installed or not - install and load as needed:
apply(as.matrix(packages), MARGIN = 1, FUN = function(x) {
  
  pkg_avail <- nzchar(system.file(package = x))   # check if library is installed on system
  
  if(pkg_avail){
    require(x, character.only = TRUE)             # load the library, if already installed
    
  }else{
    install.packages(x)                           # install the library, if missing
    require(x, character.only = TRUE)             # load after installation
  }
})

source(here("RG_function-library.R"))



path_data <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE)


data.list <- lapply(path_data, read.csv)


names(data.list) <- substr(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE)[-7],
                           1, (nchar(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE)[-7])-4))






set.seed(070622)

# long_test_T <- lapply(seq_along(data.list), FUN = function(x){
#   tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "TRUE"),
#            error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
#                                    names(data.list)[x], 
#                                    " - ", x, "\n")))
# })
# 
# saveRDS(long_test_T, file = here("Data/Variance Estimates/bootstrapped_varT.RData"))

long_test_E <- lapply(seq_along(data.list), FUN = function(x){
  tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "ERROR"),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
})

saveRDS(long_test_E, file = here("Data/Variance Estimates/bootstrapped_varE.RData"))


varE_rma.list <- lapply(seq_along(long_test_E), FUN = function(x){
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = long_test_E[[x]]$var.est, 
                        sei = long_test_E[[x]]$SE),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})

names(varE_rma.list) <- names(data.list)

saveRDS(varE_rma.list, file = here("Data/Variance Estimates/bootstrapped_varE_rma.RData"))


long_test_X <- lapply(data.list, FUN = function(data){
  
  df <- apply(as.matrix(seq_along(unique(data$source))), MARGIN = 1, FUN = function(x){
    
    
    d <- data[data$source == unique(data$source)[x],-grep("source", names(data))]
    
    D <- na.omit(d)
    
    
    varX <- var(rowMeans(D))
    
    return(data.frame(SE = sqrt((2/(nrow(D) - 1))), 
                      boot.mean = varX,
                      var.emp = log(varX)))
    
  })
    
df.formatted <- data.frame(SE = sapply(df, FUN = function(x){x$SE}),
                           boot.mean = sapply(df, FUN = function(x){x$boot.mean}),
                           var.est = sapply(df, FUN = function(x){x$var.emp}),
                           source = unique(data$source))
})

saveRDS(long_test_X, file = here("Data/Variance Estimates/bootstrapped_varX.RData"))





varX_rma.list <- lapply(seq_along(long_test_X), FUN = function(x){
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = long_test_E[[x]]$var.est, 
                        sei = long_test_E[[x]]$SE),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})

names(varX_rma.list) <- names(data.list)

saveRDS(varX_rma.list, file = here("Data/Variance Estimates/bootstrapped_varx_rma.RData"))