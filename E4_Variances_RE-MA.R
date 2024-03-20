### Reliability Generalization - Meta-Analysis of Variance Components ###

# ███████╗░░██╗██╗
# ██╔════╝░██╔╝██║
# █████╗░░██╔╝░██║
# ██╔══╝░░███████║
# ███████╗╚════██║
# ╚══════╝░░░░░╚═╝

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


# load project-specific function-library from an R-script
source(here("RG_function-library.R"))


# extrac full paths containing the extracted data for separate projects
path_data <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE)

# extract separate project data into a single list
data.list <- lapply(path_data, read.csv)

# add names to list elements
names(data.list) <- substr(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE)[-7],
                           1, (nchar(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE)[-7])-4))





# set a seed to make REML-estimation replicable
set.seed(070622)


# generate estimates of ln-error score variance and its associated standard error using bootstrapping
varE_est.L <- lapply(seq_along(data.list), FUN = function(x){
  
  # sometimes calculation of SE might lead to issues, as negative variances can not be log-transformed
  #  therefore, function is run within a tryCatch-environment, so the script does not crash
  # apply_Bootstrap_SE_Project.specific is the project-specific function
  tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "ERROR"),
           
           # print error to console
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
})

# save list-object of ln-error score variance estimates in sub-folder
saveRDS(varE_est.L, file = here("Data/Variance Estimates/bootstrapped_varE.RData"))


# Perform random-effects meta-analysis on estimates of ln-error score variance using metafor
varE_rma.list <- lapply(seq_along(varE_est.L), FUN = function(x){
  
  # at times estimates of ln-varE & SE may be NA, as negative estimates can't be transformed
  #  therefore, function is nested in tryCatch, so it doesn't break down with errors
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = varE_est.L[[x]]$var.est, 
                        sei = varE_est.L[[x]]$SE),
           
           # print error to console
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})

# add names to list-object
names(varE_rma.list) <- names(data.list)

# store list-object of results of RE-MA in separate file
saveRDS(varE_rma.list, file = here("Data/Variance Estimates/bootstrapped_varE_rma.RData"))


# generate estimates of ln-observed score variance for each sample & projects
varX_est.L <- lapply(data.list, FUN = function(data){
  
  # apply over labs in MASC
  df <- apply(as.matrix(seq_along(unique(data$source))), MARGIN = 1, FUN = function(x){
    
    # remove source-col & omit NAs
    d <- data[data$source == unique(data$source)[x],-grep("source", names(data))]
    D <- na.omit(d)
    
    # estimate observed score variance
    varX <- var(rowMeans(D))
    
    # store SE, ln(varX) & varX
    return(data.frame(SE = sqrt((2/(nrow(D) - 1))), 
                      boot.mean = varX,
                      var.emp = log(varX)))
    
  })
    
  # return formatted data.frame
  df.formatted <- data.frame(SE = sapply(df, FUN = function(x){x$SE}),
                             boot.mean = sapply(df, FUN = function(x){x$boot.mean}),
                             var.est = sapply(df, FUN = function(x){x$var.emp}),
                             source = unique(data$source))

})

# save list-object of ln-true score variance estimates in sub-folder
saveRDS(varX_est.L, file = here("Data/Variance Estimates/bootstrapped_varX.RData"))




# Perform random-effects meta-analysis on estimates of ln-observed score variance using metafor
varX_rma.list <- lapply(seq_along(varX_est.L), FUN = function(x){
  
  # at times estimates of ln-varX & SE may be NA, as negative estimates can't be transformed
  #  therefore, function is nested in tryCatch, so it doesn't break down with errors
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = varX_est.L[[x]]$var.est, 
                        sei = varX_est.L[[x]]$SE),
           
           # print error to console
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})

# add names to list-object
names(varX_rma.list) <- names(data.list)

# store list-object of results of RE-MA in separate file
saveRDS(varX_rma.list, file = here("Data/Variance Estimates/bootstrapped_varx_rma.RData"))

