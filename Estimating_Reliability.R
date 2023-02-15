### Reliability Generalization  ###

## 02/06/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary



packages <- c("tidyverse", "here", "psych", "coefficientalpha")

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

names(data.list) <- substr(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE),
                           1, (nchar(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE))-4))



alpha_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  estimate_alpha(data.list[[x]], csv = TRUE, 
                 project.title = names(data.list)[x])
                   
})






omega_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  
  tryCatch({
    estimate_omega(data.list[[x]][which(rowSums(is.na(data.list[[x]])) <= 1),], csv = TRUE, 
                   project.title = names(data.list)[x])
  },
  
  error = function(e)(cat("ERROR: ", conditionMessage(e), " - ", 
                          names(data.list)[x], 
                          " - ", x, "\n"))
  )
  

})


Bonett.alpha_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  tryCatch({
    estimate_Bonett_alpha(data.list[[x]], csv = TRUE, 
                          project.title = names(data.list)[x])
  },
  
  error = function(e)(cat("ERROR: ", conditionMessage(e), " - ", 
                          names(data.list)[x], 
                          " - ", x, "\n"))
  
    
  )
  
})




Bonett.omega_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  tryCatch({
    estimate_Bonett_omega(data.list[[x]][which(rowSums(is.na(data.list[[x]])) <= 1),], csv = TRUE, 
                          project.title = names(data.list)[x])
  },
  
  error = function(e)(cat("ERROR: ", conditionMessage(e), " - ", 
                          names(data.list)[x], 
                          " - ", x, "\n"))
  
  )
  
})


