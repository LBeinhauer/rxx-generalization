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


# source the function-library R-script
source(here("RG_function-library.R"))


# identify paths of relevant data-files
path_data <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE)


# read data-files. Resulting Object is a list.
data.list <- lapply(path_data, read.csv)

# extract project names from the path-files
names(data.list) <- substr(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE),
                           1, (nchar(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE))-4))



# Generate estimates of Cronbach's Alpha, using pre-defined function in an apply-loop
alpha_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  estimate_alpha(data.list[[x]], csv = TRUE, 
                 project.title = names(data.list)[x])
                   
})



#  Generate estimates of Bonnett-transformed Cronbach's Alpha, using pre-defined function
# in an apply-loop. Each iteration is wrapped in a tryCatch function, as some extreme cases
# might produce negative variances, which lead to errors in the sqrt-function. Currenty, these
# cases are dropped.
Bonett.alpha_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  
  # tryCatch wrapper
  tryCatch({
    # estimate Bonnett-transform of Cronbach's Alpha
    estimate_Bonett_alpha(data.list[[x]], csv = TRUE, 
                          project.title = names(data.list)[x])
  },
  
  # print error to console
  error = function(e)(cat("ERROR: ", conditionMessage(e), " - ", 
                          names(data.list)[x], 
                          " - ", x, "\n"))
    
  )
  
})





