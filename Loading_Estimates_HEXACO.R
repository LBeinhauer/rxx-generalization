### Reliability Generalization HEXACO ###

## 08/03/2022




###################################################################################################
# This script is used to load the HEXACO reliability estimates generated previously,              #
###################################################################################################


# library loading and installing as necessary


# relevant R packages
packages <- c("metafor", "tidyverse", "here", "data.table", "lavaan")

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



full_paths <- list.files(here("Reliability Estimates"), full.name = TRUE)

relevant_paths <- full_paths[grep("Hex", full_paths)]


for(i in 1:length(relevant_paths)){
  fp <- relevant_paths[i]
  name <- substr(fp, (regexpr("Estimates", fp)[1] + 10), (nchar(fp) - 4))
  assign(name, read.csv(fp))
}


rm(full_paths, relevant_paths, fp, i, name)