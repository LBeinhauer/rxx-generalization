### Reliability Generalization  ###

# ███████╗██████╗░
# ██╔════╝╚════██╗
# █████╗░░░█████╔╝
# ██╔══╝░░░╚═══██╗
# ███████╗██████╔╝
# ╚══════╝╚═════╝░

###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "metafor")

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



# get full paths to objects containing the separate Reliability esimtates
Reliability_estimates_paths <- list.files(here("Data/Reliability Estimates"), full.names =  TRUE)

# make selection of paths only to objects of untransformed Cronbach's Alpha
Alpha_estimates_paths <- Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)]

# perform random-effects meta-analyses using the metafor package
Alpha_rma.list <- lapply(Alpha_estimates_paths, FUN = function(x){
  d <- read.csv(x)
  
  metafor::rma(measure = "GEN", method = "REML", yi = d$Reliability, sei = d$StandardError)
})

# add names to the list-elements
names(Alpha_rma.list) <- substr(Alpha_estimates_paths,
                                (regexpr("Reliability Estimates/", Alpha_estimates_paths) + 22),
                                (nchar(Alpha_estimates_paths)-10))



# make selection of paths only to objects of Bonett-transformed Cronbach's Alpha
Bonett.Alpha_estimates_paths <- Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)]

# perform random-effects meta-analyses using the metafor package
Bonett.Alpha_rma.list <- lapply(Bonett.Alpha_estimates_paths, FUN = function(x){
  d <- read.csv(x)
  
  metafor::rma(measure = "GEN", method = "REML", yi = d$Reliability, sei = d$StandardError)
})

# add names to the list-elements
names(Bonett.Alpha_rma.list) <- substr(Bonett.Alpha_estimates_paths,
                                       (regexpr("Reliability Estimates/", Bonett.Alpha_estimates_paths) + 22),
                                       (nchar(Bonett.Alpha_estimates_paths)-17))



# store meta-analysis results in separate .RData files
saveRDS(Alpha_rma.list, file = here("Data/Shiny Data/Alpha_rma.list.RData"))
saveRDS(Bonett.Alpha_rma.list, file = here("Data/Shiny Data/Bonett.Alpha_rma.list.RData"))
