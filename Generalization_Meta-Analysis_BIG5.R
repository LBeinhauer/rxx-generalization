### Reliability Generalization Big Five TIPI ###

## 10/03/2022




###################################################################################################
# This script is used for meta-analysis of the reliability estimates generated previously.        #
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



source(here("ReliabilityFunctions_RG.R"))

# only run if you wish to recompute the reliability estimates!

# currently does not work! Produces errors, while running script directly does not (unclear why)
#source(here("Generalization_Initialization.R"))
source(here("Loading_Estimates.R"))



# fitting random-effects meta-analysis to estimates of reliability using Cronbach's alpha & McDonald's omega
rel.rma.fit_BF.EX <- rma(measure = "GEN", yi = est_BF.EX$reliability, sei = est_BF.EX$StandardError) # alpha HH
o.rel.rma.fit_BF.EX <- rma(measure = "GEN", yi = omega_BF.EX$reliability, sei = omega_BF.EX$StandardError) # omega HH
t.rel.rma.fit_BF.EX <- rma(measure = "GEN", yi = transformed_BF.EX$reliability, sei = transformed_BF.EX$StandardError) # transformed HH

rel.rma.fit_BF.AG <- rma(measure = "GEN", yi = est_BF.AG$reliability, sei = est_BF.AG$StandardError) # alpha HH
o.rel.rma.fit_BF.AG <- rma(measure = "GEN", yi = omega_BF.AG$reliability, sei = omega_BF.AG$StandardError) # omega HH
t.rel.rma.fit_BF.AG <- rma(measure = "GEN", yi = transformed_BF.AG$reliability, sei = transformed_BF.AG$StandardError) # transformed HH

rel.rma.fit_BF.CO <- rma(measure = "GEN", yi = est_BF.CO$reliability, sei = est_BF.CO$StandardError) # alpha HH
o.rel.rma.fit_BF.CO <- rma(measure = "GEN", yi = omega_BF.CO$reliability, sei = omega_BF.CO$StandardError) # omega HH
t.rel.rma.fit_BF.CO <- rma(measure = "GEN", yi = transformed_BF.CO$reliability, sei = transformed_BF.CO$StandardError) # transformed HH

rel.rma.fit_BF.ES <- rma(measure = "GEN", yi = est_BF.ES$reliability, sei = est_BF.ES$StandardError) # alpha HH
o.rel.rma.fit_BF.ES <- rma(measure = "GEN", yi = omega_BF.ES$reliability, sei = omega_BF.ES$StandardError) # omega HH
t.rel.rma.fit_BF.ES <- rma(measure = "GEN", yi = transformed_BF.ES$reliability, sei = transformed_BF.ES$StandardError) # transformed HH

rel.rma.fit_BF.OX <- rma(measure = "GEN", yi = est_BF.OX$reliability, sei = est_BF.OX$StandardError) # alpha HH
o.rel.rma.fit_BF.OX <- rma(measure = "GEN", yi = omega_BF.OX$reliability, sei = omega_BF.OX$StandardError) # omega HH
t.rel.rma.fit_BF.OX <- rma(measure = "GEN", yi = transformed_BF.OX$reliability, sei = transformed_BF.OX$StandardError) # transformed HH

