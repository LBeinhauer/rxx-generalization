### Reliability Generalization HEXACO | Big Five TIPI ###

## 08/03/2022




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
#source(here("Generalization_Initialization.R"))
source(here("Loading_Estimates.R"))



# fitting random-effects meta-analysis to estimates of reliability using Cronbach's alpha & McDonald's omega
alpha.rma.fit_HH <- rel_rma(AlphaHH)
omega.rma.fit_HH <- rel_rma(OmegaHH)
Bonett.rma.fit_HH <- rel_rma(BonettHH)

alpha.rma.fit_EM <- rel_rma(AlphaEM)
omega.rma.fit_EM <- rel_rma(OmegaEM)
Bonett.rma.fit_EM <- rel_rma(BonettEM)

alpha.rma.fit_EX <- rel_rma(AlphaEX)
omega.rma.fit_EX <- rel_rma(OmegaEX)
Bonett.rma.fit_EX <- rel_rma(BonettEX)

alpha.rma.fit_AG <- rel_rma(AlphaAG)
omega.rma.fit_AG <- rel_rma(OmegaAG)
Bonett.rma.fit_AG <- rel_rma(BonettAG)

alpha.rma.fit_CO <- rel_rma(AlphaCO)
omega.rma.fit_CO <- rel_rma(OmegaCO)
Bonett.rma.fit_CO <- rel_rma(BonettCO)

alpha.rma.fit_OX <- rel_rma(AlphaOX)
omega.rma.fit_OX <- rel_rma(OmegaOX)
Bonett.rma.fit_OX <- rel_rma(BonettOX)





Reg_prep <- read.csv(here("Meta-Regression/meta_regression_dat.csv"))


reg.alpha.rma.fit_OX <- rel_rma.reg(AlphaHH)



