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

# currently does not work! Produces errors, while running script directly does not (unclear why)
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



my_forest_plot(alpha.rma.fit_HH, AlphaHH, main.title = "Forest Plot - HEXACO Honesty-Humility",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_HH, OmegaHH, main.title = "Forest Plot - HEXACO Honesty-Humility",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett.rma.fit_HH, BonettHH, main.title = "Forest Plot - HEXACO Honesty-Humility",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_EM, AlphaEM, main.title = "Forest Plot - HEXACO Emotionality",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_EM, OmegaEM, main.title = "Forest Plot - HEXACO Emotionality",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett.rma.fit_EM, BonettEM, main.title = "Forest Plot - HEXACO Emotionality",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_EX, AlphaEX, main.title = "Forest Plot - HEXACO Extraversion",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_EX, OmegaEX, main.title = "Forest Plot - HEXACO Extraversion",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett.rma.fit_EX, BonettEX, main.title = "Forest Plot - HEXACO Extraversion",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_AG, AlphaAG, main.title = "Forest Plot - HEXACO Agreeableness",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_AG, OmegaAG, main.title = "Forest Plot - HEXACO Agreeableness",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett.rma.fit_AG, BonettAG, main.title = "Forest Plot - HEXACO Agreeableness",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_CO, AlphaCO, main.title = "Forest Plot - HEXACO Conscientiousness",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_CO, OmegaCO, main.title = "Forest Plot - HEXACO Conscientiousness",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett.rma.fit_CO, BonettCO, main.title = "Forest Plot - HEXACO Conscientiousness",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_OX, AlphaOX, main.title = "Forest Plot - HEXACO Openness to Experience",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_OX, OmegaOX, main.title = "Forest Plot - HEXACO Openness to Experience",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett.rma.fit_OX, BonettOX, main.title = "Forest Plot - HEXACO Openness to Experience",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)









Reg_prep <- read.csv(here("Meta-Regression/meta_regression_dat.csv"))



reg.alpha.rma.fit_HH <- rel_rma.reg(merge(AlphaHH, Reg_prep, by = "source"))
reg.omega.rma.fit_HH <- rel_rma.reg(merge(OmegaHH, Reg_prep, by = "source"))
reg.Bonett.rma.fit_HH <- rel_rma.reg(merge(BonettHH, Reg_prep, by = "source"))

reg.alpha.rma.fit_EM <- rel_rma.reg(merge(AlphaEM, Reg_prep, by = "source"))
reg.omega.rma.fit_EM <- rel_rma.reg(merge(OmegaEM, Reg_prep, by = "source"))
reg.Bonett.rma.fit_EM <- rel_rma.reg(merge(BonettEM, Reg_prep, by = "source"))

reg.alpha.rma.fit_EX <- rel_rma.reg(merge(AlphaEX, Reg_prep, by = "source"))
reg.omega.rma.fit_EX <- rel_rma.reg(merge(OmegaEX, Reg_prep, by = "source"))
reg.Bonett.rma.fit_EX <- rel_rma.reg(merge(BonettEX, Reg_prep, by = "source"))

reg.alpha.rma.fit_AG <- rel_rma.reg(merge(AlphaAG, Reg_prep, by = "source"))
reg.omega.rma.fit_AG <- rel_rma.reg(merge(OmegaAG, Reg_prep, by = "source"))
reg.Bonett.rma.fit_AG <- rel_rma.reg(merge(BonettAG, Reg_prep, by = "source"))

reg.alpha.rma.fit_CO <- rel_rma.reg(merge(AlphaCO, Reg_prep, by = "source"))
reg.omega.rma.fit_CO <- rel_rma.reg(merge(OmegaCO, Reg_prep, by = "source"))
reg.Bonett.rma.fit_CO <- rel_rma.reg(merge(BonettCO, Reg_prep, by = "source"))

reg.alpha.rma.fit_OX <- rel_rma.reg(merge(AlphaOX, Reg_prep, by = "source"))
reg.omega.rma.fit_OX <- rel_rma.reg(merge(OmegaOX, Reg_prep, by = "source"))
reg.Bonett.rma.fit_OX <- rel_rma.reg(merge(BonettOX, Reg_prep, by = "source"))




