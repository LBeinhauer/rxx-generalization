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
#source(here("Generalization_Initialization_HEXACO.R"))
source(here("Loading_Estimates_HEXACO.R"))



# fitting random-effects meta-analysis to estimates of reliability using Cronbach's alpha & McDonald's omega
alpha.rma.fit_HH <- rel_rma(AlphaHex_HH)
omega.rma.fit_HH <- rel_rma(OmegaHex_HH)
Bonett_alpha.rma.fit_HH <- rel_rma(BonettAlphaHex_HH)
Bonett_omega.rma.fit_HH <- rel_rma(BonettOmegaHex_HH)

alpha.rma.fit_EM <- rel_rma(AlphaHex_EM)
omega.rma.fit_EM <- rel_rma(OmegaHex_EM)
Bonett_alpha.rma.fit_EM <- rel_rma(BonettAlphaHex_EM)
Bonett_omega.rma.fit_EM <- rel_rma(BonettOmegaHex_EM)

alpha.rma.fit_EX <- rel_rma(AlphaHex_EX)
omega.rma.fit_EX <- rel_rma(OmegaHex_EX)
Bonett_alpha.rma.fit_EX <- rel_rma(BonettAlphaHex_EX)
Bonett_omega.rma.fit_EX <- rel_rma(BonettOmegaHex_EX)

alpha.rma.fit_AG <- rel_rma(AlphaHex_AG)
omega.rma.fit_AG <- rel_rma(OmegaHex_AG)
Bonett_alpha.rma.fit_AG <- rel_rma(BonettAlphaHex_AG)
Bonett_omega.rma.fit_AG <- rel_rma(BonettOmegaHex_AG)

alpha.rma.fit_CO <- rel_rma(AlphaHex_CO)
omega.rma.fit_CO <- rel_rma(OmegaHex_CO)
Bonett_alpha.rma.fit_CO <- rel_rma(BonettAlphaHex_CO)
Bonett_omega.rma.fit_CO <- rel_rma(BonettOmegaHex_CO)

alpha.rma.fit_OX <- rel_rma(AlphaHex_OX)
omega.rma.fit_OX <- rel_rma(OmegaHex_OX)
Bonett_alpha.rma.fit_OX <- rel_rma(BonettAlphaHex_OX)
Bonett_omega.rma.fit_OX <- rel_rma(BonettOmegaHex_OX)



my_forest_plot(alpha.rma.fit_HH, AlphaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_HH, OmegaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_HH, BonettAlphaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_HH, BonettOmegaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_EM, AlphaHex_EM, main.title = "Forest Plot - HEXACO Emotionality",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_EM, OmegaHex_EM, main.title = "Forest Plot - HEXACO Emotionality",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_EM, BonettAlphaHex_EM, main.title = "Forest Plot - HEXACO Emotionality",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_EM, BonettOmegaHex_EM, main.title = "Forest Plot - HEXACO Emotionality",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_EX, AlphaHex_EX, main.title = "Forest Plot - HEXACO Extraversion",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_EX, OmegaHex_EX, main.title = "Forest Plot - HEXACO Extraversion",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_EX, BonettAlphaHex_EX, main.title = "Forest Plot - HEXACO Extraversion",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_EX, BonettOmegaHex_EX, main.title = "Forest Plot - HEXACO Extraversion",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_AG, AlphaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_AG, OmegaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_AG, BonettAlphaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_AG, BonettOmegaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_CO, AlphaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_CO, OmegaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_CO, BonettAlphaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_CO, BonettOmegaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_OX, AlphaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_OX, OmegaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_OX, BonettAlphaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_OX, BonettOmegaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)









Reg_prep <- read.csv(here("Meta-Regression/meta_regression_dat_HEXACO.csv"))



reg.alpha.rma.fit_HH <- rel_rma.reg_hex(merge(AlphaHex_HH, Reg_prep, by = "source"))
reg.omega.rma.fit_HH <- rel_rma.reg_hex(merge(OmegaHex_HH, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_HH <- rel_rma.reg_hex(merge(BonettAlphaHex_HH, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_HH <- rel_rma.reg_hex(merge(BonettOmegaHex_HH, Reg_prep, by = "source"))

reg.alpha.rma.fit_EM <- rel_rma.reg_hex(merge(AlphaHex_EM, Reg_prep, by = "source"))
reg.omega.rma.fit_EM <- rel_rma.reg_hex(merge(OmegaHex_EM, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_EM <- rel_rma.reg_hex(merge(BonettAlphaHex_EM, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_EM <- rel_rma.reg_hex(merge(BonettOmegaHex_EM, Reg_prep, by = "source"))

reg.alpha.rma.fit_EX <- rel_rma.reg_hex(merge(AlphaHex_EX, Reg_prep, by = "source"))
reg.omega.rma.fit_EX <- rel_rma.reg_hex(merge(OmegaHex_EX, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_EX <- rel_rma.reg_hex(merge(BonettAlphaHex_EX, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_EX <- rel_rma.reg_hex(merge(BonettOmegaHex_EX, Reg_prep, by = "source"))

reg.alpha.rma.fit_AG <- rel_rma.reg_hex(merge(AlphaHex_AG, Reg_prep, by = "source"))
reg.omega.rma.fit_AG <- rel_rma.reg_hex(merge(OmegaHex_AG, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_AG <- rel_rma.reg_hex(merge(BonettAlphaHex_AG, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_AG <- rel_rma.reg_hex(merge(BonettOmegaHex_AG, Reg_prep, by = "source"))

reg.alpha.rma.fit_CO <- rel_rma.reg_hex(merge(AlphaHex_CO, Reg_prep, by = "source"))
reg.omega.rma.fit_CO <- rel_rma.reg_hex(merge(OmegaHex_CO, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_CO <- rel_rma.reg_hex(merge(BonettAlphaHex_CO, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_CO <- rel_rma.reg_hex(merge(BonettOmegaHex_CO, Reg_prep, by = "source"))

reg.alpha.rma.fit_OX <- rel_rma.reg_hex(merge(AlphaHex_OX, Reg_prep, by = "source"))
reg.omega.rma.fit_OX <- rel_rma.reg_hex(merge(OmegaHex_OX, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_OX <- rel_rma.reg_hex(merge(BonettAlphaHex_OX, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_OX <- rel_rma.reg_hex(merge(BonettOmegaHex_OX, Reg_prep, by = "source"))




