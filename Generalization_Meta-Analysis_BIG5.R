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
#source(here("Generalization_Initialization_BIG5.R"))
source(here("Loading_Estimates_BIG5.R"))



# fitting random-effects meta-analysis to estimates of reliability using Cronbach's alpha & McDonald's omega
alpha.rma.fit_EX <- rel_rma(AlphaBF_EX)
# omega.rma.fit_EX <- rel_rma(OmegaBF_EX)
Bonett_alpha.rma.fit_EX <- rel_rma(BonettAlphaBF_EX)
# Bonett_omega.rma.fit_EX <- rel_rma(BonettOmegaBF_EX)

alpha.rma.fit_AG <- rel_rma(AlphaBF_AG)
# omega.rma.fit_AG <- rel_rma(OmegaBF_AG)
Bonett_alpha.rma.fit_AG <- rel_rma(BonettAlphaBF_AG)
# Bonett_omega.rma.fit_AG <- rel_rma(BonettOmegaBF_AG)

alpha.rma.fit_CO <- rel_rma(AlphaBF_CO)
# omega.rma.fit_CO <- rel_rma(OmegaBF_CO)
Bonett_alpha.rma.fit_CO <- rel_rma(BonettAlphaBF_CO)
# Bonett_omega.rma.fit_CO <- rel_rma(BonettOmegaBF_CO)

alpha.rma.fit_ES <- rel_rma(AlphaBF_ES)
# omega.rma.fit_ES <- rel_rma(OmegaBF_ES)
Bonett_alpha.rma.fit_ES <- rel_rma(BonettAlphaBF_ES)
# Bonett_omega.rma.fit_ES <- rel_rma(BonettOmegaBF_ES)

alpha.rma.fit_OX <- rel_rma(AlphaBF_OX)
# omega.rma.fit_OX <- rel_rma(OmegaBF_OX)
Bonett_alpha.rma.fit_OX <- rel_rma(BonettAlphaBF_OX)
# Bonett_omega.rma.fit_OX <- rel_rma(BonettOmegaBF_OX)






my_forest_plot(alpha.rma.fit_EX, AlphaBF_EX, main.title = "Forest Plot - Big 5 Extraversion",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
# my_forest_plot(omega.rma.fit_EX, OmegaBF_EX, main.title = "Forest Plot - Big 5 Extraversion",
#                x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_EX, BonettAlphaBF_EX, main.title = "Forest Plot - Big 5 Extraversion",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
# my_forest_plot(Bonett_omega.rma.fit_EX, BonettOmegaBF_EX, main.title = "Forest Plot - Big 5 Extraversion",
#                x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)


my_forest_plot(alpha.rma.fit_AG, AlphaBF_AG, main.title = "Forest Plot - Big 5 Agreeableness",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
# my_forest_plot(omega.rma.fit_AG, OmegaBF_AG, main.title = "Forest Plot - Big 5 Agreeableness",
#                x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_AG, BonettAlphaBF_AG, main.title = "Forest Plot - Big 5 Agreeableness",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
# my_forest_plot(Bonett_omega.rma.fit_AG, BonettOmegaBF_AG, main.title = "Forest Plot - Big 5 Agreeableness",
#                x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)


my_forest_plot(alpha.rma.fit_CO, AlphaBF_CO, main.title = "Forest Plot - Big 5 Conscientiousness",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
# my_forest_plot(omega.rma.fit_CO, OmegaBF_CO, main.title = "Forest Plot - Big 5 Conscientiousness",
#                x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_CO, BonettAlphaBF_CO, main.title = "Forest Plot - Big 5 Conscientiousness",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
# my_forest_plot(Bonett_omega.rma.fit_CO, BonettOmegaBF_CO, main.title = "Forest Plot - Big 5 Conscientiousness",
#                x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)


my_forest_plot(alpha.rma.fit_ES, AlphaBF_ES, main.title = "Forest Plot - Big 5 Emotional Stability",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
# my_forest_plot(omega.rma.fit_ES, OmegaBF_ES, main.title = "Forest Plot - Big 5 Emotional Stability",
#                x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_ES, BonettAlphaBF_ES, main.title = "Forest Plot - Big 5 Emotional Stability",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
# my_forest_plot(Bonett_omega.rma.fit_ES, BonettOmegaBF_ES, main.title = "Forest Plot - Big 5 Emotional Stability",
#                x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)


my_forest_plot(alpha.rma.fit_OX, AlphaBF_OX, main.title = "Forest Plot - Big 5 Openness to Experience",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
# my_forest_plot(omega.rma.fit_OX, OmegaBF_OX, main.title = "Forest Plot - Big 5 Openness to Experience",
#                x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_OX, BonettAlphaBF_OX, main.title = "Forest Plot - Big 5 Openness to Experience",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
# my_forest_plot(Bonett_omega.rma.fit_OX, BonettOmegaBF_OX, main.title = "Forest Plot - Big 5 Openness to Experience",
#                x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)












Reg_prep <- read.csv(here("Meta-Regression/meta_regression_dat_BIG5.csv"))



reg.alpha.rma.fit_EX <- rel_rma.reg_bf(merge(AlphaBF_EX, Reg_prep, by = "source"))
# reg.omega.rma.fit_EX <- rel_rma.reg_bf(merge(OmegaBF_EX, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_EX <- rel_rma.reg_bf(merge(BonettAlphaBF_EX, Reg_prep, by = "source"))
# reg.Bonett_omega.rma.fit_EX <- rel_rma.reg_bf(merge(BonettOmegaBF_EX, Reg_prep, by = "source"))

reg.alpha.rma.fit_AG <- rel_rma.reg_bf(merge(AlphaBF_AG, Reg_prep, by = "source"))
# reg.omega.rma.fit_AG <- rel_rma.reg_bf(merge(OmegaBF_AG, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_AG <- rel_rma.reg_bf(merge(BonettAlphaBF_AG, Reg_prep, by = "source"))
# reg.Bonett_omega.rma.fit_AG <- rel_rma.reg_bf(merge(BonettOmegaBF_AG, Reg_prep, by = "source"))

reg.alpha.rma.fit_CO <- rel_rma.reg_bf(merge(AlphaBF_CO, Reg_prep, by = "source"))
# reg.omega.rma.fit_CO <- rel_rma.reg_bf(merge(OmegaBF_CO, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_CO <- rel_rma.reg_bf(merge(BonettAlphaBF_CO, Reg_prep, by = "source"))
# reg.Bonett_omega.rma.fit_CO <- rel_rma.reg_bf(merge(BonettOmegaBF_CO, Reg_prep, by = "source"))

reg.alpha.rma.fit_ES <- rel_rma.reg_bf(merge(AlphaBF_ES, Reg_prep, by = "source"))
# reg.omega.rma.fit_ES <- rel_rma.reg_bf(merge(OmegaBF_ES, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_ES <- rel_rma.reg_bf(merge(BonettAlphaBF_ES, Reg_prep, by = "source"))
# reg.Bonett_omega.rma.fit_ES <- rel_rma.reg_bf(merge(BonettOmegaBF_ES, Reg_prep, by = "source"))

reg.alpha.rma.fit_OX <- rel_rma.reg_bf(merge(AlphaBF_OX, Reg_prep, by = "source"))
# reg.omega.rma.fit_OX <- rel_rma.reg_bf(merge(OmegaBF_OX, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_OX <- rel_rma.reg_bf(merge(BonettAlphaBF_OX, Reg_prep, by = "source"))
# reg.Bonett_omega.rma.fit_OX <- rel_rma.reg_bf(merge(BonettOmegaBF_OX, Reg_prep, by = "source"))





