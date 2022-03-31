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



my_forest_plot(alpha.rma.fit_HH, AlphaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Item-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_HH, OmegaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Item-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_HH, BonettAlphaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Item-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_HH, BonettOmegaHex_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Item-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_EM, AlphaHex_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Item-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_EM, OmegaHex_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Item-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_EM, BonettAlphaHex_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Item-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_EM, BonettOmegaHex_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Item-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_EX, AlphaHex_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Item-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_EX, OmegaHex_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Item-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_EX, BonettAlphaHex_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Item-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_EX, BonettOmegaHex_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Item-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_AG, AlphaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Item-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_AG, OmegaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Item-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_AG, BonettAlphaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Item-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_AG, BonettOmegaHex_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Item-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_CO, AlphaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Item-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_CO, OmegaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Item-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_CO, BonettAlphaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Item-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_CO, BonettOmegaHex_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Item-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_OX, AlphaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Item-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_OX, OmegaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Item-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_OX, BonettAlphaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Item-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_OX, BonettOmegaHex_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Item-Level",
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




reg.alpha.rma.fit_scorevar_HH <- rma(data = merge(AlphaHex_HH, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_hh, measure = "GEN", method = "REML")
reg.omega.rma.fit_scorevar_HH <- rma(data = merge(OmegaHex_HH, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_hh)
reg.Bonett_alpha.rma.fit_scorevar_HH <- rma(data = merge(BonettAlphaHex_HH, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_hh)
reg.Bonett_omega.rma.fit_scorevar_HH <- rma(data = merge(BonettOmegaHex_HH, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_hh)

reg.alpha.rma.fit_scorevar_EM <- rma(data = merge(AlphaHex_EM, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_em)
reg.omega.rma.fit_scorevar_EM <- rma(data = merge(OmegaHex_EM, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_em)
reg.Bonett_alpha.rma.fit_scorevar_EM <- rma(data = merge(BonettAlphaHex_EM, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_em)
reg.Bonett_omega.rma.fit_scorevar_EM <- rma(data = merge(BonettOmegaHex_EM, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_em)

reg.alpha.rma.fit_scorevar_EX <- rma(data = merge(AlphaHex_EX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_ex)
reg.omega.rma.fit_scorevar_EX <- rma(data = merge(OmegaHex_EX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_ex)
reg.Bonett_alpha.rma.fit_scorevar_EX <- rma(data = merge(BonettAlphaHex_EX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_ex)
reg.Bonett_omega.rma.fit_scorevar_EX <- rma(data = merge(BonettOmegaHex_EX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_ex)

reg.alpha.rma.fit_scorevar_AG <- rma(data = merge(AlphaHex_AG, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_ag)
reg.omega.rma.fit_scorevar_AG <- rma(data = merge(OmegaHex_AG, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_ag)
reg.Bonett_alpha.rma.fit_scorevar_AG <- rma(data = merge(BonettAlphaHex_AG, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_ag)
reg.Bonett_omega.rma.fit_scorevar_AG <- rma(data = merge(BonettOmegaHex_AG, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_ag)

reg.alpha.rma.fit_scorevar_CO <- rma(data = merge(AlphaHex_CO, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_co)
reg.omega.rma.fit_scorevar_CO <- rma(data = merge(OmegaHex_CO, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_co)
reg.Bonett_alpha.rma.fit_scorevar_CO <- rma(data = merge(BonettAlphaHex_CO, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_co)
reg.Bonett_omega.rma.fit_scorevar_CO <- rma(data = merge(BonettOmegaHex_CO, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_co)

reg.alpha.rma.fit_scorevar_OX <- rma(data = merge(AlphaHex_OX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_ox)
reg.omega.rma.fit_scorevar_OX <- rma(data = merge(OmegaHex_OX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_ox)
reg.Bonett_alpha.rma.fit_scorevar_OX <- rma(data = merge(BonettAlphaHex_OX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_ox)
reg.Bonett_omega.rma.fit_scorevar_OX <- rma(data = merge(BonettOmegaHex_OX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_ox)




#### Facet-level analysis




# fitting random-effects meta-analysis to estimates of reliability using Cronbach's alpha & McDonald's omega
alpha.rma.fit_facets_HH <- rel_rma(AlphaHex_facets_HH)
omega.rma.fit_facets_HH <- rel_rma(OmegaHex_facets_HH)
Bonett_alpha.rma.fit_facets_HH <- rel_rma(BonettAlphaHex_facets_HH)
Bonett_omega.rma.fit_facets_HH <- rel_rma(BonettOmegaHex_facets_HH)

alpha.rma.fit_facets_EM <- rel_rma(AlphaHex_facets_EM)
omega.rma.fit_facets_EM <- rel_rma(OmegaHex_facets_EM)
Bonett_alpha.rma.fit_facets_EM <- rel_rma(BonettAlphaHex_facets_EM)
Bonett_omega.rma.fit_facets_EM <- rel_rma(BonettOmegaHex_facets_EM)

alpha.rma.fit_facets_EX <- rel_rma(AlphaHex_facets_EX)
omega.rma.fit_facets_EX <- rel_rma(OmegaHex_facets_EX)
Bonett_alpha.rma.fit_facets_EX <- rel_rma(BonettAlphaHex_facets_EX)
Bonett_omega.rma.fit_facets_EX <- rel_rma(BonettOmegaHex_facets_EX)

alpha.rma.fit_facets_AG <- rel_rma(AlphaHex_facets_AG)
omega.rma.fit_facets_AG <- rel_rma(OmegaHex_facets_AG)
Bonett_alpha.rma.fit_facets_AG <- rel_rma(BonettAlphaHex_facets_AG)
Bonett_omega.rma.fit_facets_AG <- rel_rma(BonettOmegaHex_facets_AG)

alpha.rma.fit_facets_CO <- rel_rma(AlphaHex_facets_CO)
omega.rma.fit_facets_CO <- rel_rma(OmegaHex_facets_CO)
Bonett_alpha.rma.fit_facets_CO <- rel_rma(BonettAlphaHex_facets_CO)
Bonett_omega.rma.fit_facets_CO <- rel_rma(BonettOmegaHex_facets_CO)

alpha.rma.fit_facets_OX <- rel_rma(AlphaHex_facets_OX)
omega.rma.fit_facets_OX <- rel_rma(OmegaHex_facets_OX)
Bonett_alpha.rma.fit_facets_OX <- rel_rma(BonettAlphaHex_facets_OX)
Bonett_omega.rma.fit_facets_OX <- rel_rma(BonettOmegaHex_facets_OX)



my_forest_plot(alpha.rma.fit_facets_HH, AlphaHex_facets_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Facet-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_facets_HH, OmegaHex_facets_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Facet-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_facets_HH, BonettAlphaHex_facets_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Facet-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_facets_HH, BonettOmegaHex_facets_HH, main.title = "Forest Plot - HEXACO Honesty-Humility \n\ Facet-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_facets_EM, AlphaHex_facets_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Facet-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_facets_EM, OmegaHex_facets_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Facet-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_facets_EM, BonettAlphaHex_facets_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Facet-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_facets_EM, BonettOmegaHex_facets_EM, main.title = "Forest Plot - HEXACO Emotionality \n\ Facet-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_facets_EX, AlphaHex_facets_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Facet-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_facets_EX, OmegaHex_facets_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Facet-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_facets_EX, BonettAlphaHex_facets_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Facet-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_facets_EX, BonettOmegaHex_facets_EX, main.title = "Forest Plot - HEXACO Extraversion \n\ Facet-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_facets_AG, AlphaHex_facets_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Facet-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_facets_AG, OmegaHex_facets_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Facet-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_facets_AG, BonettAlphaHex_facets_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Facet-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_facets_AG, BonettOmegaHex_facets_AG, main.title = "Forest Plot - HEXACO Agreeableness \n\ Facet-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_facets_CO, AlphaHex_facets_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Facet-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_facets_CO, OmegaHex_facets_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Facet-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_facets_CO, BonettAlphaHex_facets_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Facet-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_facets_CO, BonettOmegaHex_facets_CO, main.title = "Forest Plot - HEXACO Conscientiousness \n\ Facet-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)

my_forest_plot(alpha.rma.fit_facets_OX, AlphaHex_facets_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Facet-Level",
               x.lab = "Cronbach's Alpha", CI.display = TRUE)
my_forest_plot(omega.rma.fit_facets_OX, OmegaHex_facets_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Facet-Level",
               x.lab = "McDonald's Omega", CI.display = TRUE)
my_forest_plot(Bonett_alpha.rma.fit_facets_OX, BonettAlphaHex_facets_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Facet-Level",
               x.lab = "Cronbach's Alpha Bonett-transformed", CI.display = TRUE)
my_forest_plot(Bonett_omega.rma.fit_facets_OX, BonettOmegaHex_facets_OX, main.title = "Forest Plot - HEXACO Openness to Experience \n\ Facet-Level",
               x.lab = "McDonald's Omega Bonett-transformed", CI.display = TRUE)









Reg_prep <- read.csv(here("Meta-Regression/meta_regression_dat_HEXACO.csv"))



reg.alpha.rma.fit_facets_HH <- rel_rma.reg_hex(merge(AlphaHex_facets_HH, Reg_prep, by = "source"))
reg.omega.rma.fit_facets_HH <- rel_rma.reg_hex(merge(OmegaHex_facets_HH, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_facets_HH <- rel_rma.reg_hex(merge(BonettAlphaHex_facets_HH, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_facets_HH <- rel_rma.reg_hex(merge(BonettOmegaHex_facets_HH, Reg_prep, by = "source"))

reg.alpha.rma.fit_facets_EM <- rel_rma.reg_hex(merge(AlphaHex_facets_EM, Reg_prep, by = "source"))
reg.omega.rma.fit_facets_EM <- rel_rma.reg_hex(merge(OmegaHex_facets_EM, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_facets_EM <- rel_rma.reg_hex(merge(BonettAlphaHex_facets_EM, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_facets_EM <- rel_rma.reg_hex(merge(BonettOmegaHex_facets_EM, Reg_prep, by = "source"))

reg.alpha.rma.fit_facets_EX <- rel_rma.reg_hex(merge(AlphaHex_facets_EX, Reg_prep, by = "source"))
reg.omega.rma.fit_facets_EX <- rel_rma.reg_hex(merge(OmegaHex_facets_EX, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_facets_EX <- rel_rma.reg_hex(merge(BonettAlphaHex_facets_EX, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_facets_EX <- rel_rma.reg_hex(merge(BonettOmegaHex_facets_EX, Reg_prep, by = "source"))

reg.alpha.rma.fit_facets_AG <- rel_rma.reg_hex(merge(AlphaHex_facets_AG, Reg_prep, by = "source"))
reg.omega.rma.fit_facets_AG <- rel_rma.reg_hex(merge(OmegaHex_facets_AG, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_facets_AG <- rel_rma.reg_hex(merge(BonettAlphaHex_facets_AG, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_facets_AG <- rel_rma.reg_hex(merge(BonettOmegaHex_facets_AG, Reg_prep, by = "source"))

reg.alpha.rma.fit_facets_CO <- rel_rma.reg_hex(merge(AlphaHex_facets_CO, Reg_prep, by = "source"))
reg.omega.rma.fit_facets_CO <- rel_rma.reg_hex(merge(OmegaHex_facets_CO, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_facets_CO <- rel_rma.reg_hex(merge(BonettAlphaHex_facets_CO, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_facets_CO <- rel_rma.reg_hex(merge(BonettOmegaHex_facets_CO, Reg_prep, by = "source"))

reg.alpha.rma.fit_facets_OX <- rel_rma.reg_hex(merge(AlphaHex_facets_OX, Reg_prep, by = "source"))
reg.omega.rma.fit_facets_OX <- rel_rma.reg_hex(merge(OmegaHex_facets_OX, Reg_prep, by = "source"))
reg.Bonett_alpha.rma.fit_facets_OX <- rel_rma.reg_hex(merge(BonettAlphaHex_facets_OX, Reg_prep, by = "source"))
reg.Bonett_omega.rma.fit_facets_OX <- rel_rma.reg_hex(merge(BonettOmegaHex_facets_OX, Reg_prep, by = "source"))







reg.alpha.rma.fit_scorevar_facets_HH <- rma(data = merge(AlphaHex_facets_HH, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_hh)
reg.omega.rma.fit_scorevar_facets_HH <- rma(data = merge(OmegaHex_facets_HH, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_hh)
reg.Bonett_alpha.rma.fit_scorevar_facets_HH <- rma(data = merge(BonettAlphaHex_facets_HH, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_hh)
reg.Bonett_omega.rma.fit_scorevar_facets_HH <- rma(data = merge(BonettOmegaHex_facets_HH, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_hh)

reg.alpha.rma.fit_scorevar_facets_EM <- rma(data = merge(AlphaHex_facets_EM, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_em)
reg.omega.rma.fit_scorevar_facets_EM <- rma(data = merge(OmegaHex_facets_EM, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_em)
reg.Bonett_alpha.rma.fit_scorevar_facets_EM <- rma(data = merge(BonettAlphaHex_facets_EM, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_em)
reg.Bonett_omega.rma.fit_scorevar_facets_EM <- rma(data = merge(BonettOmegaHex_facets_EM, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_em)

reg.alpha.rma.fit_scorevar_facets_EX <- rma(data = merge(AlphaHex_facets_EX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_ex)
reg.omega.rma.fit_scorevar_facets_EX <- rma(data = merge(OmegaHex_facets_EX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_ex)
reg.Bonett_alpha.rma.fit_scorevar_facets_EX <- rma(data = merge(BonettAlphaHex_facets_EX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_ex)
reg.Bonett_omega.rma.fit_scorevar_facets_EX <- rma(data = merge(BonettOmegaHex_facets_EX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_ex)

reg.alpha.rma.fit_scorevar_facets_AG <- rma(data = merge(AlphaHex_facets_AG, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_ag)
reg.omega.rma.fit_scorevar_facets_AG <- rma(data = merge(OmegaHex_facets_AG, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_ag)
reg.Bonett_alpha.rma.fit_scorevar_facets_AG <- rma(data = merge(BonettAlphaHex_facets_AG, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_ag)
reg.Bonett_omega.rma.fit_scorevar_facets_AG <- rma(data = merge(BonettOmegaHex_facets_AG, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_ag)

reg.alpha.rma.fit_scorevar_facets_CO <- rma(data = merge(AlphaHex_facets_CO, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_co)
reg.omega.rma.fit_scorevar_facets_CO <- rma(data = merge(OmegaHex_facets_CO, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_co)
reg.Bonett_alpha.rma.fit_scorevar_facets_CO <- rma(data = merge(BonettAlphaHex_facets_CO, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_co)
reg.Bonett_omega.rma.fit_scorevar_facets_CO <- rma(data = merge(BonettOmegaHex_facets_CO, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_co)

reg.alpha.rma.fit_scorevar_facets_OX <- rma(data = merge(AlphaHex_facets_OX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_ox)
reg.omega.rma.fit_scorevar_facets_OX <- rma(data = merge(OmegaHex_facets_OX, Reg_prep, by = "source"), yi = reliability, 
                                     sei = StandardError, mods = ~ var_facets_ox)
reg.Bonett_alpha.rma.fit_scorevar_facets_OX <- rma(data = merge(BonettAlphaHex_facets_OX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_ox)
reg.Bonett_omega.rma.fit_scorevar_facets_OX <- rma(data = merge(BonettOmegaHex_facets_OX, Reg_prep, by = "source"), yi = reliability, 
                                            sei = StandardError, mods = ~ var_facets_ox)


