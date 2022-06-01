### Reliability Generalization HEXACO - Meta-Regression of Error & True Variance ###

## 26/05/2022




###################################################################################################
# This script is used for meta-analysis of the reliability estimates generated previously.        #
###################################################################################################


# library loading and installing as necessary


# relevant R packages
packages <- c("metafor", "tidyverse", "here", "data.table", "lavaan", "boot", "haven")

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






ML1_dat <- read_spss("C:/Users/beinhaul/Documents/GitHub/Heterogeneity-and-Generalizability/Data/Data Summaries/1. Original Data/ML1/ML1CleanedDataset.sav")

##### Flag Priming - Carter #####

fp_df <- data.frame(flagdv1 = ML1_dat$flagdv1, 
                    flagdv2 = ML1_dat$flagdv2, 
                    flagdv3 = ML1_dat$flagdv3, 
                    flagdv4 = ML1_dat$flagdv4, 
                    flagdv5 = ML1_dat$flagdv5, 
                    flagdv6 = ML1_dat$flagdv6, 
                    flagdv7 = ML1_dat$flagdv7, 
                    flagdv8 = ML1_dat$flagdv8, 
                    flagdv_mean = ML1_dat$flagdv,
                    source = ML1_dat$referrer,
                    DV = ML1_dat$flagdv,
                    factor = ML1_dat$flagGroup,
                    filter = ML1_dat$flagfilter) %>%
  mutate(source = as.factor(source)) %>%
  filter(filter == 1, !is.na(DV))


est_fp <- rel_extractor(fp_df, item.vec = c(1:8), scale_info = "FlagPriming")



SE_Alpha_varT_fp <- sapply(unique(fp_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(fp_df[fp_df$source == x, c(1:8)]),
                 statistic = bootstrap_SE_varT,
                 stat = "ALPHA",
                 R = 1000)
  
  return(c(sd(bvarTs$t), mean(bvarTs$t)))
}) %>%
  t() 

SE_Alpha_varE_fp <- sapply(unique(fp_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(fp_df[fp_df$source == x, c(1:8)]),
                 statistic = bootstrap_SE_varE,
                 stat = "ALPHA",
                 R = 1000)
  
  return(c(sd(bvarTs$t), mean(bvarTs$t)))
}) %>%
  t() 


o.var_fp <- fp_df %>%
  group_by(source) %>%
  summarise(var_fp = var(DV))
  


rma.varT_fp.fit <- rma(measure = "GEN", yi = (est_fp$reliability * o.var_fp$var_fp), sei = SE_Alpha_varT_fp[,1], method = "REML")
rma.varE_fp.fit <- rma(measure = "GEN", yi = o.var_fp$var_fp - (est_fp$reliability * o.var_fp$var_fp), sei = SE_Alpha_varE_fp[,1], method = "REML")

pT_fp <- my_forest_plot(rma.varT_fp.fit, est_fp, main.title = paste0("Forest Plot - Flag Priming --- I2: ", round(rma.varT_fp.fit$I2, 2)), x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_fp <- my_forest_plot(rma.varE_fp.fit, est_fp, main.title = paste0("Forest Plot - Flag Priming --- I2: ", round(rma.varE_fp.fit$I2, 2)), x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_fp, pE_fp)





SE_Alpha_varET_fp <- sapply(unique(fp_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(fp_df[fp_df$source == x, c(1:8)]),
                 statistic = bootstrap_SE_varET,
                 stat = "ALPHA",
                 R = 1000)
  
  return(c(sd(bvarTs$t), mean(bvarTs$t)))
}) %>%
  t() 

rma.varET_fp.fit <- rma(measure = "GEN", yi = (o.var_fp$var_fp - (est_fp$reliability * o.var_fp$var_fp))/((est_fp$reliability * o.var_fp$var_fp)), sei = SE_Alpha_varE_fp[,1], method = "REML")

my_forest_plot(rma.varET_fp.fit, est_fp, main.title = paste0("Forest Plot - Flag Priming --- I2: ", round(rma.varET_fp.fit$I2, 2)), x.lab = "Ratio of Error to True variance (Alpha)", ci.lvl = .975, CI.display = TRUE)









##### Currency Priming #####


cp_df <- data.frame(sysjust1 = ML1_dat$sysjust1,
                    sysjust2 = ML1_dat$sysjust2,
                    sysjust3 = ML1_dat$sysjust3,
                    sysjust4 = ML1_dat$sysjust4,
                    sysjust5 = ML1_dat$sysjust5,
                    sysjust6 = ML1_dat$sysjust6,
                    sysjust7 = ML1_dat$sysjust7,
                    sysjust8 = ML1_dat$sysjust8,
                    DV = ML1_dat$Sysjust,
                    factor = ML1_dat$MoneyGroup,
                    source = ML1_dat$referrer) %>%
  mutate(source = as.factor(source)) %>%
  filter(!is.na(DV))

est_cp <- rel_extractor(cp_df, item.vec = c(1:8), scale_info = "CurrencyPriming")



SE_Alpha_varT_cp <- sapply(unique(cp_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(cp_df[cp_df$source == x, c(1:8)]),
                 statistic = bootstrap_SE_varT,
                 stat = "ALPHA",
                 R = 1000)
  
  return(c(sd(bvarTs$t), mean(bvarTs$t)))
}) %>%
  t() 

SE_Alpha_varE_cp <- sapply(unique(cp_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(cp_df[cp_df$source == x, c(1:8)]),
                 statistic = bootstrap_SE_varE,
                 stat = "ALPHA",
                 R = 1000)
  
  return(c(sd(bvarTs$t), mean(bvarTs$t)))
}) %>%
  t() 


o.var_cp <- cp_df %>%
  group_by(source) %>%
  summarise(var_cp = var(DV))



rma.varT_cp.fit <- rma(measure = "GEN", yi = (est_cp$reliability * o.var_cp$var_cp), sei = SE_Alpha_varT_cp[,1], method = "REML")
rma.varE_cp.fit <- rma(measure = "GEN", yi = o.var_cp$var_cp - (est_cp$reliability * o.var_cp$var_cp), sei = SE_Alpha_varE_cp[,1], method = "REML")

pT_cp <- my_forest_plot(rma.varT_cp.fit, est_cp, main.title = paste0("Forest Plot - Currency Priming --- I2: ", round(rma.varT_cp.fit$I2, 2)), x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_cp <- my_forest_plot(rma.varE_cp.fit, est_cp, main.title = paste0("Forest Plot - Currency Priming --- I2: ", round(rma.varE_cp.fit$I2, 2)), x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_cp, pE_cp)





SE_Alpha_varET_cp <- sapply(unique(cp_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(cp_df[cp_df$source == x, c(1:8)]),
                 statistic = bootstrap_SE_varET,
                 stat = "ALPHA",
                 R = 1000)
  
  return(c(sd(bvarTs$t), mean(bvarTs$t)))
}) %>%
  t() 

rma.varET_cp.fit <- rma(measure = "GEN", yi = (o.var_cp$var_cp - (est_cp$reliability * o.var_cp$var_cp))/((est_cp$reliability * o.var_cp$var_cp)), sei = SE_Alpha_varE_cp[,1], method = "REML")

my_forest_plot(rma.varET_cp.fit, est_cp, main.title = paste0("Forest Plot - Currency Priming --- I2: ", round(rma.varET_cp.fit$I2, 2)), x.lab = "Ratio of Error to True variance (Alpha)", ci.lvl = .975, CI.display = TRUE)



