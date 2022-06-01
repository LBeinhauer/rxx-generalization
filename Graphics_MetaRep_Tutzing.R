### Graphics Poster Presentation MetaRep Tutzing


# library loading and installing as necessary


# relevant R packages
packages <- c("metafor", "tidyverse", "here", "data.table", "svglite")

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

source(here("Loading_Estimates_HEXACO.R"))
source(here("ReliabilityFunctions_RG.R"))


Reg_prep <- read.csv(here("Meta-Regression/meta_regression_dat_HEXACO.csv"))
SE_Alpha_varT.df <- read.csv(here("Reliability Estimates/BootstrappedSE_T_Alpha.csv"))
SE_Alpha_varE.df <- read.csv(here("Reliability Estimates/BootstrappedSE_E_Alpha.csv"))

rma.varT_Alpha_HH.fit <- rma(measure = "GEN", yi = (AlphaHex_HH$reliability * Reg_prep$var_hh), sei = SE_Alpha_varT.df[,1], method = "REML")
rma.varE_Alpha_HH.fit <- rma(measure = "GEN", yi = (Reg_prep$var_hh - AlphaHex_HH$reliability * Reg_prep$var_hh), sei = SE_Alpha_varE.df[,1], method = "REML")
rma_Alpha_HH <- rel_rma(AlphaHex_HH)


# svglite(here("Graphics/forest_varT_HH.svg"), width = 10, height = 7)
png(here("Graphics/forest_varT_HH.png"), width = 900, height = 630)
my_forest_plot(rma.varT_Alpha_HH.fit, AlphaHex_HH, main.title = paste0("Forest Plot - HEXACO HH --- I2: ", round(rma.varT_Alpha_HH.fit$I2, 2)), x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = FALSE) #+
  xlim(c(0, 0.45))
dev.off()

#svglite(here("Graphics/forest_varE_HH.svg"), width = 10, height = 7)
png(here("Graphics/forest_varE_HH.png"), width = 900, height = 630)
my_forest_plot(rma.varE_Alpha_HH.fit, AlphaHex_HH, main.title = paste0("Forest Plot - HEXACO HH --- I2: ", round(rma.varE_Alpha_HH.fit$I2, 2)), x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = FALSE) #+
  xlim(c(0, 0.45))
dev.off()

#svglite(here("Graphics/forest_reliability_HH.svg"), width = 10, height = 7)
png(here("Graphics/forest_reliability_HH.png"), width = 900, height = 630)
my_forest_plot(rma_Alpha_HH, AlphaHex_HH, main.title = paste0("Forest Plot - HEXACO HH --- I2: ", round(rma_Alpha_HH$I2, 2)), x.lab = "Reliability (Cronbach's Alpha)", ci.lvl = .975, CI.display = FALSE)
dev.off()







N <- sapply(1:length(unique(pc_df$source)), FUN = function(x){
  length(pc_df$major[which(pc_df$source == unique(pc_df$source)[x])])
})

rma_varHH <- rma(measure = "GEN", yi = Reg_prep$var_hh, sei = sqrt((2 * Reg_prep$var_hh^2) / (N-1)), method = "REML")
my_forest_plot(rma_varHH, AlphaHex_HH, main.title = paste0("Forest Plot - HEXACO HH --- I2: ", round(rma_varHH$I2, 2)), x.lab = "Observed Variance", ci.lvl = .975, CI.display = FALSE)



sqrt((2 * (AlphaHex_HH$reliability * Reg_prep$var_hh)^2) / (N-1))

sqrt((2 * (Reg_prep$var_hh - AlphaHex_HH$reliability * Reg_prep$var_hh)^2) / (N-1))

plot(SE_Alpha_varT.df[,1], sqrt((2 * (AlphaHex_HH$reliability * Reg_prep$var_hh)^2) / (N-1)))
plot(SE_Alpha_varE.df[,1], sqrt((2 * (Reg_prep$var_hh - AlphaHex_HH$reliability * Reg_prep$var_hh)^2) / (N-1)))
