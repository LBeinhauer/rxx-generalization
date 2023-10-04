


packages <- c("dplyr", "here", "magrittr")

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




# DF <- read.csv(here("Notes/vis_df.csv"), sep = "")


# Large_Sim_Data <- readRDS(file = here("Simulation Data/Sim_80000_conditions.RData"))

# rma_Bonett <- readRDS(file = "Simulation Data/Sim_80000_conditions_rmaBonett.RData")

DF_rma <- read.csv(here("Notes/Sim80000_rma.csv"))

DF_rma$tau_T2_alt <- DF_rma$tau_X^2 - DF_rma$tau_E^2
DF_rma$tau_T_alt <- sqrt(DF_rma$tau_T2_alt)

DF_rma$mu_T_alt <- DF_rma$mu_X - DF_rma$mu_E

# Formulating functions to back-transform the estimated heterogeneity in ln(1-r_xx)
var_Bonnett_backtransformed <- function(mean_x, var_x){
  (((-exp(mean_x))^2) * var_x) + (.5*((-exp(mean_x))^2)*(var_x^2)) + ((-exp(mean_x)) * (-exp(mean_x)) * (var_x^2))
}

mean_Bonnett_backtransformed <- function(mean_x, var_x){
  1 - exp(mean_x) + ((-exp(mean_x)) / 2) * var_x
}


df_rma <- DF_rma %>% 
  mutate(tau_rel_transf = sqrt(var_Bonnett_backtransformed(mean_x = mu_Bonnett, 
                                                      var_x = tau_Bonnett^2)),
         tau_rel_Botella_transf = sqrt(var_Bonnett_backtransformed(mean_x = mu_Bonnett_rel_Botella,
                                                              var_x = tau_Bonett_rel_Botella^2)),
         mu_rel_transf = mean_Bonnett_backtransformed(mean_x = mu_Bonnett,
                                                      var_x = tau_Bonnett^2),
         mu_rel_Botella_transf = mean_Bonnett_backtransformed(mean_x = mu_Bonnett_rel_Botella,
                                                              var_x = tau_Bonett_rel_Botella^2))


CVT <- seq(from = 0, to = .3, by = .1)
CVE <- seq(from = 0, to = .3, by = .1)
rel <- seq(from = .5, to = .9, by = .1)

# combine 4*4*5 conditions
condition_combinations <- expand.grid(CVT, CVE, rel)
names(condition_combinations) <- c("CVT", "CVE", "rel")

# repeat 80 conditions, each 500 times
all_conditions <- NULL
# Large-Scale Simulation Scheme
for(i in 1:1000){
  all_conditions <- rbind(all_conditions, condition_combinations)
}

# prepare data.frame containing simulation parameters and predicted values
df_pred <- data.frame(all_conditions) %>% 
  
  # compute mean true and error score variance
  mutate(mu_varT = 10 * all_conditions$rel,
         mu_varE = 10 * (1 - all_conditions$rel)) %>%
  mutate(mu_varX = mu_varT + mu_varE) %>% 
  
  # compute heterogeneity in score variance components, induced in simulation
  mutate(tau_varT = CVT * mu_varT,
         tau_varE = CVE * mu_varE) %>% 
  mutate(tau_varX = sqrt(tau_varT^2 + tau_varE^2)) %>% 
  
  # compute predicted heterogeneity in score reliability r_xx
  mutate(pred.tau_rel = sqrt(((tau_varT^2)/(mu_varX^2)) + (((mu_varT^2) * (tau_varX^2))/(mu_varX^4)) - 
           ((2*mu_varT*(tau_varT^2))/(mu_varX^3)))) %>% 
  
  # compute predicted meta-analytic estimate in score reliability r_xx
  mutate(pred.mu_rel = (mu_varT/mu_varX) + ((tau_varX^2)*(mu_varT/(mu_varX^3))) - ((tau_varT^2)/(mu_varX^2))) %>% 
  
  # compute predicted coefficient of variation for score reliability r_xx
  mutate(pred.CVrel = pred.tau_rel/pred.mu_rel) %>% 
  
  # compute predicted heterogeneity in log-transformed score variance components
  mutate(tau_lnvarT = sqrt(log(((tau_varT^2) / (mu_varT^2)) + 1)),
         tau_lnvarE = sqrt(log(((tau_varE^2) / (mu_varE^2)) + 1)),
         tau_lnvarX = sqrt(log(((tau_varX^2) / (mu_varX^2)) + 1))) %>% 
  
  # compute predicted heterogeneity in Bonnett-transformed score reliability
  mutate(pred.tau2_Bonett2 = tau_lnvarE^2 + tau_lnvarX^2 - 
           2*((tau_varE^2/((mu_varE)*(mu_varX))) - 
                (((tau_varE^2)*(tau_varX^2))/(4*(mu_varE^2)*(mu_varX^2))))) %>% 
  
  # compute predicted weights for a fixed-effects meta-analysis of ln[1-r_xx]
  mutate(pred.Wi = 1 / (((2*10)/((10-1)*(100-2))))) %>% 
  
  # computed predicted nu ("typical within-study variance of observed effect sizes or outcomes
  #  equation 9 in Higgins & Thompson, 2002)
  mutate(pred.nu = (99*(pred.Wi*100)) / (((pred.Wi*100)^2) - ((pred.Wi^2)*100))) %>% 
  
  # compute predicted H^2 (consistent with tau^2)
  mutate(pred.H2 = (pred.tau2_Bonett2 + pred.nu) / pred.nu) %>%
  
  # compute C-statistic, required for Q, to predict the H^2 inconsistent with tau^2, using the DL-estimator
  mutate(C = (100*pred.Wi) - ((100*(pred.Wi^2)) / (100*pred.Wi))) %>% 
  mutate(pred.Q = pred.tau2_Bonett2 * C + 99) %>% 
  mutate(pred.H2_inconsistent = pred.Q / 99) %>% 
  
  # compute predicted I^2 (consistent version)
  mutate(pred.I2 = 100 * (pred.tau2_Bonett2 / (pred.tau2_Bonett2 + pred.nu)))



# preparing a data.frame, containing all relevant values required for a comparison/assessment of methods
df_comparison <- data.frame(df_rma,
                            df_pred) %>% 
  
  # compute estimated coefficients of variation
  mutate(est.CVT = tau_T/mu_T,
         est.CVT_alt = tau_T_alt/mu_T_alt,
         est.CVE = tau_E/mu_E,
         est.CVrel = tau_rel_transf/mu_rel_transf,
         est.CVrel_Botella = tau_rel_Botella_transf/mu_rel_Botella_transf) %>% 
  
  # compute bias for meta-analytic estimates and estimates of heterogeneity
  mutate(bias_mu_rel = mu_rel_transf - pred.mu_rel,
         bias_mu_rel_Botella = mu_rel_Botella_transf - pred.mu_rel,
         bias_tau_rel = tau_rel_transf - pred.tau_rel,
         bias_tau_rel_Botella = tau_rel_Botella_transf - pred.tau_rel,
         bias_mu_varT = mu_T - mu_varT,
         bias_mu_varT_alt = mu_T_alt - mu_varT,
         bias_mu_varE = mu_E - mu_varE,
         bias_tau_varT = tau_T - tau_varT,
         bias_tau_varT_alt = tau_T_alt - tau_varT,
         bias_tau_varE = tau_E - tau_varE,
         bias_est.CVT = est.CVT - CVT,
         bias_est.CVT_alt = est.CVT_alt - CVT,
         bias_est.CVE = est.CVE - CVE,
         bias_est.CVrel = est.CVrel - pred.CVrel,
         bias_est.CVrel_Botella = est.CVrel_Botella - pred.CVrel)




write.csv(df_comparison, here("Notes/Sim80000_rma_df.csv"),
          row.names = FALSE)

