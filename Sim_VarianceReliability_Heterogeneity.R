# Simulation




packages <- c("tidyverse", "here", "psych", "coefficientalpha", "boot", "MASS", "truncnorm", "spsUtil")

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


 ## Heterogeneity in True Variance

# takes about 17 seconds for a single run of 1000 samples.

system.time(
  test1 <- sim_het_VC(j = 10, n = 100, k = 100,
                      reliability = .5, mean_score = 0, 
                      mean_observed_var = 10,
                      CV_var_T = .3,
                      CV_var_E = 0,
                      tau_var_T = 0,
                      tau_var_E = 0)
)



# takes about 2.5 minutes to decompose variance & generate bootstrapped SE for 100(!) samples 
#  (with 100 bootstrapped samples each)

system.time(
  long_test_T <- apply_Bootstrap_SE_nonspecific(test1$sim_data.L, var.component = "TRUE", R = 100)
)

system.time(
  long_test_E <- apply_Bootstrap_SE_nonspecific(test1$sim_data.L, var.component = "ERROR", R = 100)
)


metafor::rma(measure = "GEN", method = "REML", yi = boot.mean, sei = SE, data = long_test_T)

metafor::rma(measure = "GEN", method = "REML", yi = boot.mean, sei = SE, data = long_test_E)


saveRDS(long_test_T, file = here("Notes/bootstrapped_varT_sim.RData"))
saveRDS(long_test_E, file = here("Notes/bootstrapped_varE_sim.RData"))


hist(long_test_T$boot.mean)
hist(long_test_E$boot.mean)

mean(long_test_T$boot.mean)
mean(long_test_E$boot.mean)

sd(long_test_T$boot.mean)
sd(long_test_E$boot.mean)

mean(long_test_T$SE)
mean(long_test_E$SE)




plot(density(test1$reliability.df$Reliability))

hist(test1$reliability.df$Reliability)
hist(test1$reliability.df$StandardError)






## Heterogeneity in Error Variance


# takes about 17 seconds for a single run of 1000 samples.

system.time(
  test2 <- sim_het_VC(j = 10, n = 1000, k = 1000,
                      reliability = .5, mean_score = 0, 
                      mean_observed_var = 10,
                      tau_var_T = 0,
                      tau_var_E = 1)
)



# takes about 2.5 minutes to decompose variance & generate bootstrapped SE for 100(!) samples 
#  (with 100 bootstrapped samples each)

system.time(
  long_test_T <- apply_Bootstrap_SE_nonspecific(test2$sim_data.L, var.component = "TRUE", R = 100)
)

system.time(
  long_test_E <- apply_Bootstrap_SE_nonspecific(test2$sim_data.L, var.component = "ERROR", R = 100)
)


metafor::rma(measure = "GEN", method = "REML", yi = boot.mean, sei = SE, data = long_test_T)

metafor::rma(measure = "GEN", method = "REML", yi = boot.mean, sei = SE, data = long_test_E)


hist(long_test_T$boot.mean)
hist(long_test_E$boot.mean)

mean(long_test_T$boot.mean)
mean(long_test_E$boot.mean)

sd(long_test_T$boot.mean)
sd(long_test_E$boot.mean)

mean(long_test_T$SE)
mean(long_test_E$SE)






# simulation scheme:

# reliability .1 to .9

# CV_T 0 to .5
# CV_E 0 to .5

CVT <- seq(from = 0, to = .3, by = .1)
CVE <- seq(from = 0, to = .3, by = .1)
rel <- seq(from = .1, to = .9, by = .2)

condition_combinations <- expand.grid(CVT, CVE, rel)

names(condition_combinations) <- c("CVT", "CVE", "rel")


# Large-Scale Simulation Scheme

Large_Sim_Data <- lapply(1:nrow(condition_combinations), FUN = function(x){
  it.simdata <- sim_het_VC(j = 10, n = 100, k = 100,
                           reliability = condition_combinations$rel[x], mean_score = 0, 
                           mean_observed_var = 10,
                           CV_var_T = condition_combinations$CVT[x],
                           CV_var_E = condition_combinations$CVE[x])
  
  b.data_T <- apply_Bootstrap_SE_nonspecific(it.simdata$sim_data.L, var.component = "TRUE", R = 100)
  b.data_E <- apply_Bootstrap_SE_nonspecific(it.simdata$sim_data.L, var.component = "ERROR", R = 100)
  
  return(data.frame(varT = b.data_T$boot.mean,
                    SE_T = b.data_T$SE,
                    varE = b.data_E$boot.mean,
                    SE_E = b.data_E$SE))
  
})




# 
# (1 - tr(C)/sum(C)) * (j / (j-1))
# 
# rxx <- rnorm(100, mean = .7, sd = .1)
# 
# # Cronbach's Alpha as: rxx = (j / (j-1)) * (1 - (A / A + B))
# # assuming constant item variance 1, varying covariance & j = 10 ->
# # rxx = (10/9) * (1 - (10 / 10 + B))
# # this leads to:
# 
# A = 10
# 
# B = (10 / (-(rxx/(10/9)) + 1)) - 10
# 
# cov_jh = B / 90


