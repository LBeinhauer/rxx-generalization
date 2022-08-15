# Simulation




packages <- c("tidyverse", "here", "psych", "coefficientalpha", "boot", "MASS", "truncnorm", "spsUtil", "metafor")

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
                      CV_var_T = .2,
                      CV_var_E = .1,
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



# extracting covariance matrices (used for simulation)

cov_matrices <- lapply(1:nrow(condition_combinations), FUN = function(x){
  
  mean_var_T <- 10 * condition_combinations$rel[x]
  
  mean_var_E <- 10 - mean_var_T
  
  tau_var_T <- mean_var_T * condition_combinations$CVT[x]
  tau_var_E <- mean_var_E * condition_combinations$CVE[x]
  
  true_var <- truncnorm::rtruncnorm(n = 100, mean = mean_var_T, sd = tau_var_T, a = 0)
  
  error_var <- truncnorm::rtruncnorm(n = 100, mean = mean_var_E, sd = tau_var_E, a = 0)
  
  mats <- lapply(matrix(1:100), FUN = function(x){
    var_T1 <- true_var[x]
    
    var_E1 <- error_var[x]*10
    
    mat <- matrix(var_T1, nrow = 10, ncol = 10)
    diag(mat) <- var_T1 + var_E1
    
    return(mat)
  })
  
  return(list(mats = mats, 
              true_var = true_var,
              error_var = error_var))
  
})


cov_matrices[[1]]$mats


saveRDS(Large_Sim_Data, file = here("Notes/Sim_80_conditions.RData"))

Large_Sim_Data <- readRDS(file = here("Notes/Sim_80_conditions.RData"))

Large_Sim_Data[[1]]


Large_Sim_Data_RMA <- lapply(Large_Sim_Data, FUN = function(x){
  
  
  tauT <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = varT, sei = SE_T)
  tauE <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = varE, sei = SE_E)
  
  return(data.frame(tau_T = sqrt(tauT$tau2),
                    tau_E = sqrt(tauE$tau2),
                    p_T = sqrt(tauT$QEp),
                    p_E = sqrt(tauE$QEp)))
})

vis.df <- data.frame(condition_combinations,
                     tau_T = sapply(Large_Sim_Data_RMA, FUN = function(x){x$tau_T}),
                     tau_E = sapply(Large_Sim_Data_RMA, FUN = function(x){x$tau_E}),
                     p_T = sapply(Large_Sim_Data_RMA, FUN = function(x){x$p_T}),
                     p_E = sapply(Large_Sim_Data_RMA, FUN = function(x){x$p_E}))



ggplot(data = vis.df) + 
  geom_point(aes(y = tau_T, x = CVT)) + 
  geom_line(aes(y = tau_T, x = CVT)) +
  facet_grid(rows = vars(CVE), cols = vars(rel))


ggplot(data = vis.df) + 
  geom_point(aes(y = tau_E, x = CVE)) + 
  geom_line(aes(y = tau_E, x = CVE)) +
  facet_grid(rows = vars(CVT), cols = vars(rel))


ggplot(data = vis.df) + 
  geom_point(aes(y = tau_E, x = CVE), colour = "red") + 
  geom_line(aes(y = tau_E, x = CVE), colour = "red") +
  geom_point(aes(y = tau_T, x = CVT), colour = "blue") + 
  geom_line(aes(y = tau_T, x = CVT), colour = "blue") +
  facet_grid(cols = vars(rel)) +
  labs(y = "tau", x = "CV")



MSE_T <- mean((vis.df$tau_T - (vis.df$CVT * (vis.df$rel*10)))^2)
bias_T <- mean(vis.df$tau_T) - mean(vis.df$CVT * (vis.df$rel*10))

(vis.df$tau_T - (vis.df$CVT * (vis.df$rel*10)))^2

plot((vis.df$tau_T - (vis.df$CVT * (vis.df$rel*10))))

plot(vis.df$CVT * 10*vis.df$rel, vis.df$tau_T)

plot(vis.df$CVE * (10 - 10*vis.df$rel), vis.df$tau_E)


mean((vis.df[which(vis.df$CVE == 0),]$tau_T - (vis.df[which(vis.df$CVE == 0),]$CVT * (vis.df[which(vis.df$CVE == 0),]$rel*10))))
mean((vis.df[which(vis.df$CVE == 0),]$tau_T - (vis.df[which(vis.df$CVE == 0),]$CVT * (vis.df[which(vis.df$CVE == 0),]$rel*10)))^2)

mean((vis.df[which(vis.df$CVT == 0),]$tau_T - (vis.df[which(vis.df$CVT == 0),]$CVT * (vis.df[which(vis.df$CVT == 0),]$rel*10))))
mean((vis.df[which(vis.df$CVT == 0),]$tau_T - (vis.df[which(vis.df$CVT == 0),]$CVT * (vis.df[which(vis.df$CVT == 0),]$rel*10)))^2)


MSE_E <- mean((vis.df$tau_E - (vis.df$CVE * (10 - (vis.df$rel*10))))^2)
bias_E <- mean((vis.df$tau_E - (vis.df$CVE * (10 - (vis.df$rel*10)))))

mean((vis.df$tau_E - (vis.df$CVE * (10 - (vis.df$rel*10)))))

plot((vis.df$tau_T - (vis.df$CVT * (vis.df$rel*10))))
plot((vis.df$tau_E - (vis.df$CVE * (10 - (vis.df$rel*10)))))

plot(vis.df$p_T, (vis.df$CVT * (vis.df$rel*10)))

plot(vis.df$p_E, (vis.df$CVE * (10 - (vis.df$rel*10))))


ggplot() + 
  geom_point(aes(x = vis.df$p_T, y = (vis.df$CVT * (vis.df$rel*10))), alpha = .3) +
  geom_vline(xintercept = .05)

mean(vis.df$p_T[which(vis.df$CVT > 0)] < .05)
mean(vis.df$p_T[which(vis.df$CVT == 0)] < .05)

mean(vis.df$p_E[which(vis.df$CVE > 0)] < .05)
mean(vis.df$p_E[which(vis.df$CVE == 0)] < .05)


ggplot() + 
  geom_point(aes(x = vis.df$p_E, y = (vis.df$CVE * (10 - (vis.df$rel*10)))), alpha = .3) +
  geom_vline(xintercept = .05)


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


