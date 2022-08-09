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






sim_het_VC <- function(j, n, k, reliability = 0.5, mean_score = 0, mean_observed_var = 10, 
                       tau_var_T = 0, tau_var_E = 0){
  
  mean_var_T <- mean_observed_var * reliability
  
  mean_var_E <- mean_observed_var - mean_var_T
  
  true_var <- truncnorm::rtruncnorm(n = k, mean = mean_var_T, sd = tau_var_T, a = 0)
  
  error_var <- truncnorm::rtruncnorm(n = k, mean = mean_var_E, sd = tau_var_E, a = 0)
  
  sim_d.L <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    
    var_T1 <- true_var[x]/j
    
    var_E1 <- error_var[x]
    
    mat <- matrix(var_T1, nrow = j, ncol = j)
    
    diag(mat) <- var_T1 + var_E1
    
    obs_scores <- mvrnorm(n = n, mu = rep(0, j), Sigma = mat)
    
    sim_data <- obs_scores
    
    rel <- spsUtil::quiet(psych::alpha(sim_data, warnings = FALSE))
    
    return(list(reliability = rel$total$raw_alpha,
                StandardError = rel$total$ase,
                data = sim_data))
  })
  
  sim_data.L <- lapply(sim_d.L, FUN = function(x){x$data})
  
  sim_d.df <- data.frame(Reliability = sapply(sim_d.L, FUN = function(x){x$reliability}),
                         StandardError = sapply(sim_d.L, FUN = function(x){x$StandardError}))
  
  return(list(sim_data.L = sim_data.L,
              reliability.df = sim_d.df))
}



# takes about 17 seconds for a single run.

system.time(
  test1 <- sim_het_VC(j = 10, n = 100, k = 1000,
                      reliability = .5, mean_score = 0, 
                      mean_observed_var = 10,
                      tau_var_T = 0,
                      tau_var_E = 2)
)



plot(density(test1$reliability.df$Reliability))

hist(test1$reliability.df$Reliability)
hist(test1$reliability.df$StandardError)







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


