# Simulation




packages <- c("tidyverse", "here", "psych", "coefficientalpha", "boot", "MASS", "truncnorm")

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






# nr of items
j <- 10

# sample size
n <- 100

# nr of samples
k <- 1000

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






j <- 10

true_var <- 5

error_var <- 5

var_T1 <- true_var/(j)
var_E1 <- error_var

mat <- matrix(var_T1, nrow = j, ncol = j)

diag(mat) <- var_T1 + var_E1

obs_scores <- mvrnorm(n = n, mu = rep(0, j), Sigma = mat)




# no heterogeneity

test.L <- apply(matrix(1:k), MARGIN = 1, FUN = function(x){
  
  obs_scores <- mvrnorm(n = n, mu = rep(0, j), Sigma = mat)
  
  sim_data <- obs_scores
  
  test <- psych::alpha(sim_data)
  
  return(data.frame(reliability = test$total$raw_alpha,
                    StandardError = test$total$ase))
})

test.df <- data.frame(Reliability = rep(NA, 1000),
                      StandardError = rep(NA, 1000))

for(i in seq_along(test.L)){
  test.df[i,1] <- test.L[[i]]$reliability
  test.df[i,2] <- test.L[[i]]$StandardError
}


# doesn't find any heterogeneity
metafor::rma(measure = "GEN", method = "REML", data = test.df,
             yi = Reliability, sei = StandardError)





true_var <- truncnorm::rtruncnorm(k, mean = 5, sd = 2, a = 0)

error_var <- 5

test.L2 <- apply(matrix(1:k), MARGIN = 1, FUN = function(x){
  
  var_T1 <- true_var[x]/(j)
  
  var_E1 <- error_var
  
  mat <- matrix(var_T1, nrow = j, ncol = j)
  
  diag(mat) <- var_T1 + var_E1
  
  obs_scores <- mvrnorm(n = n, mu = rep(0, j), Sigma = mat)
  
  sim_data <- obs_scores
  
  test <- psych::alpha(sim_data)
  
  return(data.frame(reliability = test$total$raw_alpha,
                    StandardError = test$total$ase))
})

test.df2 <- data.frame(Reliability = rep(NA, 1000),
                      StandardError = rep(NA, 1000))

for(i in seq_along(test.L2)){
  test.df2[i,1] <- test.L2[[i]]$reliability
  test.df2[i,2] <- test.L2[[i]]$StandardError
}


# does find heterogeneity
metafor::rma(measure = "GEN", method = "REML", data = test.df2,
             yi = Reliability, sei = StandardError)









true_var <- 5

error_var <- truncnorm::rtruncnorm(k, mean = 5, sd = 2, a = 0)

test.L3 <- apply(matrix(1:k), MARGIN = 1, FUN = function(x){
  
  var_T1 <- true_var/j
  
  var_E1 <- error_var[x]
  
  mat <- matrix(var_T1, nrow = j, ncol = j)
  
  diag(mat) <- var_T1 + var_E1
  
  obs_scores <- mvrnorm(n = n, mu = rep(0, j), Sigma = mat)
  
  sim_data <- obs_scores
  
  test <- psych::alpha(sim_data)
  
  return(data.frame(reliability = test$total$raw_alpha,
                    StandardError = test$total$ase))
})

test.df3 <- data.frame(Reliability = rep(NA, 1000),
                       StandardError = rep(NA, 1000))

for(i in seq_along(test.L3)){
  test.df3[i,1] <- test.L3[[i]]$reliability
  test.df3[i,2] <- test.L3[[i]]$StandardError
}


# does NOT find heterogeneity
metafor::rma(measure = "GEN", method = "REML", data = test.df3,
             yi = Reliability, sei = StandardError)



# dat <- apply(as.matrix(true_scores), MARGIN = 1, FUN = function(x){
#   e <- rnorm(n, mean = 0, sd = sqrt(error_var))
#   
#   x + e
# }) 
# 
# sim_data <- t(dat)

# sim_data <- obs_scores
# 
# t <- psych::alpha(sim_data)
# 
# C <- cov(sim_data)
# (1 - tr(C)/sum(C)) * (j / (j-1))
# 
# cor(sim_data)
# 
# var(rowSums(sim_data))
# j^2*var(rowMeans(sim_data))
# 


