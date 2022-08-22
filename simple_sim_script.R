

library(spsUtil)

library(psych)

library(boot)

source("C:/Users/beinhaul/Documents/GitHub/rxx-generalization/RG_function-library.R")



rel <-  .5

obs_var <- 10

true_var <- obs_var*rel

error_var <- obs_var*(1-rel)

n <- 100

j <- 10

k <- 100




lat_scores <- rnorm(n, mean = 0, sd = sqrt(true_var))

obs_scores.M <- apply(as.matrix(1:j), MARGIN = 1, FUN = function(x){
  obs_scores <- lat_scores + rnorm(n, mean = 0, sd = sqrt(error_var * j))
})


cov(obs_scores.M)
psych::alpha(obs_scores.M)



e_vars <- rnorm(k, mean = error_var, sd = 0)
t_vars <- rnorm(k, mean = true_var, sd = 1)

obs_scores_k.L <- lapply(1:k, FUN = function(i){
  lat_scores <- rnorm(n, mean = 0, sd = sqrt(t_vars[i]))
  obs_scores.M <- apply(as.matrix(1:j), MARGIN = 1, FUN = function(x){
    obs_scores <- lat_scores + rnorm(n, mean = 0, sd = sqrt(e_vars[i] * j))
  })
  return(obs_scores.M)
})


alpha_df.L <- lapply(obs_scores_k.L, FUN = function(x){
  alph <- spsUtil::quiet(psych::alpha(x))
  var_X <- var(rowMeans(x))
  var_T <- var_X * alph$total$raw_alpha
  var_E <- var_X * (1 - alph$total$raw_alpha)
  return(data.frame(alpha = alph$total$raw_alpha, 
                    ase = alph$total$ase,
                    var_X = var_X,
                    var_T = var_T,
                    var_E = var_E))
})

alpha.df <- data.frame(alpha = sapply(alpha_df.L, FUN = function(x){x$alpha}),
                       ase = sapply(alpha_df.L, FUN = function(x){x$ase}),
                       var_X = sapply(alpha_df.L, FUN = function(x){x$var_X}),
                       var_T = sapply(alpha_df.L, FUN = function(x){x$var_T}),
                       var_E = sapply(alpha_df.L, FUN = function(x){x$var_E}))

metafor::rma(measure = "GEN", method = "REML", yi = alpha, sei = ase, data = alpha.df)



var_T.df <- apply_Bootstrap_SE_nonspecific(obs_scores_k.L, var.component = "TRUE", R = 100)
var_E.df <- apply_Bootstrap_SE_nonspecific(obs_scores_k.L, var.component = "ERROR", R = 100)


metafor::rma(measure = "GEN", method = "REML", yi = alpha.df$var_T, sei = var_T.df$SE)
metafor::rma(measure = "GEN", method = "REML", yi = alpha.df$var_E, sei = var_E.df$SE)



