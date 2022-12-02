

# packages required (likely not all atm)
packages <- c("tidyverse", "here", "psych", "coefficientalpha", "MASS", "truncnorm", "spsUtil", "metafor",
              "meta", "bayesmeta", "future.apply", "lavaan")




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

# source function-script, required for data simulation
source(here("RG_function-library.R"))


# define conditions:
# coefficient of variation for true score variance
CVT <- seq(from = 0, to = .3, by = .1)
# coefficient of variation for error score variance
CVE <- seq(from = 0, to = .3, by = .1)
# reliability ranging from .1 to .9
rel <- seq(from = .1, to = .9, by = .2)

# combine conditions
condition_combinations <- expand.grid(CVT, CVE, rel)
names(condition_combinations) <- c("CVT", "CVE", "rel")


##### Data Simulation #####

##############################################################################
# Do not run if not specifically necessary - I can send you a data file.     #
# Running likely takes a while - up to an hour or longer, depending on specs #
##############################################################################

# simple factor model for evaluation
model <- "F =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10"

# use multiple cores for simulation & first analysis
plan(multisession, workers = 7)


# version 1: identify/scale by constraining latent factor variance = 1
# std.lv = TRUE, auto.fix.first = FALSE

system.time(
  sim_data <- future_lapply(1:nrow(condition_combinations), future.seed = TRUE, FUN = function(x){
    # sim_data <- lapply(1:nrow(condition_combinations), FUN = function(x){
    
    # function to simulate data (description in RG_function-library.R)
    it.simdata <- sim_het_VC(j = 10, n = 1000, k = 20,
                             reliability = condition_combinations$rel[x], mean_score = 0, 
                             mean_observed_var = 10,
                             CV_var_T = condition_combinations$CVT[x],
                             CV_var_E = condition_combinations$CVE[x])
    
    d <- NULL
    
    # combine into single data set, wide format
    for(i in seq_along(it.simdata$sim_data.L)){
      dat <- as.data.frame(it.simdata$sim_data.L[[i]]) %>%
        mutate(group = i)
      
      d <- rbind(d, dat)
    }
    
    # fit a variety of MG-CFA models, restricting mroe and more parameters.
    # tryCatch to make sure function doesn't break down under error
    tryCatch(
      {
        fit1 <- cfa(model, group = "group", data = d, std.lv = TRUE, auto.fix.first = FALSE,)
        
        fit2 <- cfa(model, group = "group", data = d, std.lv = TRUE, auto.fix.first = FALSE,
                    group.equal = c("loadings"))
        
        fit4 <- cfa(model, group = "group", data = d, std.lv = TRUE, auto.fix.first = FALSE,
                    group.equal = c("loadings", "intercepts", "residuals"))
        
        fit3 <- lavaan::cfa(model, group = "group", data = d, std.lv = TRUE, auto.fix.first = FALSE,
                            group.equal = c("loadings", "intercepts"))
        
        fit1.5 <- cfa(model, group = "group", data = d, std.lv = TRUE, auto.fix.first = FALSE,
                      group.equal = c("intercepts", "residuals"))
        
        fit2.5 <- cfa(model, group = "group", data = d, std.lv = TRUE, auto.fix.first = FALSE,
                      group.equal = c("loadings", "residuals"))
        
        # standard errors (variances) for model 3
        lIf3.vcov <- lavInspect(fit3, what = "vcov")
        # parameter estimates for model 3
        lIf3.est <- lavInspect(fit3, what = "est")
        
        # standard errors (variances) for model 1
        lIf1.vcov <- lavInspect(fit1, what = "vcov")
        # parameter estimates for model 1
        lIf1.est <- lavInspect(fit1, what = "est")
        
        # specify return object
        return(list(data = d,
                    fit1 = fit1,
                    fit2 = fit2,
                    fit3 = fit3,
                    fit4 = fit4,
                    fit1.5 = fit1.5,
                    fit2.5 = fit2.5,
                    lIf3.vcov = lIf3.vcov,
                    lIf3.est = lIf3.est,
                    lIf1.vcov = lIf1.vcov,
                    lIf1.est = lIf1.est
        ))
      }
      
      ,
      error = function(e)(cat("ERROR: ", conditionMessage(e)))
    )
    
    
    
  })
)


#saveRDS(sim_data, file = "MGCFA-MLSEM_test.RData")
sim_data <- readRDS(file = "MGCFA-MLSEM_test.RData")



# std.lv = FALSE, auto.fix.first = TRUE

system.time(
  sim_data <- future_lapply(1:nrow(condition_combinations), future.seed = TRUE, FUN = function(x){
    # sim_data <- lapply(1:nrow(condition_combinations), FUN = function(x){
    it.simdata <- sim_het_VC(j = 10, n = 1000, k = 20,
                             reliability = condition_combinations$rel[x], mean_score = 0, 
                             mean_observed_var = 10,
                             CV_var_T = condition_combinations$CVT[x],
                             CV_var_E = condition_combinations$CVE[x])
    
    d <- NULL
    
    for(i in seq_along(it.simdata$sim_data.L)){
      dat <- as.data.frame(it.simdata$sim_data.L[[i]]) %>%
        mutate(group = i)
      
      d <- rbind(d, dat)
    }
    
    tryCatch(
      {
        fit1 <- cfa(model, group = "group", data = d, std.lv = FALSE, auto.fix.first = TRUE,)
        
        fit2 <- cfa(model, group = "group", data = d, std.lv = FALSE, auto.fix.first = TRUE,
                    group.equal = c("loadings"))
        
        fit4 <- cfa(model, group = "group", data = d, std.lv = FALSE, auto.fix.first = TRUE,
                    group.equal = c("loadings", "intercepts", "residuals"))
        
        fit3 <- lavaan::cfa(model, group = "group", data = d, std.lv = FALSE, auto.fix.first = TRUE,
                            group.equal = c("loadings", "intercepts"))
        
        fit1.5 <- cfa(model, group = "group", data = d, std.lv = FALSE, auto.fix.first = TRUE,
                      group.equal = c("intercepts", "residuals"))
        
        fit2.5 <- cfa(model, group = "group", data = d, std.lv = FALSE, auto.fix.first = TRUE,
                      group.equal = c("loadings", "residuals"))
        
        # fit4 <- lavaan::cfa(model, group = "group", data = d, std.lv = TRUE, auto.fix.first = TRUE,
        #                     group.equal = c("loadings", "intercepts", "residuals"))
        # 
        
        lIf3.vcov <- lavInspect(fit3, what = "vcov")
        lIf3.est <- lavInspect(fit3, what = "est")
        
        lIf1.vcov <- lavInspect(fit1, what = "vcov")
        lIf1.est <- lavInspect(fit1, what = "est")
        
        return(list(data = d,
                    fit1 = fit1,
                    fit2 = fit2,
                    fit3 = fit3,
                    fit4 = fit4,
                    fit1.5 = fit1.5,
                    fit2.5 = fit2.5,
                    lIf3.vcov = lIf3.vcov,
                    lIf3.est = lIf3.est,
                    lIf1.vcov = lIf1.vcov,
                    lIf1.est = lIf1.est
        ))
      }
      
      ,
      error = function(e)(cat("ERROR: ", conditionMessage(e)))
    )
    
    
    
  })
)

# saveRDS(sim_data, file = "MGCFA-MLSEM_test_scaleloading.RData")
sim_data <- readRDS(file = "MGCFA-MLSEM_test_scaleloading.RData")


# free up cores
plan(sequential)




theta <- lapply(sim_data, function(x){
  
  lIf3.vcov <- x$lIf3.vcov
  lIf3.est <- x$lIf3.est
  
  
  lIf3.vcov.theta.L <- lapply(1:20, FUN = function(x){
    if(x == 1){
      covmat <- lIf3.vcov[colnames(lIf3.vcov)  %in% sprintf("V%i~~V%i", 1:20, 1:20),
                          rownames(lIf3.vcov)  %in% sprintf("V%i~~V%i", 1:20, 1:20)] 
    }else{
      covmat <- lIf3.vcov[colnames(lIf3.vcov)  %in% paste0(sprintf("V%i~~V%i", 1:20, 1:20), ".g", x),
                          colnames(lIf3.vcov)  %in% paste0(sprintf("V%i~~V%i", 1:20, 1:20), ".g", x)]
    }
  })
  
  lIf3.est.ThetaSE <- sapply(lIf3.vcov.theta.L, FUN = function(x){(1/100) * sqrt(sum(diag(x)^2))})
  
  
  lIf3.est.Theta.L <- lapply(lIf3.est, FUN = function(x){x$theta})
  lIf3.est.Theta <- sapply(lIf3.est.Theta.L, FUN = function(x){
    1/(10^2) * sum(diag(x))
  })
  
  return(list(thetaSE = lIf3.est.ThetaSE,
              theta = lIf3.est.Theta))
  
})


tau_Theta <- sapply(theta, FUN = function(x){
  
  
  num <- tryCatch(
    sqrt(metafor::rma(yi = x$theta, sei = x$thetaSE)$tau2),
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), num, NA)
  
})


plot(tau_Theta, condition_combinations$CVE * (1 - condition_combinations$rel) * 10)


ggplot() +
  geom_point(aes(x = condition_combinations$CVE * (1 - condition_combinations$rel) * 10,
                 y = tau_Theta),
             alpha = .3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey") +
  labs(title = "Scatter tau_sim - tau_theta", 
       x = "tau_sim",
       y = "tau_theta") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey"))


ggplot() +
  geom_point(aes(x = tau_Theta[which(condition_combinations$CVT == 0)], 
                 y = condition_combinations$CVE[which(condition_combinations$CVT == 0)] * 
                   (1 - condition_combinations$rel[which(condition_combinations$CVT == 0)]) * 10),
             alpha = .3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey") +
  labs(title = "Scatter tau_sim - tau_theta (CVT = 0)", 
       x = "tau_sim",
       y = "tau_theta") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))



chi2_fit4 <- sapply(sim_data, FUN = function(x){
  num <- tryCatch(
    lavInspect(x$fit4, what = "test")$standard$stat,
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), yes = num, no = NA)
})

chi2_fit3 <- sapply(sim_data, FUN = function(x){
  
  num <- tryCatch(
    lavInspect(x$fit3, what = "test")$standard$stat,
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), yes = num, no = NA)
})


plot(condition_combinations$CVE * (1-condition_combinations$rel) * 10,
     chi2_fit4 - chi2_fit3)


exp.tau_VE <- condition_combinations$CVE * (1-condition_combinations$rel) * 10



ggplot() +
  geom_point(aes(x = exp.tau_VE, 
                 y = chi2_fit4 - chi2_fit3),
             alpha = .3,
             position = position_jitter(width = .025)) +
  labs(title = "Scatter tau_sim - Chi2-diff", 
       subtitle = "model 1: fixed loadings & intercepts, \nmodel 0: fixed loadings, intercepts, res. variances",
       x = "tau_sim",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))


ggplot() +
  geom_point(aes(x = exp.tau_VE[condition_combinations$CVT == 0],
                 y = chi2_fit4[condition_combinations$CVT == 0] - chi2_fit3[condition_combinations$CVT == 0]),
             alpha = .3, position = position_jitter(width = .05))



ggplot() +
  geom_point(aes(x = exp.tau_VE[condition_combinations$CVT == 0], 
                 y = chi2_fit4[condition_combinations$CVT == 0] - chi2_fit3[condition_combinations$CVT == 0]),
             alpha = .3,
             position = position_jitter(width = .025)) +
  labs(title = "Scatter tau_sim - Chi2-diff (CVT = 0)", 
       subtitle = "model 1: fixed loadings & intercepts, \nmodel 0: fixed loadings, intercepts, res. variances",
       x = "tau_sim",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))




chi2_fit1 <- sapply(sim_data, FUN = function(x){
  
  num <- tryCatch(
    lavInspect(x$fit1, what = "test")$standard$stat,
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), yes = num, no = NA)
})



chi2_fit2 <- sapply(sim_data, FUN = function(x){
  
  num <- tryCatch(
    lavInspect(x$fit2, what = "test")$standard$stat,
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), yes = num, no = NA)
})



chi2_fit2 - chi2_fit1

exp.tau_VT <- condition_combinations$CVT * condition_combinations$rel * 10



lambda <- lapply(sim_data, function(x){
  
  lIf1.vcov <- x$lIf1.vcov
  lIf1.est <- x$lIf1.est
  
  
  lIf1.vcov.lambda.L <- lapply(1:20, FUN = function(x){
    if(x == 1){
      covmat <- lIf1.vcov[colnames(lIf1.vcov)  %in% sprintf("F=~V%i", 1:20),
                          rownames(lIf1.vcov)  %in% sprintf("F=~V%i", 1:20)] 
    }else{
      covmat <- lIf1.vcov[colnames(lIf1.vcov)  %in% paste0(sprintf("F=~V%i", 1:20), ".g", x),
                          colnames(lIf1.vcov)  %in% paste0(sprintf("F=~V%i", 1:20), ".g", x)]
    }
  })
  
  lIf1.est.LambdaSE <- sapply(lIf1.vcov.lambda.L, FUN = function(x){sqrt(sum(diag(x)^2))})
  
  
  lIf1.est.Lambda.L <- lapply(lIf1.est, FUN = function(x){x$lambda})
  lIf1.est.Lambda <- sapply(lIf1.est.Lambda.L, FUN = function(x){
    sum(x^2)/10
  })
  
  return(list(lambdaSE = lIf1.est.LambdaSE,
              lambda = lIf1.est.Lambda))
  
})


tau_Lambda <- sapply(lambda, FUN = function(x){
  
  
  num <- tryCatch(
    sqrt(metafor::rma(yi = x$lambda, sei = x$lambdaSE)$tau2),
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), num, NA)
  
})


plot(tau_Lambda, condition_combinations$CVT * condition_combinations$rel * 10)



ggplot() +
  geom_point(aes(x = exp.tau_VT,
                 y = tau_Lambda),
             alpha = .3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey") +
  labs(title = "Scatter tau_sim - tau_lambda", 
       subtitle = "model: no parameters fixed",
       x = "tau_lambda",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))


ggplot() +
  geom_point(aes(x = exp.tau_VT[condition_combinations$CVE == 0],
                 y = tau_Lambda[condition_combinations$CVE == 0]),
             alpha = .3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey") +
  labs(title = "Scatter tau_sim - tau_lambda (CVE = 0)", 
       subtitle = "model: no parameters fixed",
       x = "tau_lambda",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))




ggplot() +
  geom_point(aes(x = exp.tau_VT, 
                 y = chi2_fit2 - chi2_fit1),
             alpha = .3,
             position = position_jitter(width = .025)) +
  labs(title = "Scatter tau_sim - Chi2-diff", 
       subtitle = "model 1: nothing fixed, \nmodel 0: fixed loadings",
       x = "tau_sim",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))



ggplot() +
  geom_point(aes(x = exp.tau_VT[condition_combinations$CVE == 0], 
                 y = (chi2_fit2 - chi2_fit1)[condition_combinations$CVE == 0]),
             alpha = .3,
             position = position_jitter(width = .025)) +
  labs(title = "Scatter tau_sim - Chi2-diff (CVE = 0)", 
       subtitle = "model 1: nothing fixed, \nmodel 0: fixed loadings",
       x = "tau_sim",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))


chi2_fit1.5 <- sapply(sim_data, FUN = function(x){
  
  num <- tryCatch(
    lavInspect(x$fit1.5, what = "test")$standard$stat,
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), yes = num, no = NA)
})


plot(condition_combinations$CVT * (condition_combinations$rel) * 10,
     chi2_fit4 - chi2_fit1.5)


exp.tau_VT <- condition_combinations$CVT * (condition_combinations$rel) * 10


ggplot() +
  geom_point(aes(x = exp.tau_VT, 
                 y = chi2_fit4 - chi2_fit1.5),
             alpha = .3,
             position = position_jitter(width = .025)) +
  labs(title = "Scatter tau_sim - Chi2-diff", 
       subtitle = "model 1: fixed intercepts, res. variances, \nmodel 0: fixed intercepts, loadings, res. variances",
       x = "tau_sim",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))



ggplot() +
  geom_point(aes(x = exp.tau_VT[condition_combinations$CVE == 0], 
                 y = (chi2_fit4 - chi2_fit1.5)[condition_combinations$CVE == 0]),
             alpha = .3,
             position = position_jitter(width = .025)) +
  labs(title = "Scatter tau_sim - Chi2-diff (CVE = 0)", 
       subtitle = "model 1: fixed intercepts, res. variances, \nmodel 0: fixed intercepts, loadings, res. variances",
       x = "tau_sim",
       y = "Chi2-diff") +
  theme(panel.grid.major.x = element_line("grey"),
        panel.grid.major.y = element_line("grey"),
        panel.grid.minor.x = element_line("lightgrey"),
        panel.grid.minor.y = element_line("lightgrey"),
        panel.background = element_rect("transparent"),
        axis.line.x = element_line("grey"),
        axis.line.y = element_line("grey"),
        axis.ticks = element_line("grey")) +
  scale_x_continuous(breaks = c(0, 1, 2))







chi2_fit2.5 <- sapply(sim_data, FUN = function(x){
  
  num <- tryCatch(
    lavInspect(x$fit2.5, what = "test")$standard$stat,
    
    error = function(e)(cat("ERROR: ", conditionMessage(e)))
  )
  
  ifelse(is.numeric(num), yes = num, no = NA)
})


plot(condition_combinations$CVT * (condition_combinations$rel) * 10,
     chi2_fit4 - chi2_fit2.5)


exp.tau_VT <- condition_combinations$CVT * (condition_combinations$rel) * 10

ggplot() +
  geom_point(aes(x = exp.tau_VT,
                 y = chi2_fit4 - chi2_fit2.5),
             alpha = .3, position = position_jitter(width = .05))


ggplot() +
  geom_point(aes(x = exp.tau_VT[condition_combinations$CVE == 0],
                 y = chi2_fit4[condition_combinations$CVE == 0] - chi2_fit2.5[condition_combinations$CVE == 0]),
             alpha = .3, position = position_jitter(width = .05))







# specify two-level model for Multilevel SEM in lavaan
# same measurement model on both levels.
two.level.model <- "
level: 1
FW =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10

level: 2
FB =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10
"


two.level.model <- "
level: 1
FW =~ V1 + V2 + V3

level: 2
FB =~ V1 + V2 + V3
"

fit <- cfa(model = two.level.model, data = sim_data[[1]]$data, cluster = "group",
           std.lv = TRUE, auto.fix.first = FALSE)

summary(fit)


# iterator to count through iteration of lapply
it <- 0

# apply ML-SEM using lavaan to previously simulated data
test <- lapply(sim_data, FUN = function(x){
  
  dat <- x$data
  
  # fit model
  fit <- lavaan::sem(model = two.level.model, data = dat, cluster = "group",
                     std.lv = TRUE, auto.fix.first = FALSE, h1 = TRUE)
  
  it <<- it + 1
  
  # return iteration
  cat(paste0(it, "\n"))
  return(fit)
})




further.test <- lapply(test, FUN = function(x){
  If <- lavInspect(x, what = "est")
  return(If)
  
})


# collect individual estimates of theta or lambda, within and  group

# theta (item residual variance) on group-level
ft.theta.group <- lapply(further.test, FUN = function(x){
  diag(x$group$theta)
})
# theta (item residual variance) on within-level
ft.theta.within <- lapply(further.test, FUN = function(x){
  diag(x$within$theta)
})

# lambda (factor loading / shared sqrt(variation)) on group-level
ft.lambda.group <- lapply(further.test, FUN = function(x){
  (x$group$lambda)
})
# lambda (factor loading / shared sqrt(variation)) on group-level
ft.lambda.within <- lapply(further.test, FUN = function(x){
  (x$within$lambda)
})


lapply(test, FUN = function(x){
  If <- lavInspect(x, what = "cov.all")
  return(If)
  
})



# aggregate individual estimates to what we would ususally deem
#  true and error score variance

# theta - residual variance equivalent to error score variance:
# sum individual estimates and divide by 100 (error variance
# averages out across items)
within.theta.sum <- sapply(further.test, FUN = function(x){
  s <- sum(diag(x$within$theta))/100
  return(ifelse(s != .1, s, NA))
})
group.theta.sum <- sapply(further.test, FUN = function(x){
  s <- sum(diag(x$group$theta)/100)
  return(ifelse(s != .1, s, NA))
})


plot(group.theta.sum)


# lmabda - factor loadings, with correct scaling by restricting
#  latent factor variance to 1, we can sum up the squared loadings 
#  to arrive at the true score variance. Since we are dealing with
#  mean total scores, we also need to divide the sum by the number
#  of items (10)
within.lambda.sumsquare <- sapply(further.test, FUN = function(x){
  s <- sum(x$within$lambda^2)/10
  return(ifelse(s != 1, s, NA))
})
group.lambda.sumsquare <- sapply(further.test, FUN = function(x){
  s <- sum(x$group$lambda^2)/10
  return(ifelse(s != 1, s, NA))
})


# notably, estimates at the within-level estimate the true and 
# error score variance reasonably well. At the group-level, 
# however, the estimates don't seem to make a lot of sense

plot(group.lambda.sumsquare)

# interestingly, estimates of true and (abs.) error score variance seem
#  to correlate stronly, no matter the actually induced heterogeneity
plot(abs(group.theta.sum), group.lambda.sumsquare)



exp.varE <- condition_combinations$CVE * (1-condition_combinations$rel) * 10

plot(exp.varE, group.theta.sum)
plot(exp.varE, sqrt(abs(group.theta.sum)))



exp.varT <- condition_combinations$CVT * condition_combinations$rel * 10

plot(exp.varT, sqrt(group.lambda.sumsquare))

