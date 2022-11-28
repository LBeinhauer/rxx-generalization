



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

source(here("RG_function-library.R"))



CVT <- seq(from = 0, to = .3, by = .1)
CVE <- seq(from = 0, to = .3, by = .1)
rel <- seq(from = .1, to = .9, by = .2)

condition_combinations <- expand.grid(CVT, CVE, rel)

names(condition_combinations) <- c("CVT", "CVE", "rel")


model <- "F =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10"

plan(multisession, workers = 7)

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
  geom_point(aes(x = tau_Theta[which(condition_combinations$CVT == 0)], 
                 y = condition_combinations$CVE[which(condition_combinations$CVT == 0)] * 
                   (1 - condition_combinations$rel[which(condition_combinations$CVT == 0)]) * 10),
             alpha = .3)



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
              alpha = .3, position = position_jitter(width = .05))


ggplot() +
  geom_point(aes(x = exp.tau_VE[condition_combinations$CVT == 0],
                 y = chi2_fit4[condition_combinations$CVT == 0] - chi2_fit3[condition_combinations$CVT == 0]),
             alpha = .3, position = position_jitter(width = .05))






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
             alpha = .3, position = position_jitter(width = .05))


ggplot() +
  geom_point(aes(x = exp.tau_VT[condition_combinations$CVE == 0],
                 y = chi2_fit4[condition_combinations$CVE == 0] - chi2_fit1.5[condition_combinations$CVE == 0]),
             alpha = .3, position = position_jitter(width = .05))







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

#saveRDS(sim_data, file = "MGCFA-MLSEM_test.RData")
sim_data <- readRDS(file = "MGCFA-MLSEM_test.RData")

it <- 0
test <- lapply(sim_data, FUN = function(x){
  
  dat <- x$data
  
  fit <- lavaan::sem(model = two.level.model, data = dat, cluster = "group",
                     std.lv = TRUE, auto.fix.first = FALSE, h1 = TRUE)
  
  it <<- it + 1
  
  cat(paste0(it, "\n"))
  return(fit)
})



further.test <- lapply(test, FUN = function(x){
  If <- lavInspect(x, what = "est")
  return(If)
  
})


lapply(test, FUN = function(x){
  If <- lavInspect(x, what = "cov.all")
  return(If)
  
})


group.theta.sum <- sapply(further.test, FUN = function(x){
  s <- sum(x$group$theta)
  return(ifelse(s != 10, s, NA))
})


group.lambda.sumsquare <- sapply(further.test, FUN = function(x){
  s <- sum(x$group$lambda^2)
  return(ifelse(s != 10, s, NA))
})



plot(abs(group.theta.sum), group.lambda.sumsquare)



exp.varE <- condition_combinations$CVE * (1-condition_combinations$rel) * 10

plot(exp.varE, group.theta.sum)
plot(exp.varE, sqrt(abs(group.theta.sum)))



exp.varT <- condition_combinations$CVT * condition_combinations$rel * 10

plot(exp.varT, sqrt(group.lambda.sumsquare))

