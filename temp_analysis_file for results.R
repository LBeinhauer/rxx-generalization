


summary(alpha_estimates.list[[1]]$Reliability)


Alpha_rma.list$Cacioppo_Need_Cognition


Bonett.Alpha_rma.list$Cacioppo_Need_Cognition


1 - exp(Bonett.Alpha_rma.list$Cacioppo_Need_Cognition$b[1])
1 - exp(Bonett.Alpha_rma.list$Cacioppo_Need_Cognition$ci.lb)
1 - exp(Bonett.Alpha_rma.list$Cacioppo_Need_Cognition$ci.ub)




summary(omega_estimates.list[[1]]$Reliability)

Omega_rma.list$Cacioppo_Need_Cognition

Bonett.Omega_rma.list$Cacioppo_Need_Cognition

1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$b[1])
1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$ci.lb)
1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$ci.ub)



varT_rma.list$Cacioppo_Need_Cognition$yi
sqrt(varT_rma.list$Cacioppo_Need_Cognition$vi)

summary(varT_rma.list$Cacioppo_Need_Cognition$yi)

varE_rma.list$Cacioppo_Need_Cognition$yi
sqrt(varE_rma.list$Cacioppo_Need_Cognition$vi)

summary(varE_rma.list$Cacioppo_Need_Cognition$yi)



varT_rma.list$Cacioppo_Need_Cognition$yi + varE_rma.list$Cacioppo_Need_Cognition$yi
varX_rma.list$Cacioppo_Need_Cognition$yi


# varT_yi & varE_yi currently bootstrapped means -> keep or change?
plot(varT_rma.list$Cacioppo_Need_Cognition$yi + varE_rma.list$Cacioppo_Need_Cognition$yi,
     varX_rma.list$Cacioppo_Need_Cognition$yi)


varT_rma.list$Cacioppo_Need_Cognition
varE_rma.list$Cacioppo_Need_Cognition









# Shnabel sense of power




summary(alpha_estimates.list[[8]]$Reliability)
summary(alpha_estimates.list[[9]]$Reliability)


Alpha_rma.list$Shnabel_Power_Sense_Rev
Bonett.Alpha_rma.list$Shnabel_Power_Sense_Rev


1 - exp(Bonett.Alpha_rma.list$Shnabel_Power_Sense_Rev$b[1])
1 - exp(Bonett.Alpha_rma.list$Shnabel_Power_Sense_Rev$ci.lb)
1 - exp(Bonett.Alpha_rma.list$Shnabel_Power_Sense_Rev$ci.ub)




Alpha_rma.list$Shnabel_Power_Sense_RPP
Bonett.Alpha_rma.list$Shnabel_Power_Sense_RPP


1 - exp(Bonett.Alpha_rma.list$Shnabel_Power_Sense_RPP$b[1])
1 - exp(Bonett.Alpha_rma.list$Shnabel_Power_Sense_RPP$ci.lb)
1 - exp(Bonett.Alpha_rma.list$Shnabel_Power_Sense_RPP$ci.ub)


summary(omega_estimates.list[[1]]$Reliability)

Omega_rma.list$Cacioppo_Need_Cognition

Bonett.Omega_rma.list$Cacioppo_Need_Cognition

1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$b[1])
1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$ci.lb)
1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$ci.ub)



varT_rma.list$Cacioppo_Need_Cognition$yi
sqrt(varT_rma.list$Cacioppo_Need_Cognition$vi)

summary(varT_rma.list$Cacioppo_Need_Cognition$yi)

varE_rma.list$Cacioppo_Need_Cognition$yi
sqrt(varE_rma.list$Cacioppo_Need_Cognition$vi)

summary(varE_rma.list$Cacioppo_Need_Cognition$yi)



varT_rma.list$Cacioppo_Need_Cognition$yi + varE_rma.list$Cacioppo_Need_Cognition$yi
varX_rma.list$Cacioppo_Need_Cognition$yi


# varT_yi & varE_yi currently bootstrapped means -> keep or change?
plot(varT_rma.list$Cacioppo_Need_Cognition$yi + varE_rma.list$Cacioppo_Need_Cognition$yi,
     varX_rma.list$Cacioppo_Need_Cognition$yi)


varT_rma.list$Cacioppo_Need_Cognition
varE_rma.list$Cacioppo_Need_Cognition



plot(sapply(varT_rma.list, FUN = function(x){x$I2}),
     sapply(lnvarT_rma.list, FUN = function(x){x$I2}))

plot(sapply(varE_rma.list, FUN = function(x){x$I2}),
     sapply(lnvarE_rma.list, FUN = function(x){x$I2}))



vars1 <- data.list[[1]] %>% 
  mutate(avg = rowMeans(data.list[[1]][,-ncol(data.list[[1]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars1$var, breaks = 20)  
hist(vars1$lnvar, breaks = 20)
  


vars2 <- data.list[[2]] %>% 
  mutate(avg = rowMeans(data.list[[2]][,-ncol(data.list[[2]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars2$var, breaks = 20)  
hist(vars2$lnvar, breaks = 20)  


vars3 <- data.list[[3]] %>% 
  mutate(avg = rowMeans(data.list[[3]][,-ncol(data.list[[3]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars3$var, breaks = 20)  
hist(vars3$lnvar, breaks = 20)  


vars4 <- data.list[[4]] %>% 
  mutate(avg = rowMeans(data.list[[4]][,-ncol(data.list[[4]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars4$var, breaks = 20)  
hist(vars4$lnvar, breaks = 20)  


vars5 <- data.list[[5]] %>% 
  mutate(avg = rowMeans(data.list[[5]][,-ncol(data.list[[5]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars5$var, breaks = 20)  
hist(vars5$lnvar, breaks = 20)  



vars6 <- data.list[[6]] %>% 
  mutate(avg = rowMeans(data.list[[6]][,-ncol(data.list[[6]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars6$var, breaks = 20)  
hist(vars6$lnvar, breaks = 20)  



vars7 <- data.list[[7]] %>% 
  mutate(avg = rowMeans(data.list[[7]][,-ncol(data.list[[7]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars7$var, breaks = 20)  
hist(vars7$lnvar, breaks = 20)  


vars8 <- data.list[[8]] %>% 
  mutate(avg = rowMeans(data.list[[8]][,-ncol(data.list[[8]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars8$var, breaks = 5)  
hist(vars8$lnvar, breaks = 5)  



vars9 <- data.list[[9]] %>% 
  mutate(avg = rowMeans(data.list[[9]][,-ncol(data.list[[9]])])) %>% 
  group_by(source) %>% 
  summarise(var = var(avg, na.rm = T),
            lnvar = log(var(avg, na.rm = T)),
            sd = sd(avg, na.rm = T),
            lnsd = log(sd(avg, na.rm = T)))


hist(vars9$var, breaks = 10)  
hist(vars9$lnvar, breaks = 10)  








### Simulation data, check equivalence of rel vs ln(1-rel) approach


Large_Sim_Data <- readRDS(file = here("Notes/Sim_40000_conditions.RData"))

rel.L <- lapply(Large_Sim_Data, FUN = function(x){
  
  x$rel
  
})

CVT <- seq(from = 0, to = .3, by = .1)
CVE <- seq(from = 0, to = .3, by = .1)
rel <- seq(from = .1, to = .9, by = .2)

# combine 4*4*5 conditions
condition_combinations <- expand.grid(CVT, CVE, rel)
names(condition_combinations) <- c("CVT", "CVE", "rel")

rel_tau_Bonett.L <- lapply(rel.L, FUN = function(x){
  
  Bonett_transf <- log(1 - x)
  V_Bonett_transf <- rep((2*10)/((10-1)*(100-2)), 100)
  
  sqrt(metafor::rma(yi = Bonett_transf
                    , vi = V_Bonett_transf
                    , measure = "GEN"
                    , method = "REML")$tau2)
  
})

rel_I2_Bonett.L <- lapply(rel.L, FUN = function(x){
  
  Bonett_transf <- log(1 - x)
  V_Bonett_transf <- rep((2*10)/((10-1)*(100-2)), 100)
  
  (metafor::rma(yi = Bonett_transf
                , vi = V_Bonett_transf
                , measure = "GEN"
                , method = "REML")$I2)
  
})

rel_I2_normal.L <- lapply(Large_Sim_Data, FUN = function(x){
  
  tryCatch({
    metafor::rma(yi = x$rel, sei = x$ase
                 , measure = "GEN"
                 , method = "REML")$I2
    
  },
  
  error = function(e)(cat(conditionMessage(e)))) 
  
})

rel_tau_Bonett.df <- data.frame(tau_rel_B = sapply(rel_tau_Bonett.L, FUN = function(x){x})
                                , I2_rel_B = sapply(rel_I2_Bonett.L, FUN = function(x){x})
                                , I2_rel = sapply(rel_I2_normal.L, FUN = function(x){ifelse(is.numeric(x), x, NA)})
                                , CVT = rep(condition_combinations$CVT, 500)
                                , CVE = rep(condition_combinations$CVE, 500)
                                , rel = rep(condition_combinations$rel, 500))

plot(rel_tau_Bonett.df$tau_rel_B, vis.df$tau_rel)
plot(rel_tau_Bonett.df$I2_rel_B, rel_tau_Bonett.df$I2_rel)
