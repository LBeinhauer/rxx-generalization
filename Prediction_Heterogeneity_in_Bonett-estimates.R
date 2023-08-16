
library(here)

DF <- read.csv(here("Notes/vis_df.csv"), sep = "")


Large_Sim_Data <- readRDS(file = here("Notes/Sim_40000_conditions.RData"))

rma_Bonett <- lapply(1:length(Large_Sim_Data), FUN = function(it){
  
  x <- Large_Sim_Data[[it]]
  
  Bonett <- log(1-x$rel)
  var <- (2*10)/((10-1)*(100-2))
  
  rma_Bon <- metafor::rma(measure = "GEN",
                          method = "REML",
                          yi = Bonett,
                          vi = var)
  
  return(list(tau2 = rma_Bon$tau2,
              mu = rma_Bon$b[1],
              H2 = rma_Bon$H2,
              k = rma_Bon$k,
              QE = rma_Bon$QE,
              I2 = rma_Bon$I2))
  
})


tau2_Bonett <- lapply(rma_Bonett, FUN = function(x){x$tau2})

unlist(tau2_Bonett)

H2 <- unlist(lapply(rma_Bonett, FUN = function(x){x$H2}))
k <- unlist(lapply(rma_Bonett, FUN = function(x){x$k}))
Q <- unlist(lapply(rma_Bonett, FUN = function(x){x$Q}))
I2 <- unlist(lapply(rma_Bonett, FUN = function(x){x$I2}))
mu <- unlist(lapply(rma_Bonett, FUN = function(x){x$mu}))


(Q+(k-1))/(k-1)





names(DF)



mu_varT <- 10 * DF$rel
mu_varE <- 10 * (1-DF$rel)

mu_varX <- mu_varT + mu_varE


tau_varT <- DF$CVT * mu_varT
tau_varE <- DF$CVE * mu_varE
tau_varX <- sqrt(tau_varT^2 + tau_varE^2)

cov_varT_varX <- tau_varT^2 - (1/2 * tau_varE^2)

pred.tau_rel <- ((tau_varT^2)/(mu_varX^2)) + (((mu_varT^2) * (tau_varX^2))/(mu_varX^4)) - ((2*mu_varT*(tau_varT^2))/(mu_varX^3))

pred.tau_rel2 <- ((tau_varT^2)/(mu_varX^2)) + (((mu_varT^2)/(mu_varX^4))*(tau_varX^2)) - (((2*mu_varT)/(mu_varX^3))*(tau_varT^2)) 


tau_lnvarT <- sqrt(log(((tau_varT^2) / (mu_varT^2)) + 1))
tau_lnvarE <- sqrt(log(((tau_varE^2) / (mu_varE^2)) + 1))
tau_lnvarX <- sqrt(log(((tau_varX^2) / (mu_varX^2)) + 1))

cov_varT_varE <- tau_varE^2


pred.tau2_Bonett <- tau_lnvarE^2 + tau_lnvarX^2 - (2*log(1 + (cov_varT_varE/(mu_varE+mu_varX))))

pred.tau2_Bonett2 <- tau_lnvarE^2 + 
  tau_lnvarX^2 - 
  2*((tau_varE^2/((mu_varE)*(mu_varX))) +
       (((tau_varE^2) + (tau_varE*tau_varT))/(4*(mu_varE^2)*(mu_varX^2))))
  

tau2 <- unlist(tau2_Bonett)

back.transform_tau2 <- ((exp(mu)^2)*tau2) + (((exp(mu)^2) / 2) * (tau2^2))



pred.mu_untransformed <- (mu_varT/mu_varX) + ((tau_varX^2)*(mu_varT/(mu_varX^3))) - ((tau_varT^2)/(mu_varX^2))




# pred.Wi <- 1 / (((2*10)/((10-1)*(100-2))) + pred.tau2_Bonett2)

pred.Wi <- 1 / (((2*10)/((10-1)*(100-2))))

pred.nu <- (99*(pred.Wi*100)) / (((pred.Wi*100)^2) - ((pred.Wi^2)*100))


pred.H2 <- (pred.tau2_Bonett2 + pred.nu) / pred.nu


C <- (100*pred.Wi) - ((100*(pred.Wi^2)) / (100*pred.Wi))

pred.Q <- pred.tau2_Bonett2 * C + 99

pred.H2_inconsistent <- pred.Q / 99


pred.I2 <- 100 * (pred.tau2_Bonett2 / (pred.tau2_Bonett2 + pred.nu))







library(ggplot2)

ggplot() +
  geom_point(aes(x = sqrt(pred.tau_rel), y = DF$tau_rel), alpha = .1,
             position = position_jitter(width = .001)) +
  geom_abline(intercept = 0, slope = 1)


ggplot() +
  geom_point(aes(x = sqrt(pred.tau_rel), y = sqrt(back.transform_tau2)), alpha = .1,
             position = position_jitter(width = .001)) +
  geom_abline(intercept = 0, slope = 1)




ggplot() +
  geom_point(aes(x = sqrt(pred.tau2_Bonett), y = sqrt(unlist(tau2_Bonett)), 
                 colour = as.factor(DF$rel)), 
             alpha = .1,
             position = position_jitter(width = .001)) +
  geom_abline(intercept = 0, slope = 1) 


dftest <- data.frame(pred = pred.tau2_Bonett,
                     est = unlist(tau2_Bonett),
                     CVT = DF$CVT,
                     CVE = DF$CVE,
                     rel = DF$rel)

ggplot(data = dftest) +
  geom_point(aes(x = sqrt(pred), y = sqrt(est), colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .001)) +
  #geom_abline(intercept = 0, slope = 1) + 
  facet_grid(rows = vars(CVT),
             cols = vars(CVE),
             scales = "free",
             space = "free")



ggplot() +
  geom_point(aes(x = sqrt(pred.tau2_Bonett2), y = sqrt(unlist(tau2_Bonett)), 
                 colour = as.factor(DF$rel)), 
             alpha = .1,
             position = position_jitter(width = .001)) +
  geom_abline(intercept = 0, slope = 1) 



dftest2 <- data.frame(pred = pred.tau2_Bonett2,
                      est = unlist(tau2_Bonett),
                      CVT = DF$CVT,
                      CVE = DF$CVE,
                      rel = DF$rel,
                      H2 = (Q+(k-1))/(k-1),
                      H2_consistent = H2,
                      I2_consistent = I2,
                      H2_inconsistent = pred.H2_inconsistent,
                      I2 = pred.I2,
                      tau_back = sqrt(back.transform_tau2),
                      tau_untr_pred = sqrt(pred.tau_rel),
                      tau_untr_pred2 = sqrt(pred.tau_rel2))


ggplot(data = dftest2) +
  #geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = rel, y = tau_back, colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))

ggplot(data = dftest2) +
  #geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = rel, y = tau_untr_pred, colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))


ggplot(data = dftest2) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = tau_untr_pred, y = tau_back, colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))




## with 2nd-order Taylor Polynomial approximation
# 
# ggplot(data = dftest2) +
#   #geom_abline(intercept = 0, slope = 1) + 
#   geom_point(aes(x = rel, y = tau_untr_pred2, colour = as.factor(rel)), alpha = .5) +
#   facet_grid(rows = vars(CVT),
#              cols = vars(CVE))
# 
# 
# ggplot(data = dftest2) + 
#   geom_abline(intercept = 0, slope = 1) + 
#   geom_point(aes(x = tau_untr_pred2, y = tau_back, colour = as.factor(rel)), alpha = .5) +
#   facet_grid(rows = vars(CVT),
#              cols = vars(CVE))
# 


ggplot(data = dftest2) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = tau_untr_pred, y = tau_back, colour = as.factor(CVT)), alpha = .5) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel))






ggplot(data = dftest2) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = sqrt(pred), y = sqrt(est), colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))


ggplot(data = dftest2) +
  geom_point(aes(x = sqrt(pred), y = H2, colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) +
  geom_line(aes(x = sqrt(pred), y = pred.H2), linewidth = 2) + 
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))


ggplot(data = dftest2) +
  geom_point(aes(x = sqrt(pred), y = H2, colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) +
  geom_line(aes(x = sqrt(pred), y = H2_inconsistent), linewidth = 2) + 
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))


ggplot(data = dftest2) +
  geom_point(aes(x = sqrt(pred), y = H2_consistent, colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) + 
  geom_line(aes(x = sqrt(pred), y = pred.H2), linewidth = 2) + 
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))



ggplot(data = dftest2) +
  geom_point(aes(x = H2, y = H2_consistent, colour = as.factor(rel)), alpha = .5) + 
  geom_abline(intercept = 0, slope = 1) + 
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))


ggplot(data = dftest2) +
  geom_point(aes(x = sqrt(pred), y = I2_consistent, colour = as.factor(rel)), alpha = .5,
             position = position_jitter(width = .01)) + 
  geom_line(aes(x = sqrt(pred), y = pred.I2), linewidth = 2) + 
  facet_grid(rows = vars(CVT),
             cols = vars(CVE))



table(dftest2[dftest2$pred < 0, c("CVT", "CVE", "rel")])

plot(H2[DF$CVT == 0 & DF$CVE == 0])

