## visualising simulation data



packages <- c("tidyverse", "here", "psych")

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




Large_Sim_Data <- readRDS(file = here("Notes/Sim_80000_conditions.RData"))


Large_Sim_Data_RMA <- lapply(Large_Sim_Data, FUN = function(x){
  
  tryCatch({
    tauT <- metafor::rma(measure = "GEN", method = "PM", data = x, yi = varT, sei = SE_T.b)
    tauE <- metafor::rma(measure = "GEN", method = "PM", data = x, yi = varE, sei = SE_E.b)
    
    return(data.frame(tau_T = sqrt(tauT$tau2),
                      tau_E = sqrt(tauE$tau2),
                      p_T = tauT$QEp,
                      p_E = tauE$QEp))
  },
  error = function(e)(cat("ERROR: ", conditionMessage(e)))
  
  )
})


Large_Sim_Data_RMA.meta <- lapply(Large_Sim_Data, FUN = function(x){
  
  tauT <- meta::metagen(TE = x$varT, seTE = x$SE_T.b)
  tauE <- meta::metagen(TE = x$varE, seTE = x$SE_E.b)
  
  return(data.frame(tau_T = (tauT$tau),
                    tau_E = (tauE$tau),
                    p_T = tauT$pval.Q,
                    p_E = tauE$pval.Q,
                    tau_T.lower = tauT$lower.tau,
                    tau_T.upper = tauT$upper.tau,
                    tau_E.lower = tauE$lower.tau,
                    tau_E.upper = tauE$upper.tau))
})


vis.df <- data.frame(all_conditions,
                     tau_T = sapply(Large_Sim_Data_RMA, FUN = function(x){ifelse(is.numeric(x$tau_T), x$tau_T, NA)}),
                     tau_E = sapply(Large_Sim_Data_RMA, FUN = function(x){ifelse(is.numeric(x$tau_E), x$tau_E, NA)}),
                     p_T = sapply(Large_Sim_Data_RMA, FUN = function(x){ifelse(is.numeric(x$p_T), x$p_T, NA)}),
                     p_E = sapply(Large_Sim_Data_RMA, FUN = function(x){ifelse(is.numeric(x$p_E), x$p_T, NA)}))

vis.df.meta <- data.frame(condition_combinations,
                          tau_T = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$tau_T}),
                          tau_E = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$tau_E}),
                          p_T = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$p_T}),
                          p_E = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$p_E}),
                          tau_T.lower = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$tau_T.lower}),
                          tau_T.upper = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$tau_T.upper}),
                          tau_E.lower = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$tau_E.lower}),
                          tau_E.upper = sapply(Large_Sim_Data_RMA.meta, FUN = function(x){x$tau_E.upper}))

Large_Sim_data_RMA_rel <- lapply(Large_Sim_Data, FUN = function(x){
  tryCatch({
    tauT <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = varT, sei = SE_T.b)
    tauE <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = varE, sei = SE_E.b)
    taurel <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = rel, sei = ase)
    taurel_corT <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = rel, sei = ase,
                                mods = ~ varT)
    taurel_corE <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = rel, sei = ase,
                                mods = ~ varE)
    taurel_corTE <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = rel, sei = ase,
                                 mods = ~ varT + varE)
    
    return(data.frame(tau_T = sqrt(tauT$tau2),
                      tau_E = sqrt(tauE$tau2),
                      tau_rel = sqrt(taurel$tau2),
                      p_T = tauT$QEp,
                      p_E = tauE$QEp,
                      p_tau = taurel$QEp,
                      var_T.MAE = tauT$b[1],
                      var_E.MAE = tauE$b[1],
                      rel.MAE = taurel$b[1],
                      tau_rel_cT = sqrt(taurel_corT$tau2),
                      tau_rel_cE = sqrt(taurel_corE$tau2),
                      tau_rel_cTE = sqrt(taurel_corTE$tau2))
    )
  },
  error = function(e)(cat("ERROR: ", conditionMessage(e)))
  
  )
})

vis.df2 <- data.frame(all_conditions,
                      varT.M = sapply(Large_Sim_Data, FUN = function(x){mean(x$varT)}),
                      SE_T.M = sapply(Large_Sim_Data, FUN = function(x){mean(x$SE_T.b)}),
                      varE.M = sapply(Large_Sim_Data, FUN = function(x){mean(x$varE)}),
                      SE_E.M = sapply(Large_Sim_Data, FUN = function(x){mean(x$SE_E.b)}),
                      rel.M = sapply(Large_Sim_Data, FUN = function(x){mean(x$rel)}),
                      ase.M = sapply(Large_Sim_Data, FUN = function(x){mean(x$ase)}),
                      varT.MAE = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$var_T.MAE), x$var_T.MAE, NA)}),
                      varE.MAE = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$var_E.MAE), x$var_E.MAE, NA)}),
                      rel.MAE = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$rel.MAE), x$rel.MAE, NA)}),
                      tau_T = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$tau_T), x$tau_T, NA)}),
                      tau_E = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$tau_E), x$tau_E, NA)}),
                      tau_rel = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$tau_rel), x$tau_rel, NA)}),
                      p_T = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$p_T), x$p_T, NA)}),
                      p_E = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$p_E), x$p_E, NA)}),
                      p_rel = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$p_rel), x$p_rel, NA)}),
                      tau_rel_cT = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$tau_rel_cT), x$tau_rel_cT, NA)}),
                      tau_rel_cE = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$tau_rel_cE), x$tau_rel_cE, NA)}),
                      tau_rel_cTE = sapply(Large_Sim_data_RMA_rel, FUN = function(x){ifelse(is.numeric(x$tau_rel_cTE), x$tau_rel_cTE, NA)})
)


write.table(vis.df2, here("Notes/vis_df.csv"), row.names = FALSE)


test <- metafor::rma(data = Large_Sim_Data[[1]], yi = varT, sei = SE_T.b, measure = "GEN", method = "REML")
test_meta <- meta::metagen(TE = Large_Sim_Data[[1]]$varT, seTE = Large_Sim_Data[[1]]$SE_T.b)
test_bayes <- bayesmeta::bayesmeta()


test_meta$tau2 + test_meta$se.tau2 * 1.96

(test_meta$upper.tau2 - test_meta$tau2)/1.96
test_meta$se.tau2

test_meta$lower.tau
test_meta$upper.tau

(test_meta$upper.tau - test_meta$tau)/1.96


sqrt(test$tau2)
sqrt(test$se.tau2)

(test$tau2)
(test$se.tau2)


sapply(Large_Sim_Data, FUN = function(x){mean(x$SE_T)})
sapply(Large_Sim_Data, FUN = function(x){mean(x$varT)})

sapply(Large_Sim_Data, FUN = function(x){sd(x$SE_T)})
sapply(Large_Sim_Data, FUN = function(x){sd(x$varT)})

plot(sapply(Large_Sim_Data, FUN = function(x){mean(x$SE_T)}))
plot(sapply(Large_Sim_Data, FUN = function(x){mean(x$varT)}))
plot(sapply(Large_Sim_Data, FUN = function(x){sd(x$varT)}))



sapply(Large_Sim_Data, FUN = function(x){mean(x$SE_E)})
sapply(Large_Sim_Data, FUN = function(x){mean(x$varE)})

sapply(Large_Sim_Data, FUN = function(x){sd(x$SE_E)})
sapply(Large_Sim_Data, FUN = function(x){sd(x$varE)})

plot(sapply(Large_Sim_Data, FUN = function(x){mean(x$SE_E)}))
plot(sapply(Large_Sim_Data, FUN = function(x){mean(x$varE)}))
plot(sapply(Large_Sim_Data, FUN = function(x){sd(x$varE)}))



pdf(here("Notes/Est_varT.pdf"))

for(i in seq_along(Large_Sim_Data)){
  plot(Large_Sim_Data[[i]]$varT)
  abline(h = condition_combinations$rel[i] * 10)
}

dev.off()
pdf(here("Notes/Est_varE.pdf"))


for(i in seq_along(Large_Sim_Data)){
  plot(Large_Sim_Data[[i]]$varE)
  abline(h = (1 - condition_combinations$rel[i]) * 10)
}

dev.off()




# Visualization for simulated data - true variance heterogeneity

# initialize pdf
pdf(here("Notes/Facet_grid_tauT.pdf"))

# summarize simulation-data
vis.df_summarized <- vis.df2 %>% 
  group_by(CVT, CVE, rel) %>%
  summarise(T_m = mean(tau_T, na.rm = T),
            E_m = mean(tau_E, na.rm = T),
            T_sd = sd(tau_T, na.rm = T),
            E_sd = sd(tau_E, na.rm = T),
            T_ll = quantile(tau_T, .025, na.rm = T),
            T_ul = quantile(tau_T, .975, na.rm = T),
            E_ll = quantile(tau_E, .025, na.rm = T),
            E_ul = quantile(tau_E, .975, na.rm = T),
            VT.MAE_m = mean(varT.MAE, na.rm = T),
            VE.MAE_m = mean(varE.MAE, na.rm = T),
            VT.MAE_ll = quantile(varT.MAE, .025, na.rm = T),
            VT.MAE_ul = quantile(varT.MAE, .975, na.rm = T),
            VE.MAE_ll = quantile(varE.MAE, .025, na.rm = T),
            VE.MAE_ul = quantile(varE.MAE, .975, na.rm = T),
            VT.M_m = mean(varT.M, na.rm = T),
            VE.M_m = mean(varE.M, na.rm = T),
            VT.M_ll = quantile(varT.M, .025, na.rm = T),
            VT.M_ul = quantile(varT.M, .975, na.rm = T),
            VE.M_ll = quantile(varE.M, .025, na.rm = T),
            VE.M_ul = quantile(varE.M, .975, na.rm = T),
            rel.M_m = mean(rel.M, na.rm = T),
            rel.M_ll = quantile(rel.M, .025, na.rm = T),
            rel.M_ul = quantile(rel.M, .975, na.rm = T),
            rel.MAE_m = mean(rel.MAE, na.rm = T),
            rel.MAE_ll = quantile(rel.MAE, .025, na.rm = T),
            rel.MAE_ul = quantile(rel.MAE, .975, na.rm = T),
            tau_rel_m = mean(tau_rel, na.rm = T),
            tau_rel_ll = quantile(tau_rel, .025, na.rm = T),
            tau_rel_ul = quantile(tau_rel, .975, na.rm = T),
            tau_rel_cT_m = mean(tau_rel_cT, na.rm = T),
            tau_rel_cT_ll = quantile(tau_rel_cT, .025, na.rm = T),
            tau_rel_cT_ul = quantile(tau_rel_cT, .975, na.rm = T),
            tau_rel_cE_m = mean(tau_rel_cE, na.rm = T),
            tau_rel_cE_ll = quantile(tau_rel_cE, .025, na.rm = T),
            tau_rel_cE_ul = quantile(tau_rel_cE, .975, na.rm = T),
            tau_rel_cTE_m = mean(tau_rel_cTE, na.rm = T),
            tau_rel_cTE_ll = quantile(tau_rel_cTE, .025, na.rm = T),
            tau_rel_cTE_ul = quantile(tau_rel_cTE, .975, na.rm = T),
  )


# visualize summarized simulation-data + uncertainty ribbon
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = CVT*(10*rel), x = CVT), colour = "grey") +
  geom_line(aes(y = CVT*(10*rel), x = CVT), colour = "grey") +
  geom_ribbon(aes(x = CVT, ymin = T_ll, ymax = T_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = T_m, x = CVT)) + 
  geom_line(aes(y = T_m, x = CVT)) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  labs(x = "Simulated Coefficient of Variation", y = "Estimated tau", title = "Sim - True Score Variance", 
       subtitle = "Columns = Reliability, Rows = CV_E, Package = metafor")


# visulaising distributions on individual estimates
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_density(aes(group = as.factor(CVT), fill = as.factor(CVT), x = tau_T), alpha = .3) +
  geom_point(aes(x = CVT * rel * 10, y = 0, group = as.factor(CVT), fill = as.factor(CVT)), shape = 23) +
  labs(x = "Estimated tau", y = "Density", title = "Sim - True Score Variance",
       subtitle = "Columns = Reliability, Rows = CV_E, Package = metafor", group = "CV_T", fill = "CV_T")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))




# ggplot(data = vis.df) + 
#   geom_point(aes(y = CVT*(10*rel), x = CVT), colour = "grey") +
#   geom_line(aes(y = CVT*(10*rel), x = CVT), colour = "grey") +
#   geom_point(aes(y = tau_T, x = CVT)) + 
#   geom_line(aes(y = tau_T, x = CVT)) +
#   facet_grid(rows = vars(CVE), cols = vars(rel)) +
#   labs(x = "Simulated Coefficient of Variation", y = "Estimated tau", title = "Sim - True Score Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_E, Package = metafor")

# visualize using meta package (not necessary atm)
# ggplot(data = vis.df.meta) + 
#   geom_ribbon(aes(x = CVT, ymin = tau_T.lower, ymax = tau_T.upper), 
#               alpha = .1, fill = "blue") +
#   geom_point(aes(y = CVT*(10*rel), x = CVT), colour = "darkgrey") +
#   geom_line(aes(y = CVT*(10*rel), x = CVT), colour = "darkgrey") +
#   geom_point(aes(y = tau_T, x = CVT)) +
#   # geom_point(aes(y = tau_T.lower, x = CVT), colour = "blue", alpha = .3) +
#   # geom_point(aes(y = tau_T.upper, x = CVT), colour = "blue", alpha = .3) + 
#   geom_line(aes(y = tau_T, x = CVT)) +
#   facet_grid(rows = vars(CVE), cols = vars(rel)) +
#   labs(x = "Simulated Coefficient of Variation", y = "Estimated tau", title = "Sim - True Score Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_E, Package = meta") 

# visualize on different scale
# ggplot(data = vis.df) + 
#   geom_abline(colour = "grey") +
#   geom_point(aes(y = CVT*10*rel, x = CVT * 10 * rel), colour = "grey") +
#   geom_point(aes(y = tau_T, x = CVT*(10*rel))) + 
#   geom_line(aes(y = tau_T, x = CVT*(10*rel))) +
#   facet_grid(rows = vars(CVE), cols = vars(rel))+
#   labs(x = "Simulated tau", y = "Estimated tau", title = "Sim - True Score Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_E, Package = metafor")

# meta on different scale
# ggplot(data = vis.df.meta) + 
#   geom_ribbon(aes(x = CVT*10*rel, ymin = tau_T.lower, ymax = tau_T.upper), 
#               alpha = .1, fill = "blue") +
#   geom_abline(colour = "grey") +
#   geom_point(aes(y = CVT*10*rel, x = CVT * 10 * rel), colour = "grey") +
#   geom_point(aes(y = tau_T, x = CVT*(10*rel))) + 
#   geom_line(aes(y = tau_T, x = CVT*(10*rel))) +
#   facet_grid(rows = vars(CVE), cols = vars(rel))+
#   labs(x = "Simulated tau", y = "Estimated tau", title = "Sim - True Score Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_E, Package = meta")


# visualize those, where no heterogeneity should be expected
# ggplot(data = vis.df[which(vis.df$CVT == 0),]) +
#   geom_line(aes(y = CVT*(10*rel), x = rel), colour = "grey") +
#   geom_point(aes(y = CVT*(10*rel), x = rel), colour = "grey") +
#   geom_point(aes(y = tau_T, x = rel)) + 
#   geom_line(aes(y = tau_T, x = rel)) +
#   facet_grid(rows = vars(CVE)) +
#   labs(x = "Simulated Reliability", y = "Estimated tau", title = "Sim - True Score Variance (zero Heterogeneity)", 
#        subtitle = "Rows = CV_E")

# visualize heterogeneity, dependent on discovery (p-value)
# ggplot() + 
#   geom_point(aes(x = vis.df$p_T, y = (vis.df$CVT * (vis.df$rel*10))), alpha = .3) +
#   geom_vline(xintercept = .05) +
#   labs(x = "P-value Heterogeneity", y = "Simulated tau", title = "Signficance Testing Heterogeneity - True Score Variance")


dev.off()



# Visualization for simulated data - error variance heterogeneity

# initialize pdf
pdf(here("Notes/Facet_grid_tauE.pdf"))


# ggplot(data = vis.df) + 
#   geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
#   geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
#   geom_point(aes(y = tau_E, x = CVE)) + 
#   geom_line(aes(y = tau_E, x = CVE)) +
#   facet_grid(rows = vars(CVT), cols = vars(rel)) +
#   labs(x = "Simulated CV_E", y = "Estimated tau", title = "Sim - Error Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor")


#  visualise summarised data - including ribbon
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_ribbon(aes(x = CVE, ymin = E_ll, ymax = E_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = E_m, x = CVE)) + 
  geom_line(aes(y = E_m, x = CVE)) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  labs(x = "Simulated Coefficient of Variation", y = "Estimated tau", title = "Sim - Error Variance", 
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor")

# viualising distributions of individual point estimates - fixed scales
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_density(aes(group = as.factor(CVE), fill = as.factor(CVE), x = tau_E), alpha = .3) +
  geom_point(aes(x = CVE * (1-rel) * 10, y = 0, group = as.factor(CVE), fill = as.factor(CVE)), shape = 23) +
  labs(x = "Estimated tau", y = "Density", title = "Sim - Error Variance",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_E", fill = "CV_E")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))


# viualising distributions of individual point estimates - free scales
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(rel), scales = "free") +
  geom_density(aes(group = as.factor(CVE), fill = as.factor(CVE), x = tau_E), alpha = .3) +
  geom_point(aes(x = CVE * (1-rel) * 10, y = 0, group = as.factor(CVE), fill = as.factor(CVE)), shape = 23) +
  labs(x = "Estimated tau", y = "Density", title = "Sim - Error Variance",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_E", fill = "CV_E")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))

# visualize uncertainty using meta package - not necessary anymore
# ggplot(data = vis.df.meta) + 
#   geom_ribbon(aes(x = CVE, ymin = tau_E.lower, ymax = tau_E.upper), alpha = .1, fill = "blue") +
#   geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
#   geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
#   geom_point(aes(y = tau_E, x = CVE)) +
#   # geom_point(aes(y = tau_E.lower, x = CVE), colour = "blue", alpha = .3) +
#   # geom_point(aes(y = tau_E.upper, x = CVE), colour = "blue", alpha = .3) + 
#   geom_line(aes(y = tau_E, x = CVE)) +
#   facet_grid(rows = vars(CVT), cols = vars(rel)) +
#   labs(x = "Simulated Coefficient of Variation", y = "Estimated tau", title = "Sim - Error Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_T, package = meta") 

# visualise on differen scale
# ggplot(data = vis.df) + 
#   geom_abline(colour = "grey") +
#   geom_point(aes(y = CVE*(10*(1-rel)), x = CVE*(10*(1-rel))), colour = "grey") +
#   geom_point(aes(y = tau_E, x = CVE*(10*(1-rel)))) + 
#   geom_line(aes(y = tau_E, x = CVE*(10*(1-rel)))) +
#   facet_grid(rows = vars(CVT), cols = vars(rel)) +
#   labs(x = "Simulated Coefficient of Variation", y = "Estimated tau", title = "Sim - Error Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_T, package = metafor")

# visualise meta on different scale
# ggplot(data = vis.df.meta) + 
#   geom_ribbon(aes(x = CVE*(10*(1-rel)), ymin = tau_E.lower, ymax = tau_E.upper), alpha = .1, fill = "blue") +
#   geom_abline(colour = "grey") +
#   geom_point(aes(y = CVE*(10*(1-rel)), x = CVE*(10*(1-rel))), colour = "grey") +
#   geom_point(aes(y = tau_E, x = CVE*(10*(1-rel)))) + 
#   geom_line(aes(y = tau_E, x = CVE*(10*(1-rel)))) +
#   facet_grid(rows = vars(CVT), cols = vars(rel)) +
#   labs(x = "Simulated Coefficient of Variation", y = "Estimated tau", title = "Sim - Error Variance", 
#        subtitle = "Columns = Reliability, Rows = CV_T, package = metafor")

# visualise those, where no heterogeneity should be expected - CVE = 0
# ggplot(data = vis.df[which(vis.df$CVE == 0),]) +
#   geom_line(aes(y = CVE*(10*(1-rel)), x = rel), colour = "grey") +
#   geom_point(aes(y = CVE*(10*(1-rel)), x = rel), colour = "grey") +
#   geom_point(aes(y = tau_E, x = rel)) + 
#   geom_line(aes(y = tau_E, x = rel)) +
#   facet_grid(rows = vars(CVT)) +
#   labs(x = "Simulated Reliability", y = "Estimated tau", title = "Sim - Error Variance (zero Heterogeneity)", 
#        subtitle = "Rows = CV_T")

# visualise heterogeneity dependent on discovery (p-value)
# ggplot() + 
#   geom_point(aes(x = vis.df$p_E, y = (vis.df$CVE * (10 - (vis.df$rel*10)))), alpha = .3) +
#   geom_vline(xintercept = .05) +
#   labs(x = "P-value Heterogeneity", y = "Simulated tau", title = "Signficance Testing Heterogeneity - Error Variance")



dev.off()




# Visualization for simulated data - reliability heterogeneity

# initialize pdf
pdf(here("Notes/Facet_grid_rel.pdf"))



#  visualise summarised data - including ribbon
ggplot(data = vis.df_summarized) + 
  # geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  # geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_ribbon(aes(x = rel, ymin = tau_rel_ll, ymax = tau_rel_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = tau_rel_m, x = rel)) + 
  geom_line(aes(y = tau_rel_m, x = rel)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated reliability", y = "Estimated tau", title = "Sim - reliability heterogeneity", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")


#  visualise summarised data - including ribbon - corrected for true variance
ggplot(data = vis.df_summarized) + 
  # geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  # geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_ribbon(aes(x = rel, ymin = tau_rel_cT_ll, ymax = tau_rel_cT_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = tau_rel_cT_m, x = rel)) + 
  geom_line(aes(y = tau_rel_cT_m, x = rel)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated reliability", y = "Estimated tau", title = "Sim - reliability heterogeneity corr True var", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")



# increasing heterogeneity in true variance
ggplot(data = vis.df_summarized) + 
  # geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  # geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_ribbon(aes(x = CVT, ymin = tau_rel_ll, ymax = tau_rel_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = tau_rel_m, x = CVT)) + 
  geom_line(aes(y = tau_rel_m, x = CVT)) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  labs(x = "Simulated CVT", y = "Estimated tau", title = "Sim - reliability heterogeneity", 
       subtitle = "Columns = reliability, Rows = CV_E, Package = metafor")



#  visualise summarised data - including ribbon - corrected for error variance
ggplot(data = vis.df_summarized) + 
  # geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  # geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_ribbon(aes(x = rel, ymin = tau_rel_cE_ll, ymax = tau_rel_cE_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = tau_rel_cE_m, x = rel)) + 
  geom_line(aes(y = tau_rel_cE_m, x = rel)) +
  facet_grid(rows = vars(CVE), cols = vars(CVT)) +
  labs(x = "Simulated reliability", y = "Estimated tau", title = "Sim - reliability heterogeneity corr Error var", 
       subtitle = "Columns = CV_T, Rows = CV_E, Package = metafor")




# increasing heterogeneity in error variance
ggplot(data = vis.df_summarized) + 
  # geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  # geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_ribbon(aes(x = CVE, ymin = tau_rel_ll, ymax = tau_rel_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = tau_rel_m, x = CVE)) + 
  geom_line(aes(y = tau_rel_m, x = CVE)) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  labs(x = "Simulated CVE", y = "Estimated tau", title = "Sim - reliability heterogeneity", 
       subtitle = "Columns = reliability, Rows = CV_T, Package = metafor")





#  visualise summarised data - including ribbon - corrected for both variance components
ggplot(data = vis.df_summarized) + 
  # geom_point(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  # geom_line(aes(y = CVE*(10*(1-rel)), x = CVE), colour = "grey") +
  geom_ribbon(aes(x = rel, ymin = tau_rel_cTE_ll, ymax = tau_rel_cTE_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = tau_rel_cTE_m, x = rel)) + 
  geom_line(aes(y = tau_rel_cTE_m, x = rel)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated reliability", y = "Estimated tau", title = "Sim - reliability heterogeneity corr both var", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")






# viualising distributions of individual point estimates - fixed scales
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_density(aes(group = as.factor(CVE), fill = as.factor(CVE), x = tau_rel), alpha = .3) +
  labs(x = "Estimated tau", y = "Density", title = "Sim - reliability heterogeneity",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_E", fill = "CV_E")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))

# viualising distributions of individual point estimates - fixed scales
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_density(aes(group = as.factor(rel), fill = as.factor(rel), x = tau_rel), alpha = .3) +
  labs(x = "Estimated tau", y = "Density", title = "Sim - reliability heterogeneity",
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor", group = "reliability", fill = "reliability")  +
  scale_fill_discrete(labels = c(".1", ".3", ".5", ".7", ".9"))



# visualizing point estimates of reliability (mean)
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = rel, x = CVT), colour = "grey") +
  geom_line(aes(y = rel, x = CVT), colour = "grey") +
  geom_ribbon(aes(x = CVT, ymin = rel.M_ll, ymax = rel.M_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = rel.M_m, x = CVT)) + 
  geom_line(aes(y = rel.M_m, x = CVT)) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  labs(x = "Simulated CVT", y = "Estimated reliability", title = "Sim - mean reliability", 
       subtitle = "Columns = reliability, Rows = CV_E, Package = metafor")


# visualizing point estimates of reliability (meta-analytic estimate)
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = rel, x = CVT), colour = "grey") +
  geom_line(aes(y = rel, x = CVT), colour = "grey") +
  geom_ribbon(aes(x = CVT, ymin = rel.MAE_ll, ymax = rel.MAE_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = rel.MAE_m, x = CVT)) + 
  geom_line(aes(y = rel.MAE_m, x = CVT)) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  labs(x = "Simulated CVT", y = "Estimated reliability", title = "Sim - meta-analytic reliability estimate", 
       subtitle = "Columns = reliability, Rows = CV_E, Package = metafor")




# visualizing point estimates of reliability (mean) - increasing reliability
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = rel, x = rel), colour = "grey") +
  geom_line(aes(y = rel, x = rel), colour = "grey") +
  geom_ribbon(aes(x = rel, ymin = rel.M_ll, ymax = rel.M_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = rel.M_m, x = rel)) + 
  geom_line(aes(y = rel.M_m, x = rel)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated CVT", y = "Estimated reliability", title = "Sim - mean reliability", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")


# visualizing point estimates of reliability (meta-analytic estimate) - increasing reliability
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = rel, x = rel), colour = "grey") +
  geom_line(aes(y = rel, x = rel), colour = "grey") +
  geom_ribbon(aes(x = rel, ymin = rel.MAE_ll, ymax = rel.MAE_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = rel.MAE_m, x = rel)) + 
  geom_line(aes(y = rel.MAE_m, x = rel)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated CVT", y = "Estimated reliability", title = "Sim - meta-analytic reliability estimate", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")

dev.off()





# visualising bias and MSE in the conditions

pdf(here("Notes/facet_grid_biasMSE.pdf"))


vis.df_summarized2 <- vis.df2 %>%
  group_by(CVT, CVE, rel) %>%
  summarise(bias_varT.M = mean(varT.M - rel*10, na.rm = T),
            bias_varT.MAE = mean(varT.MAE - rel*10, na.rm = T),
            bias_varE.M = mean(varE.M - (1-rel)*10, na.rm = T),
            bias_varE.MAE = mean(varE.MAE - (1-rel)*10, na.rm = T),
            bias_rel.M = mean(rel.M - rel, na.rm = T),
            bias_rel.MAE = mean(rel.MAE-rel, na.rm = T),
            bias_tauT = mean(tau_T - CVT*rel*10, na.rm = T),
            bias_tauE = mean(tau_E - CVE*(1-rel)*10, na.rm = T),
            MSE_varT.M = mean((varT.M - rel*10)^2, na.rm = T),
            MSE_varT.MAE = mean((varT.MAE - rel*10)^2, na.rm = T),
            MSE_varE.M = mean((varE.M - (1-rel)*10)^2, na.rm = T),
            MSE_varE.MAE = mean((varE.MAE - (1-rel)*10)^2, na.rm = T),
            MSE_rel.M = mean((rel.M - rel)^2, na.rm = T),
            MSE_rel.MAE = mean((rel.MAE-rel)^2, na.rm = T),
            MSE_tauT = mean((tau_T - CVT*rel*10)^2, na.rm = T),
            MSE_tauE = mean((tau_E - CVE*(1-rel)*10), na.rm = T)^2)


vis.df3 <- vis.df2 %>%
  mutate(bias_varT.M = (varT.M - rel*10),
         bias_varT.MAE = (varT.MAE - rel*10),
         bias_varE.M = (varE.M - (1-rel)*10),
         bias_varE.MAE = (varE.MAE - (1-rel)*10),
         bias_rel.M = (rel.M - rel),
         bias_rel.MAE = (rel.MAE-rel),
         bias_tauT = (tau_T - CVT*rel*10),
         bias_tauE = (tau_E - CVE*(1-rel)*10)
  )


## TRUE VARIANCE

# visualising bias in true variance estimate (mean)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_varT.M)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "Bias variance estimate", 
       title = "Sim - Mean Bias in True Score Variance estimate (mean)", 
       subtitle = "Columns = reliability, Rows = CV_E")


# visualise bias for each replication - true score variance (mean)
ggplot(vis.df3) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_varT.M, group = as.factor(CVT), colour = as.factor(CVT)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "Bias variance estimate", group = "CV_T", colour = "CV_T",
       title = "Sim - Bias in True Score Variance estimate (mean)", 
       subtitle = "Columns = reliability, Rows = CV_E") +
  scale_colour_discrete(labels = c("0", ".1", ".2", ".3"))


# visualising bias in true variance estimate (meta-analytic estimate)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_varT.MAE)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "Bias variance estimate", 
       title = "Sim - Mean Bias in True Score Variance estimate (meta-analytic estimate)", 
       subtitle = "Columns = reliability, Rows = CV_E")


# visualise bias for each replication - true score variance (meta-analytic estimate)
ggplot(vis.df3) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_varT.MAE, group = as.factor(CVT), colour = as.factor(CVT)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "Bias variance estimate", group = "CV_T", colour = "CV_T",
       title = "Sim - Bias in True Score Variance estimate (meta-analytic estimate)", 
       subtitle = "Columns = reliability, Rows = CV_E") +
  scale_colour_discrete(labels = c("0", ".1", ".2", ".3"))




# visualising MSE in true variance estimate (mean)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = MSE_varT.M)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "MSE variance estimate", 
       title = "Sim - Mean MSE in True Score Variance estimate (mean)", 
       subtitle = "Columns = reliability, Rows = CV_E")


# visualising MSE in true variance estimate (meta-analytic estimate)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = MSE_varT.MAE)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "MSE variance estimate", 
       title = "Sim - Mean MSE in True Score Variance estimate (meta-analytic estimate)", 
       subtitle = "Columns = reliability, Rows = CV_E")




## ERROR VARIANCE


# visualising bias in error variance estimate (mean)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = bias_varE.M)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "Bias variance estimate", 
       title = "Sim - Mean Bias in Error Variance estimate (mean)", 
       subtitle = "Columns = reliability, Rows = CV_T")


# visualise bias for each replication - error variance (mean)
ggplot(vis.df3) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = bias_varE.M, group = as.factor(CVE), colour = as.factor(CVE)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "Bias variance estimate", group = "CV_E", colour = "CV_E",
       title = "Sim - Bias in Error Variance estimate (mean)", 
       subtitle = "Columns = reliability, Rows = CV_T") +
  scale_colour_discrete(labels = c("0", ".1", ".2", ".3"))


# visualising bias in error variance estimate (meta-analytic estimate)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_varE.MAE)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "Bias variance estimate", 
       title = "Sim - Mean Bias in Error Variance estimate (meta-analytic estimate)", 
       subtitle = "Columns = reliability, Rows = CV_T")


# visualise bias for each replication - error variance (meta-analytic estimate)
ggplot(vis.df3) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = bias_varE.MAE, group = as.factor(CVE), colour = as.factor(CVE)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "Bias variance estimate", group = "CV_E", colour = "CV_E",
       title = "Sim - Bias in Error Variance estimate (meta-analytic estimate)", 
       subtitle = "Columns = reliability, Rows = CV_T") +
  scale_colour_discrete(labels = c("0", ".1", ".2", ".3"))



# visualising MSE in error variance estimate (mean)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = MSE_varE.M)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "MSE variance estimate", 
       title = "Sim - Mean MSE in True Score Variance estimate (mean)", 
       subtitle = "Columns = reliability, Rows = CV_E")


# visualising MSE in error variance estimate (meta-analytic estimate)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = MSE_varE.MAE)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "MSE variance estimate", 
       title = "Sim - Mean MSE in True Score Variance estimate (meta-analytic estimate)", 
       subtitle = "Columns = reliability, Rows = CV_E")





## RELIABILITY

# visualising bias in reliability estimate (mean)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_point(aes(x = rel, y = bias_rel.M)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "reliability", y = "Bias reliability estimate", 
       title = "Sim - Mean Bias in reliability estimate (mean)", 
       subtitle = "Columns = CV_E, Rows = CV_T")


# visualise bias for each replication - reliability (mean)
ggplot(vis.df3) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_point(aes(x = rel, y = bias_rel.M, group = as.factor(rel), colour = as.factor(rel)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "rel", y = "Bias variance estimate", group = "rel", colour = "rel",
       title = "Sim - Bias in reliability estimate (mean)", 
       subtitle = "Columns = CV_E, Rows = CV_T") +
  scale_colour_discrete(labels = c(".1", ".3", ".5", ".7", ".9"))


# visualising bias in reliability estimate (mean)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_point(aes(x = rel, y = bias_rel.MAE)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "reliability", y = "Bias reliability estimate", 
       title = "Sim - Mean Bias in reliability estimate (meta-analytic estimate)", 
       subtitle = "Columns = CV_E, Rows = CV_T")


# visualise bias for each replication - true score variance (meta-analytic estimate)
ggplot(vis.df3) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_point(aes(x = rel, y = bias_rel.MAE, group = as.factor(rel), colour = as.factor(rel)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "rel", y = "Bias variance estimate", group = "rel", colour = "rel",
       title = "Sim - Bias in reliability estimate (meta-analytic estimate)", 
       subtitle = "Columns = CV_E, Rows = CV_T") +
  scale_colour_discrete(labels = c(".1", ".3", ".5", ".7", ".9"))




# visualising MSE in reliability estimate (mean)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_point(aes(x = rel, y = MSE_rel.M)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "reliability", y = "MSE reliability estimate", 
       title = "Sim - Mean MSE in reliability estimate (mean)", 
       subtitle = "Columns = CV_E, Rows = CV_T")


# visualising MSE in true variance estimate (meta-analytic estimate)
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_point(aes(x = rel, y = MSE_rel.MAE)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "reliability", y = "MSE reliability estimate", 
       title = "Sim - Mean MSE in reliability estimate (meta-analytic estimate)", 
       subtitle = "Columns = CV_E, Rows = CV_T")




## tau variance components

# visualising bias in true variance tau estimate
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_tauT)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "Bias true variance tau estimate", 
       title = "Sim - Mean Bias in true score variance tau estimate", 
       subtitle = "Columns = reliability, Rows = CV_E")


# visualise bias for each replication - true variance tau estimate
ggplot(vis.df3) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_tauT, group = as.factor(CVT), colour = as.factor(CVT)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "Bias true variance tau estimate", group = "CV_T", colour = "CV_T", 
       title = "Sim - Mean Bias in true score variance tau estimate", 
       subtitle = "Columns = reliability, Rows = CV_E") +
  scale_colour_discrete(labels = c("0", ".1", ".2", ".3"))


# visualising bias in error variance tau estimate
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = bias_tauT)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "Bias true variance tau estimate", 
       title = "Sim - Mean Bias in true variance tau estimate", 
       subtitle = "Columns = reliability, Rows = CV_E")


# visualise bias for each replication - error variance tau estimate
ggplot(vis.df3) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = bias_tauE, group = as.factor(CVE), colour = as.factor(CVE)),
             position = position_jitter(w = 0.04, h = 0)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "Bias error variance tau estimate", group = "CV_E", colour = "CV_E", 
       title = "Sim - Mean Bias in error variance tau estimate", 
       subtitle = "Columns = reliability, Rows = CV_T") +
  scale_colour_discrete(labels = c("0", ".1", ".2", ".3"))




# visualising MSE in true score variance tau estimate 
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = MSE_tauT)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_T", y = "MSE true variance tau estimate", 
       title = "Sim - Mean MSE in true score variance tau estimate", 
       subtitle = "Columns = reliability, Rows = CV_E")


# visualising MSE in error variance tau estimate 
ggplot(vis.df_summarized2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = MSE_tauE)) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  labs(x = "CV_E", y = "Bias error variance tau estimate", 
       title = "Sim - Mean Bias in error variance tau estimate", 
       subtitle = "Columns = reliability, Rows = CV_T")



dev.off()




# visualising  variance estimates

pdf(here("Notes/facet_grid_var.pdf"))





#  visualise summarised data - including ribbon - true variance (meta-analytic estimate)
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = rel*10, x = rel*10), colour = "grey") +
  geom_line(aes(y = rel*10, x = rel*10), colour = "grey") +
  geom_ribbon(aes(x = rel*10, ymin = VT.MAE_ll, ymax = VT.MAE_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = VT.MAE_m, x = rel*10)) + 
  geom_line(aes(y = VT.MAE_m, x = rel*10)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated true score variance", y = "Estimated true score variance", title = "Sim - true score variance (meta-analytic estimate)", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")


#  visualise summarised data - including ribbon - error variance (meta-analytic estimate)
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = (1-rel)*10, x = (1-rel)*10), colour = "grey") +
  geom_line(aes(y = (1-rel)*10, x = (1-rel)*10), colour = "grey") +
  geom_ribbon(aes(x = (1-rel)*10, ymin = VE.MAE_ll, ymax = VE.MAE_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = VE.MAE_m, x = (1-rel)*10)) + 
  geom_line(aes(y = VE.MAE_m, x = (1-rel)*10)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated error variance", y = "Estimated error variance", title = "Sim - error variance (meta-analytic estimate)", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")





#  visualise summarised data - including ribbon - true variance (mean)
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = rel*10, x = rel*10), colour = "grey") +
  geom_line(aes(y = rel*10, x = rel*10), colour = "grey") +
  geom_ribbon(aes(x = rel*10, ymin = VT.M_ll, ymax = VT.M_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = VT.M_m, x = rel*10)) + 
  geom_line(aes(y = VT.M_m, x = rel*10)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated true score variance", y = "Estimated true score variance", title = "Sim - true score variance (mean)", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")


#  visualise summarised data - including ribbon - error variance (mean)
ggplot(data = vis.df_summarized) + 
  geom_point(aes(y = (1-rel)*10, x = (1-rel)*10), colour = "grey") +
  geom_line(aes(y = (1-rel)*10, x = (1-rel)*10), colour = "grey") +
  geom_ribbon(aes(x = (1-rel)*10, ymin = VE.M_ll, ymax = VE.M_ul), alpha = .1, fill = "blue") +
  geom_point(aes(y = VE.M_m, x = (1-rel)*10)) + 
  geom_line(aes(y = VE.M_m, x = (1-rel)*10)) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  labs(x = "Simulated error variance", y = "Estimated error variance", title = "Sim - error variance (mean)", 
       subtitle = "Columns = CV_E, Rows = CV_T, Package = metafor")





# viualising distributions of individual point estimates - true variance
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_density(aes(group = as.factor(CVE), fill = as.factor(CVE), x = varT.M), alpha = .3) +
  geom_point(aes(x = rel * 10, y = 0, group = as.factor(CVE), fill = as.factor(CVE)), shape = 23) +
  labs(x = "variance estimate", y = "Density", title = "Sim - true score variance (meta-analytic estimate)",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_E", fill = "CV_E")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))

# viualising distributions of individual point estimates - true variance
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(rel), scales = "free") +
  geom_density(aes(group = as.factor(CVE), fill = as.factor(CVE), x = varT.M), alpha = .3) +
  geom_point(aes(x = rel * 10, y = 0, group = as.factor(CVT), fill = as.factor(CVT)), shape = 23) +
  labs(x = "variance estimate", y = "Density", title = "Sim - true score variance (meta-analytic estimate)",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_E", fill = "CV_E")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))




# viualising distributions of individual point estimates - error variance
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_density(aes(group = as.factor(CVT), fill = as.factor(CVT), x = varE.M), alpha = .3) +
  geom_point(aes(x = (1-rel) * 10, y = 0, group = as.factor(CVT), fill = as.factor(CVT)), shape = 23) +
  labs(x = "variance estimate", y = "Density", title = "Sim - error variance (meta-analytic estimate)",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_T", fill = "CV_T")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))

# viualising distributions of individual point estimates - error variance
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVE), cols = vars(rel), scales = "free") +
  geom_density(aes(group = as.factor(CVT), fill = as.factor(CVT), x = varE.M), alpha = .3) +
  geom_point(aes(x = (1-rel) * 10, y = 0, group = as.factor(CVT), fill = as.factor(CVT)), shape = 23) +
  labs(x = "variance estimate", y = "Density", title = "Sim - error variance (meta-analytic estimate)",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_T", fill = "CV_T")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))





# viualising distributions of individual point estimates - standard error true variance
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_density(aes(group = as.factor(CVE), fill = as.factor(CVE), x = SE_T.M), alpha = .3) +
  labs(x = "mean bootstrapped standard error", y = "Density", title = "Sim - Standard Error true score variance",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_E", fill = "CV_E")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))


# viualising distributions of individual point estimates - standard error error variance - scales free
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVE), cols = vars(rel), scales = "free") +
  geom_density(aes(group = as.factor(CVT), fill = as.factor(CVT), x = SE_E.M), alpha = .3) +
  labs(x = "mean bootstrapped standard error", y = "Density", title = "Sim - Standard Error error variance",
       subtitle = "Columns = Reliability, Rows = CV_E, Package = metafor", group = "CV_T", fill = "CV_T")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))

# viualising distributions of individual point estimates - standard error error variance - scales free
ggplot(data = vis.df2) +
  facet_grid(rows = vars(CVT), cols = vars(rel), scales = "free") +
  geom_density(aes(group = as.factor(CVE), fill = as.factor(CVE), x = SE_E.M), alpha = .3) +
  labs(x = "mean bootstrapped standard error", y = "Density", title = "Sim - Standard Error error variance",
       subtitle = "Columns = Reliability, Rows = CV_T, Package = metafor", group = "CV_E", fill = "CV_E")  +
  scale_fill_discrete(labels = c("0", ".1", ".2", ".3"))



dev.off()







vis.df_summarised3 <- vis.df2 %>%
  group_by(CVT, CVE, rel) %>%
  summarise(cor_rel.M_ase.M = cor(rel.M, ase.M),
            cor_varT.M_SE_T.M = cor(varT.M, SE_T.M),
            cor_varE.M_SE_E.M = cor(varE.M, SE_E.M))


ggplot(vis.df_summarised3) +
  facet_grid(rows = vars(CVT), cols = vars(CVE)) +
  geom_point(aes(x = rel, y = cor_rel.M_ase.M))

ggplot(vis.df_summarised3) +
  facet_grid(rows = vars(CVE), cols = vars(rel)) +
  geom_point(aes(x = CVT, y = cor_varT.M_SE_T.M))

ggplot(vis.df_summarised3) +
  facet_grid(rows = vars(CVT), cols = vars(rel)) +
  geom_point(aes(x = CVE, y = cor_varE.M_SE_E.M))






pdf(here("Notes/tau_T_distributions.pdf"))


apply(condition_combinations, MARGIN = 1, function(y){
  d <- vis.df %>%
    filter(CVE == y[2], CVT == y[1], rel == y[3]) %>%
    .$tau_T
  
  plot(density(d, na.rm = T))
})

dev.off()


ggplot(data = vis.df) + 
  geom_point(aes(y = tau_E, x = CVE), colour = "red") + 
  geom_line(aes(y = tau_E, x = CVE), colour = "red") +
  geom_point(aes(y = tau_T, x = CVT), colour = "blue") + 
  geom_line(aes(y = tau_T, x = CVT), colour = "blue") +
  facet_grid(cols = vars(rel)) +
  labs(y = "tau", x = "CV")




var_X.L <- lapply(Large_Sim_Data, FUN = function(x){
  var_X <- x$varT + x$varE
  SE_var_X <- (var_X * (99/100))*sqrt(2/(100-1))
  
  return(data.frame(var_X = var_X,
                    SE_var_X = SE_var_X))
})


plot(sapply(var_X.L, FUN = function(x){mean(x$SE_var_X)}), sapply(Large_Sim_Data, FUN = function(x){mean(x$SE_T)}))


var_X_rma <- sapply(var_X.L, FUN = function(x){
  sqrt(metafor::rma(measure = "GEN", method = "REML", yi = var_X, sei = SE_var_X, data = x)$tau2)
})


alpha_rma <- sapply(Large_Sim_Data, FUN = function(x){
  sqrt(metafor::rma(measure = "GEN", method = "REML", yi = rel, sei = ase, data = x)$tau2)
})

condition_combinations$CVT * 10 * condition_combinations$rel
condition_combinations$CVE * 10 * (1-condition_combinations$rel)
condition_combinations$CVT * 10 * condition_combinations$rel + condition_combinations$CVE * 10 * (1-condition_combinations$rel)

plot(condition_combinations$CVT * 10 * condition_combinations$rel + condition_combinations$CVE * 10 * (1-condition_combinations$rel),
     var_X_rma)
abline(a = 0, b = 1)


data <- sim_het_VC(j = 10, n = 100, k = 100,
                   reliability = condition_combinations$rel[33], mean_score = 0, 
                   mean_observed_var = 10,
                   CV_var_T = condition_combinations$CVT[33],
                   CV_var_E = condition_combinations$CVE[33])


B.varX <- lapply(data$sim_data.L, FUN = function(x){
  B <- boot::boot(x, 
                  statistic = function(data, indices){
                    d <- data[indices,]
                    v <- var(rowMeans(d))
                    return(v)
                  },
                  R = 1000)
  
  var_X <- var(rowMeans(x))
  
  return(data.frame(b.SE_varX = sd(B$t),
                    SE_varX = (var_X * (99/100))*sqrt(2/(100-1))))
})




plot(sapply(B.varX, FUN = function(x){x$SE_varX}), sapply(B.varX, FUN = function(x){x$b.SE_varX}))
abline(a = 0, b = 1)


MSE_T <- mean((vis.df$tau_T - (vis.df$CVT * (vis.df$rel*10)))^2)
bias_T <- mean(vis.df$tau_T) - mean(vis.df$CVT * (vis.df$rel*10))

(vis.df$tau_T - (vis.df$CVT * (vis.df$rel*10)))^2

plot((vis.df$tau_T - (vis.df$CVT * (vis.df$rel*10))))


plot(vis.df$CVT * 10*vis.df$rel, vis.df$tau_T)
abline(a = 0, b = 1)


plot(vis.df$CVE * (10 - 10*vis.df$rel), vis.df$tau_E)
abline(a = 0, b = 1)


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
  geom_vline(xintercept = .05) +
  labs(x = "P-value Heterogeneity", y = "Simulated tau", title = "Signficance Testing Heterogeneity - True Score Variance")


mean(vis.df$p_T[which(vis.df$CVT > 0)] < .05)
mean(vis.df$p_T[which(vis.df$CVT == 0)] < .05)

mean(vis.df$p_E[which(vis.df$CVE > 0)] < .05)
mean(vis.df$p_E[which(vis.df$CVE == 0)] < .05)


ggplot() + 
  geom_point(aes(x = vis.df$p_E, y = (vis.df$CVE * (10 - (vis.df$rel*10)))), alpha = .3) +
  geom_vline(xintercept = .05) +
  labs(x = "P-value Heterogeneity", y = "Simulated tau", title = "Signficance Testing Heterogeneity - Error Variance")


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
