

packages <- c("dplyr", "here", "magrittr", "ggplot2")

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



df_comparison <- read.csv(here("Simulation Data/Sim80000_rma_df.csv"))



full_sim <- read.csv(here("Simulation Data/Sim80000_rma_df.csv"))

sim_aggregates_m <- full_sim %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = T)))

sim_aggregates_ll <- full_sim %>%
  group_by(CVT, CVE, rel) %>%
  summarise(across(everything(), \(x) quantile(x, .025, na.rm = T)))

names(sim_aggregates_ll) <- paste0(names(sim_aggregates_ll), "_ll")

sim_aggregates_ul <- full_sim %>%
  group_by(CVT, CVE, rel) %>%
  summarise(across(everything(), \(x) quantile(x, .975, na.rm = T)))

names(sim_aggregates_ul) <- paste0(names(sim_aggregates_ul), "_ul")


sim_aggregates_full <- data.frame(sim_aggregates_m,
                                  sim_aggregates_ll,
                                  sim_aggregates_ul)


write.csv(sim_aggregates_full, here("Data/Shiny Data/Sim80000_rma_agg_df.csv"),
          row.names = FALSE)



df_comparison_means <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = T)))

names(df_comparison_means) <- c("CVT", "CVE", "rel", 
                                paste0("mean_", names(df_comparison_means)[!names(df_comparison_means) %in% c("CVT", "CVE", "rel")]))


df_comparison_80ll <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) quantile(x, probs = .1, na.rm = T)))

names(df_comparison_80ll) <- c("CVT", "CVE", "rel", 
                                paste0("ll80_", names(df_comparison_80ll)[!names(df_comparison_80ll) %in% c("CVT", "CVE", "rel")]))


df_comparison_80ul <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) quantile(x, probs = .9, na.rm = T)))

names(df_comparison_80ul) <- c("CVT", "CVE", "rel", 
                                paste0("ul80_", names(df_comparison_80ul)[!names(df_comparison_80ul) %in% c("CVT", "CVE", "rel")]))


df_comparison_vars <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(tau_T:mu_rel_Botella_transf, \(x) var(x, na.rm = T)))

names(df_comparison_vars) <- c("CVT", "CVE", "rel", 
                               paste0("var_", names(df_comparison_vars)[!names(df_comparison_vars) %in% c("CVT", "CVE", "rel")]))


df_comparison_summary <- data.frame(df_comparison_means,
                                    df_comparison_80ll[,!names(df_comparison_80ll) %in% c("CVT", "CVE", "rel")],
                                    df_comparison_80ul[,!names(df_comparison_80ul) %in% c("CVT", "CVE", "rel")],
                                    df_comparison_vars[,!names(df_comparison_vars) %in% c("CVT", "CVE", "rel")])


pdf(file = "C:/Users/Lukas/Downloads/sim80000_results_disagg.pdf")

ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = tau_varT, y = tau_T, colour = as.factor(CVT)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in true score variance",
       subtitle = "rows = CVE, columns = reliability, var(T) = var(x * REL)",
       colour = "CVT")   


ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = tau_varT, y = tau_T_alt, colour = as.factor(CVT)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in true score variance",
       subtitle = "rows = CVE, columns = reliability, var(T) = var(x) - var(E)",
       colour = "CVT")   


ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = tau_varT^2, y = tau_T2_alt, colour = as.factor(CVT)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "estimated heterogeneity (tau^2) in true score variance",
       x = "predicted heterogeneity (tau^2) in true score variance",
       title = "Heterogeneity in true score variance",
       subtitle = "rows = CVE, columns = reliability, var(T) = var(x) - var(E)",
       colour = "CVT")   


ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = mu_varT, y = mu_T, colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "meta-analytic mean true score variance",
       x = "population mean true score variance",
       title = "Meta-analytic mean true score variance",
       subtitle = "rows = CVE, columns = CVT, var(T) = var(x * REL)",
       colour = "reliability")   


ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = mu_varT, y = mu_T_alt, colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "meta-analytic mean true score variance",
       x = "population mean true score variance",
       title = "Meta-analytic mean true score variance",
       subtitle = "rows = CVE, columns = CVT, var(T) = var(x) - var(E)",
       colour = "reliability")   


ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = tau_varE, y = tau_E, colour = as.factor(CVE)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVT),
             cols = vars(rel)) +
  labs(y = "estimated heterogeneity (tau) in error score variance",
       x = "predicted heterogeneity (tau) in error score variance",
       title = "Heterogeneity in error score variance",
       subtitle = "rows = CVT, columns = reliability",
       colour = "CVE")   


ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = mu_varE, y = mu_E, colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "meta-analytic mean error score variance",
       x = "population mean error score variance",
       title = "Meta-analytic mean error score variance",
       subtitle = "rows = CVE, columns = CVT",
       colour = "reliability")   



ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = pred.tau_rel, y = tau_rel_transf, colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .005)) +
  facet_grid(rows = vars(CVT),
             cols = vars(CVE)) +
  labs(y = "estimated heterogeneity (tau) in r",
       x = "predicted heterogeneity (tau) in r",
       title = "Transformed estimates of heterogeneity in ln(1-r)",
       subtitle = "rows = CVT, columns = CVE",
       colour = "Reliability") 



ggplot(data = df_comparison) +
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(aes(x = pred.mu_rel, y = mu_rel_transf, colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "meta-analytic mean score reliability",
       x = "population mean score reliability",
       title = "Meta-analytic mean score reliability",
       subtitle = "rows = CVE, columns = CVT",
       colour = "reliability") 


ggplot(data = df_comparison) +
  geom_point(aes(x = pred.mu_rel, y = tau_rel_transf, colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVT),
             cols = vars(CVE)) +
  labs(y = "estimated heterogeneity (tau) in r",
       x = "predicted level of score reliability",
       title = "Transformed estimates of heterogeneity in ln(1-r)",
       subtitle = "rows = CVT, columns = CVE",
       colour = "Reliability") 


ggplot(data = df_comparison) +
  geom_line(aes(x = rel, y = CVE/CVT), size = 2) +
  geom_point(aes(x = rel, y = est.CVE/est.CVT_alt, colour = as.factor(rel)), alpha = .1,
             position = position_jitter(width = .01)) +
  facet_grid(rows = vars(CVT),
             cols = vars(CVE)) +
  labs(y = "ratio of coefficient of variation (CV_E / CV_T)",
       x = "level of score reliability",
       title = "Ratio of Coefficients of Variation",
       subtitle = "rows = CVT, columns = CVE, y-Axis limited between 0 & 10",
       colour = "Reliability") +
  ylim(0, 10)



dev.off()



pdf(file = "C:/Users/Lukas/Downloads/sim80000_results_agg.pdf")


## Summaries True Score variance

# T- on scale tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varT, 
                  ymin = ul80_tau_T,
                  ymax = ll80_tau_T), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = mean_tau_varT, y = mean_tau_T, colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = CVT, var(T) = var(X) * REL",
       colour = "Score \n Reliability") 

# T alt-- on scale tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varT, 
                  ymin = ul80_tau_T_alt,
                  ymax = ll80_tau_T_alt), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = mean_tau_varT, y = mean_tau_T_alt, colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = CVT, var(T) = var(X) - var(E)",
       colour = "Score \n Reliability") 



# T- mean bias tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varT, 
                  ymin = ul80_bias_tau_varT,
                  ymax = ll80_bias_tau_varT),
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varT, y = mean_bias_tau_varT, colour = as.factor(rel))) + 
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean bias in estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = CVT, var(T) = var(X) * REL",
       colour = "Score \n Reliability") 


# T alt.- mean bias tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varT, 
                  ymin = ul80_bias_tau_varT_alt,
                  ymax = ll80_bias_tau_varT_alt),
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varT, y = mean_bias_tau_varT_alt, colour = as.factor(CVT))) + 
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "mean bias in estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = rel, var(T) = var(X) - var(E)",
       colour = "CVT")  


# T alt.- mean relative bias tau
ggplot(data = df_comparison_summary) +
  # geom_ribbon(aes(x = mean_tau_varT, 
  #                 ymin = ul80_bias_tau_varT_alt,
  #                 ymax = ll80_bias_tau_varT_alt),
  #             fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varT, y = abs(mean_bias_tau_varT_alt)/mean_tau_varT, colour = as.factor(CVT))) + 
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "mean bias in estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = rel, var(T) = var(X) - var(E)",
       colour = "CVT")  


# T alt.- sd tau
ggplot(data = df_comparison_summary) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varT, y = sqrt(var_tau_T_alt), colour = as.factor(CVT))) + 
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "sd in estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = rel, var(T) = var(X) - var(E)",
       colour = "CVT")  


# T alt.- relative sd tau
ggplot(data = df_comparison_summary) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varT, y = sqrt(var_tau_T_alt)/mean_tau_varT, colour = as.factor(CVT))) + 
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "sd in estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (tau) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = rel, var(T) = var(X) - var(E)",
       colour = "CVT")  



# T- on scale CVT
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVT, 
                  ymin = ul80_tau_T,
                  ymax = ll80_tau_T), 
              fill = "blue", alpha = .1) +
  geom_line(aes(x = CVT, y = mean_tau_varT), colour = "grey") +
  geom_point(aes(x = CVT, y = mean_tau_T)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel))  +
  labs(y = "mean estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (CVT) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = Score Reliability, var(T) = var(X) * REL") 


# T alt.- on scale CVT
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVT, 
                  ymin = ul80_tau_T_alt,
                  ymax = ll80_tau_T_alt), 
              fill = "blue", alpha = .1) +
  geom_line(aes(x = CVT, y = mean_tau_varT), colour = "grey") +
  geom_point(aes(x = CVT, y = mean_tau_T_alt)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel))  +
  labs(y = "mean estimated heterogeneity (tau) in true score variance",
       x = "predicted heterogeneity (CVT) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = Score Reliability, var(T) = var(X) - var(E)") 


# T- CVT v CVT
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVT, 
                  ymin = ul80_est.CVT,
                  ymax = ll80_est.CVT), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey") +
  geom_point(aes(x = CVT, y = mean_est.CVT)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "mean estimated heterogeneity (CVT) in true score variance",
       x = "predicted heterogeneity (CVT) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = Score Reliability, var(T) = var(X) * REL") 


# T- CVT v CVT
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVT, 
                  ymin = ul80_est.CVT_alt,
                  ymax = ll80_est.CVT_alt), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey") +
  geom_point(aes(x = CVT, y = mean_est.CVT_alt)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "mean estimated heterogeneity (CVT) in true score variance",
       x = "predicted heterogeneity (CVT) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = Score Reliability, var(T) = var(X) - var(E)") 


# T- bias CVT v CVT
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVT, 
                  ymin = ul80_bias_est.CVT,
                  ymax = ll80_bias_est.CVT), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  geom_point(aes(x = CVT, y = mean_bias_est.CVT)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "mean bias in estimated heterogeneity (CVT) in true score variance",
       x = "predicted heterogeneity (CVT) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = Score Reliability, var(T) = var(X) * REL") 

# T- bias CVT v CVT
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVT, 
                  ymin = ul80_bias_est.CVT_alt,
                  ymax = ll80_bias_est.CVT_alt), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 0, colour = "grey") +
  geom_point(aes(x = CVT, y = mean_bias_est.CVT_alt)) +
  facet_grid(rows = vars(CVE),
             cols = vars(rel)) +
  labs(y = "mean bias in estimated heterogeneity (CVT) in true score variance",
       x = "predicted heterogeneity (CVT) in true score variance",
       title = "Heterogeneity in True Score Variance",
       subtitle = "rows = CVE, columns = Score Reliability, var(T) = var(X) - var(E)") 



## Summaries error score variance

# E- on scale tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varE, 
                  ymin = ul80_tau_E,
                  ymax = ll80_tau_E), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = mean_tau_varE, y = mean_tau_E, colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in error score variance",
       x = "predicted heterogeneity (tau) in error score variance",
       title = "Heterogeneity in Error Score Variance",
       subtitle = "rows = CVE, columns = CVT",
       colour = "Score \n Reliability")

# E- bias on scale tau
ggplot(data = df_comparison_summary) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varE, y = mean_bias_tau_varE, colour = as.factor(CVE))) +
  facet_grid(rows = vars(CVT),
             cols = vars(rel)) +
  labs(y = "mean bias in heterogeneity (tau) in error score variance",
       x = "predicted heterogeneity (tau) in error score variance",
       title = "Heterogeneity in Error Score Variance",
       subtitle = "rows = CVT, columns = rel",
       colour = "CVE")


# E- var on scale tau
ggplot(data = df_comparison_summary) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varE, y = sqrt(var_tau_E), colour = as.factor(CVE))) +
  facet_grid(rows = vars(CVT),
             cols = vars(rel)) +
  labs(y = "sd in heterogeneity (tau) in error score variance",
       x = "predicted heterogeneity (tau) in error score variance",
       title = "Heterogeneity in Error Score Variance",
       subtitle = "rows = CVT, columns = rel",
       colour = "CVE")

# E- on scale CVE
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVE, 
                  ymin = ul80_tau_E,
                  ymax = ll80_tau_E), 
              fill = "blue", alpha = .1) +
  geom_line(aes(x = CVE, y = mean_tau_varE), colour = "grey") +
  geom_point(aes(x = CVE, y = mean_tau_E)) +
  facet_grid(rows = vars(CVT),
             cols = vars(rel)) +
  labs(y = "mean estimated heterogeneity (tau) in error score variance",
       x = "predicted heterogeneity (CVE) in error score variance",
       title = "Heterogeneity in Error Score Variance",
       subtitle = "rows = CVT, columns = Score Reliability") 

# E- CVE vs CVE
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = CVE, 
                  ymin = ul80_est.CVE,
                  ymax = ll80_est.CVE), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey") +
  geom_point(aes(x = CVE, y = mean_est.CVE)) +
  facet_grid(rows = vars(CVT),
             cols = vars(rel)) +
  labs(y = "mean estimated heterogeneity (CVE) in errpr score variance",
       x = "predicted heterogeneity (CVE) in error score variance",
       title = "Heterogeneity in Error Score Variance",
       subtitle = "rows = CVT, columns = Score Reliability") 


## Score Reliability (back-transformed)

# rel- scale rel vs tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = rel, 
                  ymin = ul80_tau_rel_transf,
                  ymax = ll80_tau_rel_transf), 
              fill = "blue", alpha = .1) +
#  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = rel, y = mean_tau_rel_transf)) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in score reliability",
       x = "level of score reliability",
       title = "Heterogeneity in Score Reliability",
       subtitle = "rows = CVE, columns = CVT")



# rel - no back-transform
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = rel, 
                  ymin = ul80_tau_Bonnett,
                  ymax = ul80_tau_Bonnett), 
              fill = "blue", alpha = .1) +
  geom_point(aes(x = rel, y = mean_tau_Bonnett)) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in Bonnett",
       x = "level of score reliability",
       title = "Heterogeneity in Score Reliability - Bonnett - uncorrection",
       subtitle = "rows = CVE, columns = CVT") 


# Botella - correction of Rel
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = rel, 
                  ymin = ul80_tau_Bonett_rel_Botella,
                  ymax = ul80_tau_Bonett_rel_Botella), 
              fill = "blue", alpha = .1) +
  geom_point(aes(x = rel, y = mean_tau_Bonett_rel_Botella)) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in Bonnett - Botella-cor.",
       x = "level of score reliability",
       title = "Heterogeneity in Score Reliability- Botella correction",
       subtitle = "rows = CVE, columns = CVT") 



# Botella - correction of Rel - scale of tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_lnvarE, 
                  ymin = ul80_tau_Bonett_rel_Botella,
                  ymax = ul80_tau_Bonett_rel_Botella), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = mean_tau_lnvarE, y = mean_tau_Bonett_rel_Botella)) +
  facet_grid(rows = vars(CVT),
             cols = vars(rel)) +
  labs(y = "mean estimated heterogeneity (tau) in Bonnett - Botella-cor.",
       x = "predicted heterogeneity (tau) in ln error score variance",
       title = "Heterogeneity in Score Reliability - Botella correction",
       subtitle = "rows = CVE, columns = CVT") 



# Botella - correction of Rel back-transformed! - scale of tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varE, 
                  ymin = ll80_tau_Bonett_rel_Botella_transf,
                  ymax = ul80_tau_Bonett_rel_Botella_transf), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = mean_tau_varE, y = mean_tau_Bonett_rel_Botella_transf)) +
  facet_grid(rows = vars(CVT),
             cols = vars(rel)) +
  labs(y = "mean estimated heterogeneity (tau) in Bonnett - Botella-cor.",
       x = "predicted heterogeneity (tau) in ln error score variance",
       title = "Heterogeneity in Score Reliability - Botella correction",
       subtitle = "rows = CVE, columns = CVT") 



# rel- scale tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_pred.tau_rel,
                  ymin = ul80_tau_rel_transf,
                  ymax = ll80_tau_rel_transf),
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = mean_pred.tau_rel, y = mean_tau_rel_transf, colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in score reliability",
       x = "predicted heterogeneity (tau) in score reliability",
       title = "Heterogeneity in Score Reliability",
       subtitle = "rows = CVE, columns = CVT", 
       colour = "Score \n Reliability")


# rel- mean bias scale tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_pred.tau_rel,
                  ymin = ul80_bias_tau_rel,
                  ymax = ll80_bias_tau_rel),
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_pred.tau_rel, y = mean_bias_tau_rel, colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean bias in estimated heterogeneity (tau) in score reliability",
       x = "predicted heterogeneity (tau) in score reliability",
       title = "Heterogeneity in Score Reliability",
       subtitle = "rows = CVE, columns = CVT", 
       colour = "Score \n Reliability")


# rel- mean variance scale tau
ggplot(data = df_comparison_summary) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_pred.tau_rel, y = sqrt(var_tau_rel_transf), colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean variance in estimated heterogeneity (tau) in score reliability",
       x = "predicted heterogeneity (tau) in score reliability",
       title = "Heterogeneity in Score Reliability",
       subtitle = "rows = CVE, columns = CVT", 
       colour = "Score \n Reliability")



# rel- CVrel vs CVrel
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_pred.CVrel, 
                  ymin = ul80_est.CVrel,
                  ymax = ll80_est.CVrel), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1, colour = "grey") +
  geom_point(aes(x = mean_pred.CVrel, y = mean_est.CVrel, colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT))  +
  labs(y = "mean estimated heterogeneity (CVrel) in score reliability",
       x = "predicted heterogeneity (CVrel) in score reliability",
       title = "Heterogeneity in Score Reliability",
       subtitle = "rows = CVE, columns = CVT",
       colour = "Score \n Reliability")


## Summaries Observed Score variance

# T- on scale tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varX, 
                  ymin = ul80_tau_X,
                  ymax = ll80_tau_X), 
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(x = mean_tau_varX, y = mean_tau_X, colour = as.factor(rel))) +
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean estimated heterogeneity (tau) in observed score variance",
       x = "predicted heterogeneity (tau) in observed score variance",
       title = "Heterogeneity in Observed Score Variance",
       subtitle = "rows = CVE, columns = CVT",
       colour = "Score \n Reliability") 



# T- mean bias tau
ggplot(data = df_comparison_summary) +
  geom_ribbon(aes(x = mean_tau_varX, 
                  ymin = ul80_bias_tau_varX,
                  ymax = ll80_bias_tau_varX),
              fill = "blue", alpha = .1) +
  geom_abline(intercept = 0, slope = 0) +
  geom_point(aes(x = mean_tau_varX, y = mean_bias_tau_varX, colour = as.factor(rel))) + 
  facet_grid(rows = vars(CVE),
             cols = vars(CVT)) +
  labs(y = "mean bias in estimated heterogeneity (tau) in observed score variance",
       x = "predicted heterogeneity (tau) in observed score variance",
       title = "Heterogeneity in Observed Score Variance",
       subtitle = "rows = CVE, columns = CVT",
       colour = "Score \n Reliability") 



dev.off()

