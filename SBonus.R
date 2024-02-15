### Reliability Generalization  ###

# ░██████╗░██████╗░░█████╗░███╗░░██╗██╗░░░██╗░██████╗
# ██╔════╝░░██╔══██╗██╔══██╗████╗░██║██║░░░██║██╔════╝
# ╚█████╗░░░██████╦╝██║░░██║██╔██╗██║██║░░░██║╚█████╗░
# ░╚═══██╗░░██╔══██╗██║░░██║██║╚████║██║░░░██║░╚═══██╗
# ██████╔╝░░██████╦╝╚█████╔╝██║░╚███║╚██████╔╝██████╔╝
# ╚═════╝░░░╚═════╝░░╚════╝░╚═╝░░╚══╝░╚═════╝░╚═════╝░

###################################################################################################
# This script is used to generate additional graphics not found in the preprint at osf.io/ud9rb   #
# The graphics demonstrate that for true score variance, the estimation is much more biased and   #
#  inefficient.                                                                                   #
###################################################################################################


# library loading and installing as necessary


# selected packages required for analysis and simulation
packages <- c("tidyverse", "here", "metafor", "future.apply")

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



# loading aggregated results from simulation
df_comparison <- read.csv(here("Simulation Data/Sim80000_rma_df.csv"))

# generate estimates of mean per condition
df_comparison_means <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = T)))
names(df_comparison_means) <- c("CVT", "CVE", "rel", 
                                paste0("mean_", names(df_comparison_means)[!names(df_comparison_means) %in% c("CVT", "CVE", "rel")]))

# generate estimates of lower level of 80%-estimation interval
df_comparison_80ll <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) quantile(x, probs = .1, na.rm = T)))
names(df_comparison_80ll) <- c("CVT", "CVE", "rel", 
                               paste0("ll80_", names(df_comparison_80ll)[!names(df_comparison_80ll) %in% c("CVT", "CVE", "rel")]))

# generate estimates of upper level of 80%-estimation interval
df_comparison_80ul <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) quantile(x, probs = .9, na.rm = T)))
names(df_comparison_80ul) <- c("CVT", "CVE", "rel", 
                               paste0("ul80_", names(df_comparison_80ul)[!names(df_comparison_80ul) %in% c("CVT", "CVE", "rel")]))

# generate estimates of estimates' variance within each condition (to assess efficiency)
df_comparison_vars <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(tau_T:mu_rel_Botella_transf, \(x) var(x, na.rm = T)))
names(df_comparison_vars) <- c("CVT", "CVE", "rel", 
                               paste0("var_", names(df_comparison_vars)[!names(df_comparison_vars) %in% c("CVT", "CVE", "rel")]))

# combine the data-sets
df_comparison_summary <- data.frame(df_comparison_means,
                                    df_comparison_80ll[,!names(df_comparison_80ll) %in% c("CVT", "CVE", "rel")],
                                    df_comparison_80ul[,!names(df_comparison_80ul) %in% c("CVT", "CVE", "rel")],
                                    df_comparison_vars[,!names(df_comparison_vars) %in% c("CVT", "CVE", "rel")])


### Generate figures

# p1, individual estimates of true score variance heterogeneity, grouped across levels of CV_T.
# CV_E is set to zero here
df_comparison %>% 
  filter(CVE == 0) %>% 
  ggplot(aes(y = tau_T, x = tau_varT, colour = as.factor(rel))) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(position = position_jitter(width = .03), 
             alpha = .01) +
  facet_wrap(vars(factor(CVT, labels = c("CV[TV] == 0", "CV[TV] == .1", "CV[TV] == .2", "CV[TV] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression(hat(tau)[TV]),
       x = expression(tau[TV]),
       subtitle = "a) Estimates",
       colour = "Score\nReliability") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"))



# p2, mean bias of true score variance heterogeneity estimates, grouped across levels of CV_T.
# CV_E is set to zero here
df_comparison_summary %>% 
  filter(CVE == 0) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 0) +
  geom_ribbon(aes(x = mean_tau_varT,
                  ymin = ul80_bias_tau_varT,
                  ymax = ll80_bias_tau_varT),
              fill = "blue", alpha = .1) +
  geom_point(aes(x = mean_tau_varT, y = mean_bias_tau_varT, colour = as.factor(rel))) +
  facet_wrap(vars(factor(CVT, labels = c("CV[TV] == 0", "CV[TV] == .1", "CV[TV] == .2", "CV[TV] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("mean bias in " ~ hat(tau)[EV]),
       x = expression(tau[TV]),
       subtitle = "b) Bias",
       colour = "Score\nReliability") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"))


# p3, assessment of effiency in true score variance heterogeneity estimates, grouped across levels of CV_T.
# CV_e is set to zero here
df_comparison_summary %>% 
  filter(CVE == 0) %>% 
  ggplot() +
  geom_point(aes(x = mean_tau_varT, y = sqrt(var_tau_T), colour = as.factor(rel))) +
  scale_y_reverse() +
  facet_wrap(vars(factor(CVT, labels = c("CV[TV] == 0", "CV[TV] == .1", "CV[TV] == .2", "CV[TV] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("efficiency in " ~ hat(tau)[EV]),
       x = expression(tau[TV]),
       subtitle = "c) Efficiency",
       colour = "Score\nReliability") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))
