### Reliability Generalization  ###


# ░██████╗░░██╗██╗
# ██╔════╝░██╔╝██║
# ╚█████╗░██╔╝░██║
# ░╚═══██╗███████║
# ██████╔╝╚════██║
# ╚═════╝░░░░░░╚═╝

###################################################################################################
# This script is used to manipulate the simulated data for the preparation of graphics used in    #
#  the final report/publication. It also prepares the graphics found in the preprint osf.io/ud9rb #
# Additionally, aggregated-data for the shiny app is stored.                                      #
###################################################################################################


# library loading and installing as necessary


# selected packages required for data manipulation
packages <- c("dplyr", "here", "magrittr", "ggplot2", "ggpubr", "RColorBrewer")

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


# load data-set from simulations (including estimates of individual bias)
df_comparison <- read.csv(here("Simulation Data/Sim80000_rma_df.csv"))





### Preparing data for graphics for final report/publication 

# generate mean estimates for each condition
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

cols <- RColorBrewer::brewer.pal(7, "Blues")

### Generate figures

# p1, individual estimates of error score variance heterogeneity, grouped across levels of CV_E.
# CV_T is set to zero here
p1 <- df_comparison %>% 
  filter(CVT == 0) %>% 
  ggplot(aes(y = tau_E, x = tau_varE, colour = as.factor(rel))) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(position = position_jitter(width = .03), 
             alpha = .01) +
  facet_wrap(vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0", "CV[sigma[E]^2] == .1", "CV[sigma[E]^2] == .2", "CV[sigma[E]^2] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression(hat(tau)[sigma[E]^2]),
       x = expression(tau[sigma[E]^2]),
       subtitle = "a) Estimates",
       colour = "Score\nReliability") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12)) +
  scale_color_manual(values = cols[c(3:7)])

# p2, mean bias of error score variance heterogeneity estimates, grouped across levels of CV_E.
# CV_T is set to zero here
p2 <- df_comparison_summary %>% 
  filter(CVT == 0) %>% 
  mutate(mean_tau_varE = ifelse(CVE == 0, yes = (mean_tau_varE + (((rel -.7)*(-1))/4)), no = mean_tau_varE)) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 0) +
  geom_errorbar(aes(x = mean_tau_varE,
                    ymin = ul80_bias_tau_varE,
                    ymax = ll80_bias_tau_varE,
                    colour = as.factor(rel)),
                alpha = .7, width = .01, linewidth = 1) +
  geom_point(aes(x = mean_tau_varE, y = mean_bias_tau_varE, colour = as.factor(rel))) +
  facet_wrap(vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0", "CV[sigma[E]^2] == .1", "CV[sigma[E]^2] == .2", "CV[sigma[E]^2] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("mean bias in " ~ hat(tau)[sigma[E]^2]),
       x = expression(tau[sigma[E]^2]),
       subtitle = "b) Bias",
       colour = "Score\nReliability") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12)) +
  scale_color_manual(values = cols[c(3:7)])

# p3, assessment of effiency in error score variance heterogeneity estimates, grouped across levels of CV_E.
# CV_T is set to zero here
p3 <- df_comparison_summary %>% 
  filter(CVT == 0) %>% 
  ggplot() +
  geom_point(aes(x = mean_tau_varE, y = sqrt(var_tau_E), colour = as.factor(rel))) +
  facet_wrap(vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0", "CV[sigma[E]^2] == .1", "CV[sigma[E]^2] == .2", "CV[sigma[E]^2] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("efficiency in " ~ hat(tau)[sigma[E]^2]),
       x = expression(tau[sigma[E]^2]),
       subtitle = "c) Efficiency",
       colour = "Score\nReliability") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12)) +
  scale_color_manual(values = cols[c(3:7)])

# extract legend from figure 3, to add to the combined legend of all three graphics
pleg <- ggpubr::get_legend(p3)

# combine p1, p2 and p3.
parr <- ggpubr::ggarrange(p1, p2, p3, ncol = 3, common.legend = TRUE, legend = "bottom", 
                          legend.grob = pleg)

# Add title
annotate_figure(parr, top = text_grob(expression("Estimation quality in explicitly modelling " ~ tau[sigma[E]^2])))

# save graphic "figure 1" as .png with transparent background
ggsave(file = here("Graphics/figure1.png"),
       plot = last_plot(), 
       width = 10, 
       height = 8)




## generate estimates of type-I-error and power, for figures 2 and 3
# use significance level of .05
df_c_s_rate <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(Botella_sig = mean(p_Bonett_rel_Botella < .05, na.rm = T),
            varE_sig = mean(p_E < .05, na.rm = T),
            Bonnett_sig = mean(p_Bonett < .05, na.rm = T))

# store in a data-frame, suitable to make graphics
df_rate_vis <- data.frame(sig = c(df_c_s_rate$Botella_sig,
                                  df_c_s_rate$varE_sig,
                                  df_c_s_rate$Bonnett_sig),
                          method = as.factor(c(rep("Bot", length(df_c_s_rate$Botella_sig)),
                                               rep("VE", length(df_c_s_rate$Botella_sig)),
                                               rep("Bon", length(df_c_s_rate$Botella_sig)))),
                          CVT = df_c_s_rate$CVT,
                          CVE = df_c_s_rate$CVE,
                          rel = df_c_s_rate$rel) 

cols2 <- RColorBrewer::brewer.pal(9, "Greens")


# p4, at average score reliability of .5, estimates of type-I-error as grouped bar-charts
# CV_E is set to zero here
p4 <- df_rate_vis %>% 
  filter(CVE == 0, rel == .5) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  facet_wrap(vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))), 
             nrow = 1, scales = "fixed", labeller = label_parsed) +
  scale_fill_manual(values = cols2[c(3, 6, 9)]) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "#EAEAEA"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12))  +
  labs(x = "Method", y = "Type-I-Error",
       subtitle = "a) Score Reliability of .5") 

# p5, at average score reliability of .9, estimates of type-I-error as grouped bar-charts
# CV_E is set to zero here
p5 <- df_rate_vis %>% 
  filter(CVE == 0, rel == .9) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  facet_wrap(vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))), 
             nrow = 1, scales = "fixed", labeller = label_parsed) +
  scale_fill_manual(values = cols2[c(3, 6, 9)]) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "#EAEAEA"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12)) +
  labs(x = "Method", y = "Type-I-Error",
       subtitle = "b) Score Reliability of .9")

# extract legend from p4 to add to the combined graph
pleg2 <- ggpubr::get_legend(p4)

# combine graphics p4 and p5
parr2 <- ggpubr::ggarrange(p4, p5, ncol = 1, legend = "none")

# add title to figure 2
annotate_figure(parr2, top = text_grob("Type-I-Error comparison across methods"))

# save figure 2 as .png with mostly transparent background
ggsave(file =  here("Graphics/figure2.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)



# p6, at average score reliability of .5, estimates of power as grouped bar-charts
# Here, the plots are faceted along levels of CV_E and CV_T (CV_E == 0 is excluded)

p6 <- df_rate_vis %>% 
  filter(CVE != 0, rel == .5) %>% 
  mutate(CVE_n = paste0("CV[sigma[E]^2] == ", CVE),
         CVT_n = paste0("CV[sigma[T]^2] == ", CVT)) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  facet_grid(rows = vars(factor(CVE_n)),
             cols = vars(factor(CVT_n)),
             labeller = label_parsed) +
  scale_fill_manual(values = cols2[c(3, 6, 9)]) +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "#EAEAEA"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12)) +
  labs(x = "Method", y = "Power")

# add title to figure
annotate_figure(p6, top = "Power comparison across methods")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/figure3.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)





### Prepare Data-file for shiny-app

## Means
sim_aggregates_m <- df_comparison %>% 
  group_by(CVT, CVE, rel) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = T)))

## lower-limit of 95%-interval
sim_aggregates_ll <- df_comparison %>%
  group_by(CVT, CVE, rel) %>%
  summarise(across(everything(), \(x) quantile(x, .025, na.rm = T)))
names(sim_aggregates_ll) <- paste0(names(sim_aggregates_ll), "_ll")

## upper-limit of 95%-interval
sim_aggregates_ul <- df_comparison %>%
  group_by(CVT, CVE, rel) %>%
  summarise(across(everything(), \(x) quantile(x, .975, na.rm = T)))
names(sim_aggregates_ul) <- paste0(names(sim_aggregates_ul), "_ul")

# combine into single data-frame
sim_aggregates_full <- data.frame(sim_aggregates_m,
                                  sim_aggregates_ll,
                                  sim_aggregates_ul)

# store in csv
write.csv(sim_aggregates_full, here("Data/Shiny Data/Sim80000_rma_agg_df.csv"),
          row.names = FALSE)


