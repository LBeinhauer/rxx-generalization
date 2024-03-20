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
packages <- c("tidyverse", "here", "metafor", "future.apply", "ggpubr", "RColorBrewer")

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


cols <- RColorBrewer::brewer.pal(7, "Blues")
cols2 <- RColorBrewer::brewer.pal(9, "Greens")

### Generate figures

# p1, individual estimates of true score variance heterogeneity, grouped across levels of CV_T.
# CV_E is set to zero here
p1 <- df_comparison %>% 
  filter(CVE == 0) %>% 
  ggplot(aes(y = tau_T, x = tau_varT, colour = as.factor(rel))) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(position = position_jitter(width = .03), 
             alpha = .01) +
  facet_wrap(vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression(hat(tau)[sigma[T]^2]),
       x = expression(tau[sigma[T]^2]),
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



# p2, mean bias of true score variance heterogeneity estimates, grouped across levels of CV_T.
# CV_E is set to zero here
p2 <- df_comparison_summary %>% 
  filter(CVE == 0) %>% 
  mutate(mean_tau_varT = ifelse(CVT == 0, yes = (mean_tau_varT + (((rel -.7)*(-1))/4)), no = mean_tau_varT)) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 0) +
  geom_errorbar(aes(x = mean_tau_varT,
                    ymin = ul80_bias_tau_varT,
                    ymax = ll80_bias_tau_varT,
                    colour = as.factor(rel)),
                alpha = .7, width = .01, linewidth = 1) +
  geom_point(aes(x = mean_tau_varT, y = mean_bias_tau_varT, colour = as.factor(rel))) +
  facet_wrap(vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("mean bias in " ~ hat(tau)[sigma[T]^2]),
       x = expression(tau[sigma[T]^2]),
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


# p3, assessment of effiency in true score variance heterogeneity estimates, grouped across levels of CV_T.
# CV_e is set to zero here
p3 <- df_comparison_summary %>% 
  filter(CVE == 0) %>% 
  ggplot() +
  geom_point(aes(x = mean_tau_varT, y = sqrt(var_tau_T), colour = as.factor(rel))) +
  facet_wrap(vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))), 
             nrow = 4, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("efficiency in " ~ hat(tau)[sigma[T]^2]),
       x = expression(tau[sigma[T]^2]),
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



pleg <- ggpubr::get_legend(p3)

# combine p1, p2 and p3.
parr <- ggpubr::ggarrange(p1, p2, p3, ncol = 3, common.legend = TRUE, legend = "bottom", 
                          legend.grob = pleg)

# Add title
annotate_figure(parr, top = text_grob(expression("Estimation quality in explicitly modelling " ~ tau[sigma['T']^2])))

# save graphic "figure 1" as .png with transparent background
ggsave(file = here("Graphics/appendix_fig1.png"),
       plot = last_plot(), 
       width = 10, 
       height = 8)




p4 <- df_comparison %>% 
  mutate(CVT_n = paste0("CV[sigma[T]^2] == ", CVT),
         CVE_n = paste0("CV[sigma[E]^2] == ", CVE)) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(y = tau_E, x = tau_varE, colour = as.factor(rel)),
             position = position_jitter(width = .03), 
             alpha = .01) +
  facet_grid(cols = vars(factor(CVT_n)),
             rows = vars(factor(CVE_n)),
             scales = "fixed", labeller = label_parsed) +
  labs(y = expression(hat(tau)[EV]),
       x = expression(tau[EV]),
       colour = "Score\nReliability") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text.x.top = element_text(size = 12),
        strip.text.y.right = element_text(size = 12)) +
  scale_color_manual(values = cols[c(3:7)])
  
# add title to figure
annotate_figure(p4, top = "a) Estimates - wide")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig2.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)
  
p5 <- df_comparison_summary %>% 
  mutate(CVT_n = paste0("CV[sigma[T]^2] == ", CVT),
         CVE_n = paste0("CV[sigma[E]^2] == ", CVE)) %>% 
  mutate(mean_tau_varE = ifelse(CVE == 0, yes = (mean_tau_varE + (((rel -.7)*(-1))/4)), no = mean_tau_varE)) %>% 
  ggplot() +
  geom_point(aes(x = mean_tau_varE, y = mean_bias_tau_varE, colour = as.factor(rel))) +
  geom_abline(intercept = 0, slope = 0) +
  facet_grid(cols = vars(factor(CVT_n)),
             rows = vars(factor(CVE_n)),
             scales = "fixed", labeller = label_parsed) +
  geom_errorbar(aes(x = mean_tau_varE,
                    ymin = ul80_bias_tau_varE,
                    ymax = ll80_bias_tau_varE,
                    colour = as.factor(rel)),
                alpha = .7, width = .01, linewidth = 1) +
  labs(y = expression(hat(tau)[EV]),
       x = expression(tau[EV]),
       colour = "Score\nReliability") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text.x.top = element_text(size = 12),
        strip.text.y.right = element_text(size = 12)) +
  scale_color_manual(values = cols[c(3:7)])

# add title to figure
annotate_figure(p5, top = "b) Mean Bias - wide")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig3.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)



p6 <- df_comparison_summary %>% 
  mutate(CVT_n = paste0("CV[sigma[T]^2] == ", CVT),
         CVE_n = paste0("CV[sigma[E]^2] == ", CVE)) %>% 
  ggplot() +
  geom_point(aes(x = mean_tau_varE, y = sqrt(var_tau_E), colour = as.factor(rel))) +
  geom_abline(intercept = 0, slope = 0) +
  facet_grid(cols = vars(factor(CVT_n)),
             rows = vars(factor(CVE_n)),
             scales = "fixed", labeller = label_parsed) +
  labs(y = expression(hat(tau)[EV]),
       x = expression(tau[EV]),
       colour = "Score\nReliability") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text.x.top = element_text(size = 12),
        strip.text.y.right = element_text(size = 12)) +
  scale_color_manual(values = cols[c(3:7)])

# add title to figure
annotate_figure(p6, top = "c) Efficiency - wide")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig4.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)





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



# p4, at average score reliability of .5, estimates of type-I-error as grouped bar-charts
# CV_E is set to zero here
p7 <- df_rate_vis %>% 
  filter(CVE == 0) %>% 
  mutate(CVT_n = paste0("CV[sigma[T]^2] == ", CVT),
         rel_n = paste0("rho[XX] == ", rel)) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  facet_grid(cols = vars(factor(CVT_n)),
             rows = vars(factor(rel_n)), 
             scales = "fixed", labeller = label_parsed) +
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
        strip.text.x.top = element_text(size = 12),
        strip.text.y.right = element_text(size = 12))  +
  labs(x = "Method", y = "Type-I-Error")

# add title to figure
annotate_figure(p7, top = "Type-I-Error")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig5.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)


# p6, at average score reliability of .5, estimates of power as grouped bar-charts
# Here, the plots are faceted along levels of CV_E and CV_T (CV_E == 0 is excluded)
p8 <- df_rate_vis %>% 
  filter(CVE != 0, rel == .5) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  facet_grid(rows = vars(factor(CVE, labels = c("CV[EV] == .1", "CV[EV] == .2", "CV[EV] == .3"))),
             cols = vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))),
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
annotate_figure(p8, top = "a) score reliability of .5")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig6.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)


p9 <- df_rate_vis %>% 
  filter(CVE != 0, rel == .6) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  facet_grid(rows = vars(factor(CVE, labels = c("CV[EV] == .1", "CV[EV] == .2", "CV[EV] == .3"))),
             cols = vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))),
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
annotate_figure(p9, top = "b) score reliability of .6")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig7.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)


p10 <- df_rate_vis %>% 
  filter(CVE != 0, rel == .7) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  facet_grid(rows = vars(factor(CVE, labels = c("CV[EV] == .1", "CV[EV] == .2", "CV[EV] == .3"))),
             cols = vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))),
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
annotate_figure(p10, top = "c) score reliability of .7")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig8.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)



p11 <- df_rate_vis %>% 
  filter(CVE != 0, rel == .8) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  facet_grid(rows = vars(factor(CVE, labels = c("CV[EV] == .1", "CV[EV] == .2", "CV[EV] == .3"))),
             cols = vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))),
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
annotate_figure(p11, top = "d) score reliability of .8")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig9.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)



p12 <- df_rate_vis %>% 
  filter(CVE != 0, rel == .9) %>% 
  ggplot() +
  geom_bar(aes(x = method, y = sig, fill = method), stat = "identity", colour = "black") +
  geom_text(aes(x = method, y = sig, label = paste0(round(sig*100, 0), "%")), vjust = -.1) +
  scale_y_continuous(labels = paste0(c(0, .25, .5, .75, 1) * 100, "%"), limits = c(0, 1.02)) +
  scale_x_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  scale_fill_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  facet_grid(rows = vars(factor(CVE, labels = c("CV[EV] == .1", "CV[EV] == .2", "CV[EV] == .3"))),
             cols = vars(factor(CVT, labels = c("CV[sigma[T]^2] == 0", "CV[sigma[T]^2] == .1", "CV[sigma[T]^2] == .2", "CV[sigma[T]^2] == .3"))),
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
annotate_figure(p12, top = "e) score reliability of .9")

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/appendix_fig10.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)
