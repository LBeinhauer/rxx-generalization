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
packages <- c("dplyr", "here", "magrittr", "ggplot2", "ggpubr", "RColorBrewer", "grid")

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
df_comparison <- read.csv(here("Simulation Data/Sim1080_rma_df.csv"))

df_comparison_summary <- read.csv(here("Simulation Data/Sim1080_rma_df_agg.csv"))


cols <- RColorBrewer::brewer.pal(7, "Blues")

### Generate figures


############
# Figure 1 #
############

# mean bias of error score variance heterogeneity estimates, grouped across levels of CV_E.
# CV_T is set to zero here, k restricted to 12 and 60, CVE restricted to 0, .05, and .3

# build specific data-frame for figures 1 and 2
df_comparison_summary_d1 <- df_comparison_summary %>% 
  mutate(sim_tau_varE = ifelse(CVE == 0, yes = (10*(1-rel)*CVE + (((rel -.7)*(-1))/4)), no = 10*(1-rel)*CVE)) %>% 
  filter(CVT == 0 & CVE == 0 & k %in% c(12, 60))

cols <- RColorBrewer::brewer.pal(7, "Blues")

p1 <- df_comparison_summary_d1 %>% 
  ggplot() +
  geom_line(aes(x = sim_tau_varE, y = mean_bias_tau_varE, colour = as.factor(j))) +
  geom_point(aes(x = sim_tau_varE, y = mean_bias_tau_varE, colour = as.factor(j))) +
  facet_grid(rows = vars(factor(k, labels = c("K == 12", "K == 60"))),
             cols = vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0"))),
             labeller = label_parsed) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(y = expression("mean bias in " ~ hat(tau)[sigma[E]^2]),
       x = expression("Reliability"),
       colour = "J", subtitle = "a) no heterogeneity") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12),
        strip.text.y = element_blank()) +
  scale_color_manual(values = cols[c(3:7)]) +
  scale_x_continuous(breaks = c(-.05, -.025, 0, .025, .05), labels = c(.5, .6, .7, .8, .9)) +
  lims(y = c(-.03, .15))


df_comparison_summary_d2 <- df_comparison_summary %>% 
  mutate(sim_tau_varE = ifelse(CVE == 0, yes = (10*(1-rel)*CVE + (((rel -.7)*(-1))/4)), no = 10*(1-rel)*CVE)) %>% 
  filter(CVT == 0 & CVE %in% c(.05, .3) & k %in% c(12, 60))

p2 <- df_comparison_summary_d2 %>% 
  ggplot() +
  geom_line(aes(x = sim_tau_varE, y = mean_bias_tau_varE, colour = as.factor(j))) +
  geom_point(aes(x = sim_tau_varE, y = mean_bias_tau_varE, colour = as.factor(j))) +
  facet_grid(rows = vars(factor(k, labels = c("k == 12", "k == 60"))),
             cols = vars(factor(CVE, labels = c("CV[sigma[E]^2] == .05", "CV[sigma[E]^2] == .3"))),
             labeller = label_parsed, scales = "free_x") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(y = expression("mean bias in " ~ hat(tau)[sigma[E]^2]),
       x = expression(tau[sigma[E]^2]),
       colour = "J", subtitle = "b) heterogeneity present") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_color_manual(values = cols[c(3:7)]) +
  lims(y = c(-.03, .15))

arranged_plot1 <- ggpubr::ggarrange(p1, p2, common.legend = T, widths = c(1.2, 2), legend = "bottom")

annotate_figure(
  arranged_plot1,
  left = linesGrob(x = unit(c(38.2, 38.2), "npc"), 
                   y = unit(c(.1, .9), "npc"), 
                   gp = gpar(col = "black", lwd = 2, lty = 2))
)

# save graphic "figure 1" as .png with transparent background
ggsave(file = here("Graphics/figure1.png"),
       plot = last_plot(), 
       width = 10, 
       height = 4.5)


############
# Figure 2 #
############

# efficiency of error score variance heterogeneity estimates, grouped across levels of CV_E.
# CV_T is set to zero here, k restricted to 12 and 60, CVE restricted to 0, .05, and .3

p3 <- df_comparison_summary_d1 %>% 
  ggplot() +
  geom_line(aes(x = sim_tau_varE, y = sqrt(var_bias_tau_varE), colour = as.factor(j))) +
  geom_point(aes(x = sim_tau_varE, y = sqrt(var_bias_tau_varE), colour = as.factor(j))) +
  facet_grid(rows = vars(factor(k, labels = c("K == 12", "K == 60"))),
             cols = vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0"))),
             scales = "free_x", labeller = label_parsed) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(y = expression("sd (inefficiency) in " ~ hat(tau)[sigma[E]^2]),
       x = expression("Reliability"),
       colour = "J", subtitle = "a) no heterogeneity") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12),
        strip.text.y = element_blank()) +
  scale_color_manual(values = cols[c(3:7)]) +
  scale_x_continuous(breaks = c(-.05, -.025, 0, .025, .05), labels = c(.5, .6, .7, .8, .9)) +
  lims(y = c(0, .2))



p4 <- df_comparison_summary_d2 %>% 
  ggplot() +
  geom_line(aes(x = sim_tau_varE, y = sqrt(var_bias_tau_varE), colour = as.factor(j))) +
  geom_point(aes(x = sim_tau_varE, y = sqrt(var_bias_tau_varE), colour = as.factor(j))) +
  facet_grid(rows = vars(factor(k, labels = c("K == 12", "K == 60"))),
             cols = vars(factor(CVE, labels = c("CV[sigma[E]^2] == .05", "CV[sigma[E]^2] == .3"))),
             scales = "free_x", labeller = label_parsed) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  labs(y = expression("sd (inefficiency) in " ~ hat(tau)[sigma[E]^2]),
       x = expression(tau[sigma[E]^2]),
       colour = "J", subtitle = "b) heterogeneity present") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_color_manual(values = cols[c(3:7)]) +
  lims(y = c(0, .2))

arranged_plot2 <- ggpubr::ggarrange(p3, p4, common.legend = T, widths = c(1.2, 2), legend = "bottom")

annotate_figure(
  arranged_plot2,
  left = linesGrob(x = unit(c(38.2, 38.2), "npc"), 
                   y = unit(c(.1, .9), "npc"), 
                   gp = gpar(col = "black", lwd = 2, lty = 2))
)

# save graphic "figure 2" as .png with transparent background
ggsave(file = here("Graphics/figure2.png"),
       plot = last_plot(), 
       width = 10, 
       height = 4.5)


############
# Figure 3 #
############


# compute type-I-error rate for EV-MA, store in data-frame and pipe into ggplot
# CVE is fixed at zero and reliability restricted to .5 and .9. Other simulation parameters
#  are "free"

df_comparison %>% 
  group_by(CVT, CVE, rel, k, j) %>% 
  summarise(varE_sig = mean(p_varE < .05, na.rm = T)) %>% 
  filter(CVE == 0, rel %in% c(.5, .9), k %in% c(12, 33, 60)) %>% 
  ggplot() +
  geom_line(aes(x = CVT, y = varE_sig, colour = as.factor(j))) +
  geom_point(aes(x = CVT, y = varE_sig, colour = as.factor(j))) +
  facet_grid(rows = vars(factor(rel, labels = c("Rel. = .5", "Rel. = .9"))),
             cols = vars(factor(k, labels = c("K = 12", "K = 33", "K = 60")))) +
  lims(y = c(0, .2)) +
  geom_hline(aes(yintercept = .05), linetype = "dotted") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  labs(x = expression(CV[sigma["T"]^2]), y = "Type-I-Error Rate", colour = "J") +
  scale_color_manual(values = cols[c(3:7)])

# save graphic "figure 3" as .png with transparent background
ggsave(file = here("Graphics/figure3.png"),
       plot = last_plot(), 
       width = 10, 
       height = 4)


# average error rate across k
df_comparison %>% 
  group_by(CVT, CVE, rel, k, j) %>% 
  summarise(varE_sig = mean(p_varE < .05, na.rm = T)) %>% 
  filter(CVE == 0, rel %in% c(.5, .6, .7, .8, .9), k %in% c(12, 33, 60)) %>% 
  group_by(k) %>% 
  summarise(mean_varE_sig = mean(varE_sig))


############
# Figure 4 #
############


# compute Power for EV-MA, store in data-frame and pipe into ggplot
# CVT is restricted to 0 and .3 and reliability restricted to .5 and .9. CV_E is restricted
#  to all non-zero conditions. Other simulation parameters are "free"

df_comparison %>% 
  group_by(CVT, CVE, rel, k, j) %>% 
  summarise(varE_sig = mean(p_varE < .05, na.rm = T)) %>% 
  filter(CVE != 0, rel %in% c(.5, .9), k %in% c(12, 33, 60), CVT %in% c(0, .3)) %>% 
  ggplot() +
  geom_line(aes(x = CVE, y = varE_sig, colour = as.factor(j), linetype = as.factor(CVT))) +
  geom_point(aes(x = CVE, y = varE_sig, colour = as.factor(j), shape = as.factor(CVT))) +
  facet_grid(rows = vars(factor(rel, labels = c("Rel == .5", "Rel == .9"))),
             cols = vars(factor(k, labels = c("K == 12", "K == 33", "K == 60"))),
             labeller = label_parsed) +
  lims(y = c(0, 1)) +
  geom_hline(aes(yintercept = .95), linetype = "dotted") +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  labs(x = expression(CV[sigma[E]^2]), y = "Power", colour = "J",
       shape = expression(CV[sigma["T"]^2]), linetype = expression(CV[sigma["T"]^2])) +
  scale_color_manual(values = cols[c(3:7)])

# save graphic "figure 4" as .png with transparent background
ggsave(file = here("Graphics/figure4.png"),
       plot = last_plot(), 
       width = 10, 
       height = 4)




############
# Figure 5 #
############


# figure to illustrate RG-MA and B&S-MR's dependency on CVT and CVE, thereby demonstrating
#  that both do not assess error variance heterogeneity alone

## generate estimates of significance rates, across simulation conditions
df_c_s_rate <- df_comparison %>% 
  group_by(CVT, CVE, rel, k, j) %>% 
  summarise(Botella_sig = mean(p_rel_Bot < .05, na.rm = T),
            Bonnett_sig = mean(p_rel < .05, na.rm = T))

# store in a data-frame, suitable to make graphics
df_rate_vis <- data.frame(sig = c(df_c_s_rate$Botella_sig,
                                  df_c_s_rate$Bonnett_sig),
                          method = as.factor(c(rep("Bot", length(df_c_s_rate$Botella_sig)),
                                               rep("Bon", length(df_c_s_rate$Botella_sig)))),
                          CVT = df_c_s_rate$CVT,
                          CVE = df_c_s_rate$CVE,
                          rel = df_c_s_rate$rel,
                          k = df_c_s_rate$k,
                          j = df_c_s_rate$j) 

cols2 <- RColorBrewer::brewer.pal(9, "Greens")

df_rate_vis %>% 
  filter(j == 10, k == 60, CVE %in% c(0, .1, .2, .3), rel == .5) %>% 
  ggplot() +
  geom_line(aes(y = sig, x = CVT, colour = as.factor(method))) +
  geom_point(aes(y = sig, x = CVT, colour = as.factor(method))) +
  facet_wrap(vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0", "CV[sigma[E]^2] == .05", "CV[sigma[E]^2] == .1", "CV[sigma[E]^2] == .3"))),
             #cols = vars(factor(rel, labels = c("Rel. == .5", "Rel. == .7", "Rel. == .9"))),
             labeller = label_parsed, nrow = 1) +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", colour = "transparent"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.ticks = element_line(colour = "grey"),
        strip.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))  +
  scale_color_manual(labels = c("BRG-MA", "Botella's model"), values = cols2[c(5,7)]) +
  labs(x = expression(CV[sigma["T"]^2]), y = "Significance rate", colour = "Method")


# save graphic "figure 5" as .png with transparent background
ggsave(file = here("Graphics/figure5.png"),
       plot = last_plot(), 
       width = 10, 
       height = 3)





