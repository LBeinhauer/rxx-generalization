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
df_comparison <- read.csv(here("Simulation Data/Sim1080_rma_df.csv"))

df_comparison_summary <- read.csv(here("Simulation Data/Sim1080_rma_df_agg.csv"))


cols <- RColorBrewer::brewer.pal(7, "Blues")

### Generate figures


# p1, mean bias of error score variance heterogeneity estimates, grouped across levels of CV_E.
# CV_T is set to zero here, j to 3, k to 10, CVE restricted to 0, .05, .1 and .3
p1 <- df_comparison_summary %>% 
  filter(CVT == 0, j == 3, k == 12, CVE %in% c(0, .05, .1, .3)) %>% 
  mutate(sim_tau_varE = ifelse(CVE == 0, yes = (10*(1-rel)*CVE + (((rel -.7)*(-1))/4)), no = 10*(1-rel)*CVE)) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 0) +
  geom_errorbar(aes(x = sim_tau_varE,
                    ymin = ul80_bias_tau_varE,
                    ymax = ll80_bias_tau_varE,
                    colour = as.factor(rel)),
                alpha = .7, width = .01, linewidth = 1) +
  geom_point(aes(x = sim_tau_varE, y = mean_bias_tau_varE, colour = as.factor(rel))) +
  facet_wrap(vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0", "CV[sigma[E]^2] == .05", "CV[sigma[E]^2] == .1", "CV[sigma[E]^2] == .3"))), 
             nrow = 6, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("mean bias in " ~ hat(tau)[sigma[E]^2]),
       x = expression(tau[sigma[E]^2]),
       subtitle = "a) Bias",
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

# p2, assessment of effiency in error score variance heterogeneity estimates, grouped across levels of CV_E.
# CV_T is set to zero here, j to 3, k to 10, CVE restricted to 0, .05, .1 and .3
p2 <- df_comparison_summary %>% 
  filter(CVT == 0, j == 3, k == 12, CVE %in% c(0, .05, .1, .3)) %>% 
  ggplot() +
  geom_point(aes(x = 10*(1-rel)*CVE, y = sqrt(var_tau_varE), colour = as.factor(rel))) +
  facet_wrap(vars(factor(CVE, labels = c("CV[sigma[E]^2] == 0", "CV[sigma[E]^2] == .05", "CV[sigma[E]^2] == .1", "CV[sigma[E]^2] == .3"))), 
             nrow = 6, scales = "fixed", labeller = label_parsed) +
  labs(y = expression("efficiency in " ~ hat(tau)[sigma[E]^2]),
       x = expression(tau[sigma[E]^2]),
       subtitle = "b) Efficiency",
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

# extract legend from figure 2, to add to the combined legend of all three graphics
pleg <- ggpubr::get_legend(p2)


# combine p1 and p2
parr <- ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom", 
                          legend.grob = pleg)

# Add title
annotate_figure(parr)

# save graphic "figure 1" as .png with transparent background
ggsave(file = here("Graphics/figure1.png"),
       plot = last_plot(), 
       width = 10, 
       height = 8)




## generate estimates of type-I-error and power, for figures 2 and 3
# use significance level of .05
df_c_s_rate <- df_comparison %>% 
  group_by(CVT, CVE, rel, k, j) %>% 
  summarise(Botella_sig = mean(p_rel_Bot < .05, na.rm = T),
            varE_sig = mean(p_varE < .05, na.rm = T),
            Bonnett_sig = mean(p_rel < .05, na.rm = T))

# store in a data-frame, suitable to make graphics
df_rate_vis <- data.frame(sig = c(df_c_s_rate$Botella_sig,
                                  df_c_s_rate$varE_sig,
                                  df_c_s_rate$Bonnett_sig),
                          method = as.factor(c(rep("Bot", length(df_c_s_rate$Botella_sig)),
                                               rep("VE", length(df_c_s_rate$Botella_sig)),
                                               rep("Bon", length(df_c_s_rate$Botella_sig)))),
                          CVT = df_c_s_rate$CVT,
                          CVE = df_c_s_rate$CVE,
                          rel = df_c_s_rate$rel,
                          k = df_c_s_rate$k,
                          j = df_c_s_rate$j) 

cols2 <- RColorBrewer::brewer.pal(9, "Greens")


# p3, at average score reliability of .5 and .9, j = 3, k = 12, estimates of type-I-error as line-plots
# CV_E is set to zero here
p3 <- df_rate_vis %>% 
  filter(CVE == 0, rel %in% c(.5, .9), j == 3, k == 12) %>% 
  ggplot() +
  geom_line(aes(y = sig, x = CVT, colour = as.factor(method)),
            linewidth = 1.5) +
  facet_wrap(vars(factor(rel, label = c("Score Reliability = .5", "Score Reliability = .9")))) +
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
  scale_color_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  labs(x = expression(CV[sigma["T"]^2]), y = "Type-I-Error", colour = "Method")

# add title to figure
annotate_figure(p3)

# save figure 3 as .png with transparent background
ggsave(file =  here("Graphics/figure2.png"),
       plot = last_plot(), 
       width = 10, 
       height = 7)

# p3, at average score reliability of .5 and .9, j = 5, k in 12 and 60, estimates of power as line-plots
# CV_T is set to zero and .3, across levels of CV_E which are not zero
p4 <- df_rate_vis %>% 
  filter(CVT %in% c(0, .3), rel == .5, j == 5, k %in% c(12, 60), CVE != 0) %>% 
  ggplot() +
  geom_line(aes(y = sig, x = CVE, colour = as.factor(method), linetype = as.factor(CVT)),
            linewidth = 1.5) +
  # facet_grid(rows = vars(factor(j, labels = c("J = 3", "J = 10"))),
  #            cols = vars(factor(k, labels = c("K = 12", "K = 60"))),
  #            scales = "fixed") +
  facet_wrap(vars(factor(k, labels = c("K = 12", "K = 60")))) +
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
  scale_color_discrete(labels = c("RG-MA", "B&S", "EV-MA")) +
  # guides(linetype = "none") +
  labs(x = expression(CV[sigma[E]^2]), y = "Power", colour = "Method", linetype = expression(CV[sigma["T"]^2])) 


# Add title
annotate_figure(p4)

# save graphic "figure 1" as .png with transparent background
ggsave(file = here("Graphics/figure3.png"),
       plot = last_plot(), 
       width = 10, 
       height = 8)




