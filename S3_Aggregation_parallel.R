### Reliability Generalization  ###


# ░██████╗██████╗░
# ██╔════╝╚════██╗
# ╚█████╗░░█████╔╝
# ░╚═══██╗░╚═══██╗
# ██████╔╝██████╔╝
# ╚═════╝░╚═════╝░

###################################################################################################
# This script is used to back-transform estimates and aggregate results from the simulated data   #
#  file                                                                                           #
###################################################################################################


# library loading and installing as necessary


# selected packages required for data manipulation
packages <- c("dplyr", "here", "magrittr")

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



# load in simulated data
DF_rma1 <- read.csv(here("Simulation Data/full_df_rma_halfA.csv")) %>% 
  rename(tau_rel_Bot = tau_rel.1)

DF_rma2 <- read.csv(here("Simulation Data/full_df_rma_halfB.csv")) %>% 
  rename(tau_rel_Bot = tau_rel.1)

DF_rma <- rbind(DF_rma1, DF_rma2)

rm(DF_rma1, DF_rma2)

all_conditions <- readRDS(here("Simulation Data/full_seeds.RDS"))

full_DF_rma <- left_join(DF_rma, all_conditions, by = "seed") %>% 
  mutate(sim_mu_varE = (1-rel) * 10) %>% 
  mutate(sim_tau_varE = sim_mu_varE * CVE) %>% 
  mutate(bias_mu_rel = mu_rel - rel,
         bias_mu_varE = mu_varE - sim_mu_varE,
         bias_tau_varE = tau_varE - sim_tau_varE)


cols_to_avg <- c("mu_varE", "tau_varE", "mu_rel", "tau_rel", 
                 "mu_rel_Bot", "tau_rel_Bot", "bias_mu_rel",
                 "bias_mu_varE", "bias_tau_varE")

cols_to_group <- names(all_conditions)[-6]

df_comparison_means <- full_DF_rma %>% 
  group_by(across(cols_to_group)) %>% 
  summarise(across(cols_to_avg, \(x) mean(x, na.rm = T)))
names(df_comparison_means) <- c(cols_to_group, 
                                paste0("mean_", names(df_comparison_means)[!names(df_comparison_means) %in% cols_to_group]))

# generate estimates of lower level of 80%-estimation interval
df_comparison_80ll <- full_DF_rma %>% 
  group_by(across(cols_to_group)) %>% 
  summarise(across(cols_to_avg, \(x) quantile(x, probs = .1, na.rm = T)))
names(df_comparison_80ll) <- c(cols_to_group, 
                               paste0("ll80_", names(df_comparison_80ll)[!names(df_comparison_80ll) %in% cols_to_group]))

# generate estimates of upper level of 80%-estimation interval
df_comparison_80ul <- full_DF_rma %>% 
  group_by(across(cols_to_group)) %>% 
  summarise(across(cols_to_avg, \(x) quantile(x, probs = .9, na.rm = T)))
names(df_comparison_80ul) <- c(cols_to_group, 
                               paste0("ul80_", names(df_comparison_80ul)[!names(df_comparison_80ul) %in% cols_to_group]))

# generate estimates of estimates' variance within each condition (to assess efficiency)
df_comparison_vars <- full_DF_rma %>% 
  group_by(across(cols_to_group)) %>% 
  summarise(across(cols_to_avg, \(x) var(x, na.rm = T)))
names(df_comparison_vars) <- c(cols_to_group, 
                               paste0("var_", names(df_comparison_vars)[!names(df_comparison_vars) %in% cols_to_group]))

# combine the data-sets
df_comparison_summary <- data.frame(df_comparison_means,
                                    df_comparison_80ll[,!names(df_comparison_80ll) %in% cols_to_group],
                                    df_comparison_80ul[,!names(df_comparison_80ul) %in% cols_to_group],
                                    df_comparison_vars[,!names(df_comparison_vars) %in% cols_to_group])


write.csv(full_DF_rma, here("Simulation Data/Sim1080_rma_df.csv"),
          row.names = FALSE)

write.csv(df_comparison_summary, here("Simulation Data/Sim1080_rma_df_agg.csv"),
          row.names = FALSE)


