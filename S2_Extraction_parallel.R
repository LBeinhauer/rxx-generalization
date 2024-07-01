### Reliability Generalization  ###

# ░██████╗██████╗░
# ██╔════╝╚════██╗
# ╚█████╗░░░███╔═╝
# ░╚═══██╗██╔══╝░░
# ██████╔╝███████╗
# ╚═════╝░╚══════╝

###################################################################################################
# This script is used to manipulate and extract the relevant data from the simulation data-file.  #
# Results are stored in .csv files                                                                #
###################################################################################################


# library loading and installing as necessary


# selected packages required for data manipulation
packages <- c("tidyverse", "here", "psych", "future.apply")

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


all_conditions <- readRDS(here("Simulation Data/full_seeds.RDS"))

full_df <- readRDS("C:/Users/Lukas/Downloads/OneDrive-2024-06-27/full_df.RDS")

full_df2 <- left_join(full_df, all_conditions[,-3], by = "seed")

#conditions <- readRDS("C:/Users/Lukas/Documents/Git_RStudio/rxx-generalization/Simulation Data/full_seeds.RDS")

# sim_file_paths <- list.files(here("Simulation Data/completed"), full.names = T)
# sim_file_seeds <- substr(list.files(here("Simulation Data/completed")), 1, nchar(list.files(here("Simulation Data/completed"))) - 4)

# Large_Sim_Data <- lapply(sim_file_paths[grep(".RDS$", sim_file_paths)], readRDS)
# names(Large_Sim_Data) <- sim_file_seeds

bt_var_m <- function(rma_obj){
  exp(rma_obj$b[1] + (.5*rma_obj$tau2))
}

bt_var_v <- function(rma_obj){
  (exp(rma_obj$tau2) - 1) * exp((2 * rma_obj$b[1]) + rma_obj$tau2)
}

var_Bonnett_backtransformed <- function(rma_obj){
  (((-exp(rma_obj$b[1]))^2) * rma_obj$tau2) + (.5*((-exp(rma_obj$b[1]))^2)*(rma_obj$tau2^2)) + ((-exp(rma_obj$b[1])) * (-exp(rma_obj$b[1])) * (rma_obj$tau2^2))
}

mean_Bonnett_backtransformed <- function(rma_obj){
  1 - exp(rma_obj$b[1]) + ((-exp(rma_obj$b[1])) / 2) * rma_obj$tau2
}


full_df_1sthalf <- full_df2[full_df2$seed %in% unique(full_df2$seed)[1:round(length(unique(full_df2$seed))/2)],]
full_df_2ndhalf <- full_df2[full_df2$seed %in% unique(full_df2$seed)[(round(length(unique(full_df2$seed))/2)+1):length(unique(full_df2$seed))],]
rm(full_df, full_df2)

# analysis of these is computationally heavy, therefore we again use parallelisation
#  replace 7 by the nr. of cores you want to use
plan(multisession, workers = 7)
# in default, future can only use about 500 mb of working memory. You can override this
#  using the following code. This allows for about 1000 mb of ram, which should be sufficient here
options(future.globals.maxSize = 3000*1024^2)


system.time(
  full_df_1sthalf_rma_L <- future_lapply(unique(full_df_1sthalf$seed), FUN = function(x){
    
    d <- full_df_1sthalf[full_df_1sthalf$seed == x, ]
    
    Var_Bonett_rel <- (2*d$j[1])/((d$j[1] - 1)*(100-2))
    
    Bonett_rel <- log(1 - d$rel)
    
    d_rma_E <- metafor::rma(yi = log(varE),
                            sei = SE_E.b,
                            measure = "GEN",
                            method = "REML",
                            data = d)
    
    d_rma_rel <- metafor::rma(yi = Bonett_rel,
                              vi = Var_Bonett_rel,
                              measure = "GEN",
                              method = "REML",
                              data = d)
    
    d_rma_rel_Bot <- metafor::rma(yi = Bonett_rel,
                                  vi = Var_Bonett_rel,
                                  measure = "GEN",
                                  method = "REML",
                                  mods = ~log(varE + varT),
                                  data = d)
    
    
    
    return(data.frame(mu_varE = bt_var_m(d_rma_E), 
                      tau_varE = sqrt(bt_var_v(d_rma_E)),
                      mu_rel = mean_Bonnett_backtransformed(d_rma_rel),
                      tau_rel = sqrt(var_Bonnett_backtransformed(d_rma_rel)),
                      mu_rel_Bot = mean_Bonnett_backtransformed(d_rma_rel_Bot),
                      tau_rel = sqrt(var_Bonnett_backtransformed(d_rma_rel_Bot)),
                      QE_varE = d_rma_E$QE,
                      QE_rel = d_rma_rel$QE,
                      QE_rel_Bot = d_rma_rel_Bot$QE,
                      p_varE = d_rma_E$QEp,
                      p_rel = d_rma_rel$QEp,
                      p_rel_Bot = d_rma_rel_Bot$QEp,
                      I2_varE = d_rma_E$I2,
                      I2_rel = d_rma_rel$I2,
                      I2_rel_Bot = d_rma_rel_Bot$I2,
                      H2_varE = d_rma_E$H2,
                      H2_rel = d_rma_rel$H2,
                      H2_rel_Bot = d_rma_rel_Bot$H2,
                      seed = x))
    
  })
)

full_df_1sthalf_rma_df <- do.call(rbind, full_df_1sthalf_rma_L)

write.csv(full_df_1sthalf_rma_df, "C:/Users/Lukas/Downloads/OneDrive-2024-06-27/full_df_rma_halfA.csv",
          row.names = FALSE)

plan(sequential)


rm(full_df_1sthalf)

plan(multisession, workers = 7)
# in default, future can only use about 500 mb of working memory. You can override this
#  using the following code. This allows for about 3000 mb of ram, which should be sufficient here
options(future.globals.maxSize = 3000*1024^2)

system.time(
  full_df_2ndhalf_rma_L <- future_lapply(unique(full_df_2ndhalf$seed), FUN = function(x){
    
    d <- full_df_2ndhalf[full_df_2ndhalf$seed == x, ]
    
    Var_Bonett_rel <- (2*d$j[1])/((d$j[1] - 1)*(100-2))
    
    Bonett_rel <- log(1 - d$rel)
    
    d_rma_E <- metafor::rma(yi = log(varE),
                            sei = SE_E.b,
                            measure = "GEN",
                            method = "REML",
                            data = d)
    
    d_rma_rel <- metafor::rma(yi = Bonett_rel,
                              vi = Var_Bonett_rel,
                              measure = "GEN",
                              method = "REML",
                              data = d)
    
    d_rma_rel_Bot <- metafor::rma(yi = Bonett_rel,
                                  vi = Var_Bonett_rel,
                                  measure = "GEN",
                                  method = "REML",
                                  mods = ~log(varE + varT),
                                  data = d)
    
    
    
    return(data.frame(mu_varE = bt_var_m(d_rma_E), 
                      tau_varE = sqrt(bt_var_v(d_rma_E)),
                      mu_rel = mean_Bonnett_backtransformed(d_rma_rel),
                      tau_rel = sqrt(var_Bonnett_backtransformed(d_rma_rel)),
                      mu_rel_Bot = mean_Bonnett_backtransformed(d_rma_rel_Bot),
                      tau_rel = sqrt(var_Bonnett_backtransformed(d_rma_rel_Bot)),
                      QE_varE = d_rma_E$QE,
                      QE_rel = d_rma_rel$QE,
                      QE_rel_Bot = d_rma_rel_Bot$QE,
                      p_varE = d_rma_E$QEp,
                      p_rel = d_rma_rel$QEp,
                      p_rel_Bot = d_rma_rel_Bot$QEp,
                      I2_varE = d_rma_E$I2,
                      I2_rel = d_rma_rel$I2,
                      I2_rel_Bot = d_rma_rel_Bot$I2,
                      H2_varE = d_rma_E$H2,
                      H2_rel = d_rma_rel$H2,
                      H2_rel_Bot = d_rma_rel_Bot$H2,
                      seed = x))
    
  })
)

full_df_2ndhalf_rma_df <- do.call(rbind, full_df_2ndhalf_rma_L)

write.csv(full_df_2ndhalf_rma_df, "C:/Users/Lukas/Downloads/OneDrive-2024-06-27/full_df_rma_halfB.csv",
          row.names = FALSE)

plan(sequential)
