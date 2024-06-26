### Reliability Generalization  ###

# ░██████╗░░███╗░░
# ██╔════╝░████║░░
# ╚█████╗░██╔██║░░
# ░╚═══██╗╚═╝██║░░
# ██████╔╝███████╗
# ╚═════╝░╚══════╝

###################################################################################################
# This script is used to simulate data with heterogeneity in score reliability, true and error    #
#  score variance. Simulations are done on multiple cores using the R-library (future).           #
#                                                                                                 #
# Warning - this can be very resource-intensive and might occupy your cores for a long time       #
###################################################################################################


# library loading and installing as necessary


# selected packages required for analysis and simulation
packages <- c("tidyverse", "here", "boot", "MASS", 
              "spsUtil", "metafor", "future.apply")

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

# source function from function-script
source(here("RG_function-library.R"))


# simulation scheme:

# reliability .5 to .9 (steps of .1)
# CV_T 0 to .3 (steps of .1)
# CV_E 0 to .3 (steps of .1)
CVT <- seq(from = 0, to = .3, by = .1)
#CVE <- seq(from = 0, to = .3, by = .1)
CVE <- c(0, .01, .05, .1, .2, .3)
rel <- seq(from = .5, to = .9, by = .1)
k <- c(12, 33, 60)
j <- c(3, 5, 10)

# combine 4*4*5 conditions
condition_combinations <- expand.grid(CVT, CVE, rel, k, j)
names(condition_combinations) <- c("CVT", "CVE", "rel", "k", "j")

# repeat 240 conditions, each 1000 times
all_conditions <- NULL
# Large-Scale Simulation Scheme
for(i in 1:1000){
  all_conditions <- rbind(all_conditions, condition_combinations)
}



# call additional cores (make sure you have at least 1 core remaining, in case
#  you need to terminate the sessions), replace 7 by the nr. of cores you want to use
plan(multisession, workers = 7)


# set seed to make results reproducible
set.seed(040823)



# time for 20 replications at n = 100, k = 100, R = 3000 using 7 cores: ~270 seconds
## NOT ON POWER
# time for 1 replication at n = 100, k = c(12, 33, 60), R = 100, using 7 cores: 107 seconds
## NOT ON POWER
# exclusing saving of .cvs & RDS, no varT

# keep time while running simulation
system.time(
  Large_Sim_Data <- future_lapply(1:nrow(all_conditions[1:240,]), future.seed = TRUE, FUN = function(x){
    
    # simulate data, according to specific condition simulated at the time
      # additional information on sim_het_VC can be found in the function-library
    it.simdata <- sim_het_VC(j = 3, n = 100, k = all_conditions$k[x],
                             reliability = all_conditions$rel[x], mean_score = 0, 
                             mean_observed_var = 10,
                             CV_var_T = all_conditions$CVT[x],
                             CV_var_E = all_conditions$CVE[x],
                             empirical = TRUE)
    
    # bootstrap for error score variance
    b.data_E <- apply_Bootstrap_SE_nonspecific(it.simdata, var.component = "ERROR", R = 3000)
    # bootstrap for true score variance (leads to issues in estimation! This is only for com-
    #  parison in the appendix, not used for actual estimation in current framework!)
    # b.data_T <- apply_Bootstrap_SE_nonspecific(it.simdata, var.component = "TRUE", R = 3000)
    
    # collect estimates of Cronbach's Alpha and corresponding standard error (ase)
    a <- lapply(it.simdata, FUN = function(x){
      al <- spsUtil::quiet(psych::alpha(x))
      
      return(data.frame(rel = al$total$raw_alpha,
                        ase = al$total$ase))
    })
    
    # calculate sampled error score variance using
      # the earlier collected estimates of Cronbach's Alpha.
    varE <- sapply(it.simdata, FUN = function(x){var(rowMeans(x))}) * (1-sapply(a, FUN = function(x){x$rel}))
    varT <- sapply(it.simdata, FUN = function(x){var(rowMeans(x))}) * (sapply(a, FUN = function(x){x$rel}))
    
    # collect reliability and standard error from list object (in vector form)
    rel <- sapply(a, FUN = function(x){x$rel})
    ase <- sapply(a, FUN = function(x){x$ase})
    
    # prepare data frame to be returned by function
    df <- data.frame(varE.b = b.data_E$df.formatted$boot.mean,
                     SE_E.b = b.data_E$df.formatted$SE,
                     varE = varE,
                     # varT.b = b.data_T$df.formatted$boot.mean,
                     # SE_T.b = b.data_T$df.formatted$SE,
                     varT = varT,
                     rel = rel,
                     ase = ase)
    
    # collect results of Bootstrapping endeavours in seperate .csv-files
    # write.csv(df, here(paste0("Simulation Data/Simulation_sub_files/sim", x, ".csv")), row.names = FALSE)
    # collect simulated data (lists) in seperate .RData-files
    # saveRDS(it.simdata, here(paste0("Simulation Data/Data/", x, ".RData")))
    
    return(df)
    
  })
)

# turn off the multi-processing - free all other cores
plan(sequential)

## The following code might help you kill the cores running simulations, if anything goes wrong

# v <- listenv::listenv()
#
# # replace 15 by the number of cores you have selected!
# for(ii in 1:15){
#   v[[ii]] %<-% {
#     Sys.getpid()
#   }
# }
#
# # replace 15 by the number of cores you have selected!
# for(i in 1:15){
#   system(sprintf("kill -9 %s", v[[i]]))
# }

# store list-object in .RData file
saveRDS(Large_Sim_Data, file = here("Simulation Data/Sim_80000_conditions.RData"))



