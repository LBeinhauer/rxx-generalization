# Simulation - multi-core



# selected packages required for analysis
packages <- c("tidyverse", "here", "psych", "coefficientalpha", "boot", "MASS", 
              "truncnorm", "spsUtil", "metafor", "meta", "bayesmeta", "future.apply")

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


## Heterogeneity in True Variance

# takes about .15 seconds for a single run of 1000 samples.

system.time(
  test1 <- sim_het_VC(j = 10, n = 100, k = 100,
                      reliability = .5, mean_score = 0, 
                      mean_observed_var = 10,
                      CV_var_T = .2,
                      CV_var_E = .1,
                      tau_var_T = 0,
                      tau_var_E = 0)
)


# takes about 16 seconds to decompose variance & generate bootstrapped SE for 100(!) samples 
#  (with 3000 bootstrapped samples each)


system.time(
  long_test_T <- apply_Bootstrap_SE_nonspecific(test1$sim_data.L, var.component = "TRUE", R = 3000)
)


plan(multisession, workers = 7)

# takes about 12 seconds instead on 7 cores

system.time(
  long_test_T <- future_apply_Bootstrap_SE_nonspecific(test1$sim_data.L, var.component = "TRUE", R = 3000)
)

plan(sequential)



# simulation scheme:

# reliability .1 to .9
# CV_T 0 to .3
# CV_E 0 to .3
CVT <- seq(from = 0, to = .3, by = .1)
CVE <- seq(from = 0, to = .3, by = .1)
rel <- seq(from = .1, to = .9, by = .2)

# combine 4*4*5 conditions
condition_combinations <- expand.grid(CVT, CVE, rel)
names(condition_combinations) <- c("CVT", "CVE", "rel")

# repeat 80 conditions, each 1000 times
all_conditions <- NULL
# Large-Scale Simulation Scheme
for(i in 1:1000){
  all_conditions <- rbind(all_conditions, condition_combinations)
}

# 
# completion_grid <- matrix(0, nrow = nrow(all_conditions), ncol = 1)
# 
# write.csv(completion_grid, here("Notes/completion_grid.csv"), row.names = FALSE)
# write.csv(completion_grid, here("Notes/working_on_grid.csv"), row.names = FALSE)
# 
# 
# 
# 
# 
# bingo_apply <- function(cells, completion_grid_path, working_on_grid_path){
#   # pull info, which cells completed
#   comp_grid <- read.csv(completion_grid_path)
#   
#   # assess, whether anything still needs to be computed
#   if(sum(comp_grid == 0) == 0){
#     stop("All cells completed")
#   }
#   
#   # pull info, which cells are worked on atm
#   working_grid <- read.csv(working_on_grid_path)
#   
#   # combine the infos
#   both_grid <- cbind(comp_grid, working_grid)
#   
#   
#   if(length(rowSums(both_grid) == 0) == 0){
#     stop("Either all cells completed, or all remaining cells already worked on")
#   }
#   
#   # determine, which cell should be worked on now
#   it <- which(rowSums(both_grid) == 0)[1]
#   
#   # update working_grid
#   working_grid[it,] <- 1
#   working_grid_updated <- working_grid
#   
#   # update info, which cells are worked on atm
#   write.csv(working_grid_updated, file = working_on_grid_path, row.names = FALSE)
#   
#   
#   # do action
#   
#   
#   
#   # re-assess file, which have been completed so far
#   comp_grid_end <- read.csv(completion_grid_path)
#   
#   # update completion grid
#   comp_grid_updated <- comp_grid_end
#   comp_grid_updated[it,] <- 1
#   
#   # update info, which cells have been completed
#   write.csv(comp_grid_updated, file = completion_grid_path, row.names = FALSE)
#   
#   # re-assess file, which is being worked on atm
#   working_grid_end <- read.csv(working_on_grid_path)
#   
#   
#   # update working_grid
#   working_grid_end[it,] <- 0
#   
#   # update info, which cells are worked on atm
#   write.csv(working_grid_end, file = working_on_grid_path, row.names = FALSE)
# }
# 
# 
# 
# 
# 


# call additional cores (make sure you have at least 1 core remaining, in case
  # you need to terminate the sessions)
plan(multisession, workers = 7)



set.seed(050922)



# time for 20 replications at n = 100, k = 100, R = 3000 using 7 cores: 280 seconds


# sim_apply_boot <- function(condition_row, seed)



# keep time while running simulation
system.time(
  Large_Sim_Data <- future_lapply(1:nrow(all_conditions[1:20,]), future.seed = TRUE, FUN = function(x){
    
    # simulate data, according to specific condition simulated at the time
      # additional information on sim_het_VC can be found in the function-library
    it.simdata <- sim_het_VC(j = 10, n = 100, k = 100,
                             reliability = all_conditions$rel[x], mean_score = 0, 
                             mean_observed_var = 10,
                             CV_var_T = all_conditions$CVT[x],
                             CV_var_E = all_conditions$CVE[x],
                             empirical = FALSE)
    
    # bootstrap separately for true score and error score variance
    b.data_T <- apply_Bootstrap_SE_nonspecific(it.simdata$sim_data.L, var.component = "TRUE", R = 3000)
    b.data_E <- apply_Bootstrap_SE_nonspecific(it.simdata$sim_data.L, var.component = "ERROR", R = 3000)
    
    # collect estimates of Cronbach's Alpha and corresponding standard error (ase)
    a <- lapply(it.simdata$sim_data.L, FUN = function(x){
      al <- spsUtil::quiet(psych::alpha(x))
      
      return(data.frame(rel = al$total$raw_alpha,
                        ase = al$total$ase))
    })
    
    # calculate sample true score variance and error score variance using
      # the earlier collected estimates of Cronbach's Alpha.
    varT <- sapply(it.simdata$sim_data.L, FUN = function(x){var(rowMeans(x))}) * sapply(a, FUN = function(x){x$rel})
    varE <- sapply(it.simdata$sim_data.L, FUN = function(x){var(rowMeans(x))}) * (1-sapply(a, FUN = function(x){x$rel}))
    
    # collect reliability and standard error from list object (in vector form)
    rel <- sapply(a, FUN = function(x){x$rel})
    ase <- sapply(a, FUN = function(x){x$ase})
    
    # prepare data frame to be returned by function
    df <- data.frame(varT.b = b.data_T$boot.mean,
                     SE_T.b = b.data_T$SE,
                     varE.b = b.data_E$boot.mean,
                     SE_E.b = b.data_E$SE,
                     varT = varT,
                     varE = varE,
                     rel = rel,
                     ase = ase)
    
    
    # write.csv(df, here(paste0("Notes/Simulation_sub_files/sim", it, ".csv")), row.names = FALSE)
    
    
    return(df)
    
  })
)


plan(sequential)

# v <- listenv::listenv()
# for(ii in 1:15){
#   v[[ii]] %<-% {
#     Sys.getpid()
#   }
# }
# for(i in 1:15){
#   system(sprintf("kill -9 %s", v[[i]]))
# }


saveRDS(Large_Sim_Data, file = here("Notes/Sim_80000_conditions.RData"))



