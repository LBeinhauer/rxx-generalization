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
packages <- c("tidyverse", "boot", "MASS", "psych", "here",
              "spsUtil", "doParallel", "foreach", "Rmpi", "doMPI"
              )

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
# source(here("RG_function-library.R"))

# Load packages to parallelize jobs
library("Rmpi", lib.loc = "/beegfs/project/p2266/02_lars/r-libraries")
library("doMPI", lib.loc = "/beegfs/project/p2266/02_lars/r-libraries")

# Set up for MPI Cluster (Parallelization set up for 72 threads in each of the 10 notes)
# cl <- startMPIcluster()
# registerDoMPI(cl)


# threads <- 72*10


# simulation scheme:

CVT <- seq(from = 0, to = .3, by = .1)
CVE <- c(0, .01, .05, .1, .2, .3)
rel <- seq(from = .5, to = .9, by = .1)
k <- c(12, 33, 60)
j <- c(3, 5, 10)

# combine 4*6*5*3*3 conditions (1080)
condition_combinations <- expand.grid(CVT, CVE, rel, k, j)
names(condition_combinations) <- c("CVT", "CVE", "rel", "k", "j")

# repeat 1080 conditions, each 1000 times
all_conditions <- NULL
# Large-Scale Simulation Scheme
for(i in 1:1000){
  all_conditions <- rbind(all_conditions, condition_combinations)
}

set.seed(200624)
seeds <- sample(1:1e7, size = nrow(all_conditions))

all_conditions$seed <- seeds

df_all <- all_conditions


ProjectPath <-  here()

saveRDS(all_conditions, file = paste0(ProjectPath, "/Simulation Data/", "full_seeds.RDS"))

used_ids <- gsub(".csv", "", list.files(paste0(ProjectPath, "/Simulation Data/", "completed")))


# Read data
df_all <- readRDS(paste0(ProjectPath, "/Simulation Data/", "full_seeds.RDS"))

# Number of unique IDs for simulation
used_ids <- gsub(".csv", "", list.files(paste0(ProjectPath, "/Simulation Data/", "completed")))

# Subset data
df_all <- df_all[!df_all$seed %in% used_ids, ]

# Get IDs for uncompleted simulations
unique_ids <- df_all$seed


# registerDoParallel(cores=threads)
threads <- 7

cl <- makeCluster(threads)
registerDoParallel(cl)

system.time(
  
  foreach(
    index = 1:threads,
    .combine = "rbind",
    .errorhandling = "remove",
    .inorder = FALSE,
    .packages = packages,
    .export = c("ProjectPath", "df_all", "unique_ids", "threads")
  ) %dopar% {
    
    
    for(j in 1:round((length(unique_ids)/threads))){
      
      source(here("RG_function-library.R"))
      library(boot)
      library(MASS)
      library(psych)
      
      used_ids <- gsub(".RDS", "", list.files(paste0(ProjectPath, "/Simulation Data/", "temp/")))
      
      # Subset data to include only simulation IDs that have not been used
      df_all <- df_all[!df_all$seed %in% used_ids, ]
      
      # Extract IDs for simulation
      ids <- df_all$seed
      
      # Stop if all IDs have been used for simulation
      if(length(ids) == 0) {
        stop("job completed")
      }
      
      # Choose ID for simulation
      condition_id <- sample(ids, 1)
      
      # Extract data containing all information regarding the setup of the simulation for the current condition
      df_eval <- df_all[df_all$seed %in% condition_id, ]
      
      # Store temporary data to smooth simulation in parallel processing. This temporary data will inform other workers about the currently running simulations.
      saveRDS(
        NULL,
        paste0(
          ProjectPath,
          "/Simulation Data/",
          "temp/",
          condition_id,
          ".RDS"
        )
      )
      
      
      set.seed(df_eval$seed)
      
      it.simdata <- sim_het_VC(j = df_eval$j, n = 100, k = df_eval$k,
                               reliability = df_eval$rel, mean_score = 0, 
                               mean_observed_var = 10,
                               CV_var_T = df_eval$CVT,
                               CV_var_E = df_eval$CVE,
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
      
      saveRDS(df, paste0(ProjectPath, "/Simulation Data/", "completed/", df_eval$seed, ".RDS")) 
      
    }
    
  }
  
)

# saveRDS(Large_Sim_Data, file = here("Simulation Data/Sim_80000_conditions.RData"))



