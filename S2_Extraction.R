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


# we didn't store parameters of all sim conditions, however, these are easily recovered:
CVT <- seq(from = 0, to = .3, by = .1)
CVE <- seq(from = 0, to = .3, by = .1)
rel <- seq(from = .1, to = .9, by = .2)

# combine 4*4*5 conditions
condition_combinations <- expand.grid(CVT, CVE, rel)
names(condition_combinations) <- c("CVT", "CVE", "rel")

# repeat 80 conditions, each 500 times
all_conditions <- NULL
# Large-Scale Simulation Scheme
for(i in 1:1000){
  all_conditions <- rbind(all_conditions, condition_combinations)
}


# load the simulation data
Large_Sim_Data <- readRDS(file = here("Simulation Data/Sim_80000_conditions.RData"))

# analysis of these is computationally heavy, therefore we again use parallelisation
#  replace 7 by the nr. of cores you want to use
plan(multisession, workers = 7)
# in default, future can only use about 500 mb of working memory. You can override this
#  using the following code. This allows for about 1000 mb of ram, which should be sufficient here
options(future.globals.maxSize = 1024*1024^2)

# take time of analysis (out of interest)
system.time(
  
# again, we loop over elements of our simulation object
Large_Sim_Data_RMA <- future_lapply((1:length(Large_Sim_Data)), FUN = function(it){
  
  # at times, calculation may break down as variance estimates were negative, which couldn't be further
  #  transformed. Therefore we use tryCatch, so the program does not break down
  tryCatch({
    
    # extract specific list.element
    x <- Large_Sim_Data[[it]]
    
    # perform Bonett-transformation
    Bonett_rel <- log(1 - x$rel)
    
    # calculate log-transformed observed score variance
    ln_varX <- log(x$varT + x$varE)
    
    # calculate standard error (variance) for log-transformed observed score variance
    Var_lnvarX <- 2/(100 - 1)
    
    # calculate standard error (variance) for Bonett-transformed score reliability
    Var_Bonett_rel <- (2*10)/(9*98)
    
    
    # perform meta-analyses on log-transformed score variance estimates
    # NOTE that tau_lnT is not actually used in the manuscript - this produces distorted estimates
    #  of heterogeneity. It is only computed here for comparison in the appendix
    tau_lnT <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = log(varT), sei = SE_T.b)
    tau_lnE <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = log(varE), sei = SE_E.b)
    
    # taurel is the analysis of score reliability estimates directly
    taurel <- metafor::rma(measure = "GEN", method = "REML", data = x, yi = rel, sei = ase)
    tau_lnX <- metafor::rma(measure = "GEN", method = "REML", yi = ln_varX, vi = Var_lnvarX)
    
    # meta-analysis of Bonett-transformed score reliability estimates
    rma_Bonett_rel_base <- metafor::rma(yi = Bonett_rel,
                                        vi = Var_Bonett_rel,
                                        measure = "GEN",
                                        method = "REML")
    
    # meta-analysis of Bonett-transformed score reliability estimates, as proposed by Botella & Suero
    #  this implies that log-transformed observed score variance is used as a predictor in a meta-
    #  regression model
    rma_Bonett_rel_Botella <- metafor::rma(yi = Bonett_rel,
                                           vi = Var_Bonett_rel,
                                           measure = "GEN",
                                           method = "REML",
                                           mods = ~ ln_varX)
    
    # compute estimates of mean score variance (component) back on the original scale
    muT <- exp(tau_lnT$b[1] + (.5*tau_lnT$tau2))
    muE <- exp(tau_lnE$b[1] + (.5*tau_lnE$tau2))
    muX <- exp(tau_lnX$b[1] + (.5*tau_lnX$tau2))
    
    # # compute estimates of mean score variance (component) back on the original scale
    # muT <- exp(tau_lnT$b[1]) + (.5*exp(tau_lnT$b[1])*tau_lnT$tau2)
    # muE <- exp(tau_lnE$b[1]) + (.5*exp(tau_lnE$b[1])*tau_lnE$tau2)
    # muX <- exp(tau_lnX$b[1]) + (.5*exp(tau_lnX$b[1])*tau_lnX$tau2)
    
    
    # tauT2 <- (muT^2) * (exp(tau_lnT$tau2) - 1)
    # tauE2 <- (muE^2) * (exp(tau_lnE$tau2) - 1)
    
    # tauT2 <- ((exp(tau_lnT$b[1])^2) * tau_lnT$tau2) + (.5*(exp(tau_lnT$b[1])^2)*(tau_lnT$tau2^2)) + ((exp(tau_lnT$b[1])^2) * (tau_lnT$tau2^2)) 
    # tauE2 <- ((exp(tau_lnE$b[1])^2) * tau_lnE$tau2) + (.5*(exp(tau_lnE$b[1])^2)*(tau_lnE$tau2^2)) + ((exp(tau_lnE$b[1])^2) * (tau_lnE$tau2^2)) 
    
    # compute estimates of heterogeneity in score variance (component) back on the original scale
    tauT2 <- (exp(tau_lnT$tau2) - 1) * exp((2 * tau_lnT$b[1]) + tau_lnT$tau2)
    tauE2 <- (exp(tau_lnE$tau2) - 1) * exp((2 * tau_lnE$b[1]) + tau_lnE$tau2)
    tauX2 <- (exp(tau_lnX$tau2) - 1) * exp((2 * tau_lnX$b[1]) + tau_lnX$tau2)
    
    
    # store estimates of heterogeneity in terms of tau, I^2, H^2, test-statistics of heterogeneity
    #  QE and its p-value, meta-analytic estimate of the mean and nr. of samples used in estimation
    #  in a data.frame. Do so for each parameter (transformed and untransformed)
    return(data.frame(tau_T = sqrt(tauT2),
                      tau_lnT = sqrt(tau_lnT$tau2),
                      tau_E = sqrt(tauE2),
                      tau_lnE = sqrt(tau_lnE$tau2),
                      tau_X = sqrt(tauX2),
                      tau_lnX = sqrt(tau_lnX$tau2),
                      tau_rel = sqrt(taurel$tau2),
                      tau_Bonnett = sqrt(rma_Bonett_rel_base$tau2),
                      tau_Bonett_rel_Botella = sqrt(rma_Bonett_rel_Botella$tau2),
                      I2_T = tau_lnT$I2,
                      I2_E = tau_lnE$I2,
                      I2_X = tau_lnX$I2,
                      I2_rel = taurel$I2,
                      I2_Bonnett = rma_Bonett_rel_base$I2,
                      I2_Bonnett_rel_Botella = rma_Bonett_rel_Botella$I2,
                      H2_T = tau_lnT$H2,
                      H2_E = tau_lnE$H2,
                      H2_X = tau_lnX$H2,
                      H2_rel = taurel$H2,
                      H2_Bonnett = rma_Bonett_rel_base$H2,
                      H2_Bonnett_rel_Botella = rma_Bonett_rel_Botella$H2,
                      QE_T = tau_lnT$QE,
                      QE_E = tau_lnE$QE,
                      QE_X = tau_lnX$QE,
                      QE_rel = taurel$QE,
                      QE_Bonnett = rma_Bonett_rel_base$QE,
                      QE_Bonnett_rel_Botella = rma_Bonett_rel_Botella$QE,
                      p_T = tau_lnT$QEp,
                      p_E = tau_lnE$QEp,
                      p_X = tau_lnX$QEp,
                      p_rel = taurel$QEp,
                      p_Bonett = rma_Bonett_rel_base$QEp,
                      p_Bonett_rel_Botella = rma_Bonett_rel_Botella$QEp,
                      mu_T = muT,
                      mu_lnT = tau_lnT$b[1],
                      mu_E = muE,
                      mu_lnE = tau_lnE$b[1],
                      mu_X = muX,
                      mu_lnX = tau_lnX$b[1],
                      mu_rel = taurel$b[1],
                      mu_Bonnett = rma_Bonett_rel_base$b[1],
                      mu_Bonnett_rel_Botella = rma_Bonett_rel_Botella$b[1],
                      k_T = tau_lnT$k,
                      k_E = tau_lnE$k,
                      k_X = tau_lnX$k,
                      k_rel = taurel$k,
                      k_Bonett = rma_Bonett_rel_base$k,
                      k_Bonett_rel_Botella = rma_Bonett_rel_Botella$k))
  }
  ,
  # print errors to the console
  error = function(e)(cat(""))
  
  )
})

)


# turn off parallelization
plan(sequential)

# currently for each sim-condition, an individual data.frame is stored in a separate list-element We
#  would prefer if this was a single matrix/data.frame. 
# Therefore, we initially prepare an empty (NA) matrix of the desired size
mat_rma <- matrix(NA, nrow = 80000, ncol = length(Large_Sim_Data_RMA[[3]]))


# looping over the list-elements, we store simulation analysis results in the rows of the matrix
for(i in 1:length(Large_Sim_Data_RMA)){
  
  # again, to avoid issued of empy list-elements due to negative variances, we wrap with trycatch
  tryCatch({
    mat_rma[i,] <- unlist(Large_Sim_Data_RMA[[i]])
  },
  # print errors to console
  error = function(e)(cat(""))
  )
 
}

# make data.frame from the matrix
df_rma <- as.data.frame(mat_rma)

# rename columns of data.frame, using the names we used in the list-elements
names(df_rma) <- names(Large_Sim_Data_RMA[[3]])

# store data.frame in a .csv file
write.csv(df_rma, here("Simulation Data/Sim80000_rma.csv"),
          row.names = FALSE)

# store list-object in a .RData-file
saveRDS(Large_Sim_Data_RMA, here("Simulation Data/Sim80000_rma.RData"))

