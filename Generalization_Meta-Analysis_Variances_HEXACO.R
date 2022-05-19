### Reliability Generalization HEXACO - Analysis of Error & True Variance ###

## 18/05/2022




###################################################################################################
# This script is used for meta-analysis of the reliability estimates generated previously.        #
###################################################################################################


# library loading and installing as necessary


# relevant R packages
packages <- c("metafor", "tidyverse", "here", "data.table", "lavaan", "boot")

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



source(here("ReliabilityFunctions_RG.R"))

# only run if you wish to recompute the reliability estimates!
# source(here("Generalization_Initialization_HEXACO.R"))
source(here("Loading_Estimates_HEXACO.R"))


Reg_prep <- read.csv(here("Meta-Regression/meta_regression_dat_HEXACO.csv"))








### HEXACO ###

### RRR10 cheating Priming - (Mazar, Amir & Ariely, 2008)

# Thankfully, for the RRR10 project, the data can be taken from a file directly:
pc_df <- as.data.frame(fread(here("Data/RRR10/raw_data_corrected_MAA.csv")))

# data <- data[which(data$age >= 18 & data$age <= 25),]
# retain observations only for participants eligible for analysis
pc_df <- pc_df[which(pc_df$inclusion == "inclusion both RRR" | pc_df$inclusion == "inclusion Mazar only"),] %>%
  mutate(source = lab.name)


pc_df$source[which(pc_df$source == unique(pc_df$source)[8])] <- "Gonzalez-Iraizoz"
pc_df$source[which(pc_df$source == unique(pc_df$source)[16])] <- "Ozdogru"


# identify relevant labs for analsysis
labs_in_paper <- c("Laine", "klein Selle & Rozmann", "Aczel", "Ferreira-Santos", "Meijer", "Loschelder", "Wick", "Suchotzki", 
                   "Sutan", "Vanpaemel", "Verschuere", "Wiggins", "Gonzalez-Iraizoz", "Koppel", "Birt", "McCarthy", "Evans", 
                   "Holzmeister", "Ozdogru")
labs_in_data <- unique(pc_df$source)
# labs_in_data[8] <- "Gonzalez-Iraizoz"
# labs_in_data[16] <- "Ozdogru"

# remove labs from data, which we do not need for analysis
labs_excl <- labs_in_data[!labs_in_data %in% labs_in_paper]
pc_df <- pc_df[which(!pc_df$source %in% labs_excl),]



# include only participants in cheat condition (design was 2x2, cheat - no cheat x commandment - books)
pc_df <- pc_df[which(pc_df$maz.cheat.cond == "cheat"),]

names(pc_df)

# retain only those columns, which are needed for subsequent analysis.
pc_df <- pc_df[,c(which(names(pc_df) %in% c("inclusion", "maz.prime.cond", "maz.cheat.cond", "source",
                                            "compensation", "language", "major", "gender", "age")),
                  grep("^hex", names(pc_df)))]













# recoding the hexaco items, that need recoding

for(i in grep("^hex", names(pc_df))){
  pc_df[,i] <- as.integer(pc_df[,i])
}
# these are the numbers of the items, that need recoding
items_hex_recode <- c(30, 12, 60, 42, 24, 28, 53, 35, 41, 59, 28, 52, 10, 46, 9, 15, 57, 21, 26, 32, 14, 20, 44, 56, 1, 31, 49, 19, 55, 48)
names_items_hex_recode <- paste0("hex", items_hex_recode) # pasting "hex" and number gives the column names

names_items_hex_recode_R <- paste0(names_items_hex_recode, "_R") # adding _R for names of items, that are recoded
pc_df[,names_items_hex_recode_R] <- 6 - pc_df[,names_items_hex_recode] # recode items that need recoding


# identifying items for specific scales

# Honesty-Humility
items_hex_HH <- c(6, 30, 54, 12, 36, 60, 18, 42, 24, 48) # items in Honesty-Humility subscale 
names_items_hex_HH <- ifelse(items_hex_HH %in% items_hex_recode, paste0("hex", items_hex_HH, "_R"), paste0("hex",items_hex_HH)) # did item need recoding?
pc_hex_items_HH <- which(names(pc_df) %in% names_items_hex_HH) # select all items from honesty-humility subscale, correctly coded

# Emotionality
items_hex_EM <- c(5, 29, 53, 11, 35, 17, 41, 23, 47, 59) # items in Emotionality subscale
names_items_hex_EM <- ifelse(items_hex_EM %in% items_hex_recode, paste0("hex", items_hex_EM, "_R"), paste0("hex",items_hex_EM)) # did item need recoding?
pc_hex_items_EM <- which(names(pc_df) %in% names_items_hex_EM) # select all items from Emotionality subscale, correctly coded

#Extraversion
items_hex_EX <- c(4, 28, 52, 10, 34, 58, 16, 40, 22, 46) # items in Extraversion subscale
names_items_hex_EX <- ifelse(items_hex_EX %in% items_hex_recode, paste0("hex", items_hex_EX, "_R"), paste0("hex",items_hex_EX)) # did item need recoding?
pc_hex_items_EX <- which(names(pc_df) %in% names_items_hex_EX) # select all items from Extraversion subscale, correctly coded

#Agreeableness
items_hex_AG <- c(3, 27, 9, 33, 51, 15, 39, 57, 21, 45) # items in Agreeableness subscale
names_items_hex_AG <- ifelse(items_hex_AG %in% items_hex_recode, paste0("hex", items_hex_AG, "_R"), paste0("hex",items_hex_AG)) # did item need recoding?
pc_hex_items_AG <- which(names(pc_df) %in% names_items_hex_AG) # select all items from Agreeableness subscale, correctly coded

#Conscientiousness
items_hex_CO <- c(2, 26, 8, 32, 14, 38, 50, 20, 44, 56) # items in Conscientiousness subscale
names_items_hex_CO <- ifelse(items_hex_CO %in% items_hex_recode, paste0("hex", items_hex_CO, "_R"), paste0("hex",items_hex_CO)) # did item need recoding?
pc_hex_items_CO <- which(names(pc_df) %in% names_items_hex_CO) # select all items from Conscientiousness subscale, correctly coded

#Openness to Experience
items_hex_OX <- c(1, 25, 7, 31, 13, 37, 49, 19, 43, 55) # items in Openness to Experience subscale
names_items_hex_OX <- ifelse(items_hex_OX %in% items_hex_recode, paste0("hex", items_hex_OX, "_R"), paste0("hex",items_hex_OX)) # did item need recoding?
pc_hex_items_OX <- which(names(pc_df) %in% names_items_hex_OX) # select all items from Openness to Experience subscale, correctly coded





# takes a while: on my machine

SE_Alpha_varT_HH <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_HH]),
       statistic = bootstrap_SE_varT,
       stat = "ALPHA",
       R = 1000)
  
  sd(bvarTs$t)
})

SE_Alpha_varT_EM <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EM]),
                 statistic = bootstrap_SE_varT,
                 stat = "ALPHA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Alpha_varT_EX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EX]),
                 statistic = bootstrap_SE_varT,
                 stat = "ALPHA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Alpha_varT_AG <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_AG]),
                 statistic = bootstrap_SE_varT,
                 stat = "ALPHA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Alpha_varT_CO <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_CO]),
                 statistic = bootstrap_SE_varT,
                 stat = "ALPHA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Alpha_varT_OX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_OX]),
                 statistic = bootstrap_SE_varT,
                 stat = "ALPHA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Alpha_varT.df <- data.frame(HH = SE_Alpha_varT_HH,
                               EM = SE_Alpha_varT_EM,
                               EX = SE_Alpha_varT_EX,
                               AG = SE_Alpha_varT_AG,
                               CO = SE_Alpha_varT_CO,
                               OX = SE_Alpha_varT_OX)

write.csv(SE_Alpha_varT.df, here("Reliability Estimates/BootstrappedSE_Alpha.csv"))

rma.varT_Alpha_HH.fit <- rma(measure = "GEN", yi = (AlphaHex_HH$reliability * Reg_prep$var_hh), sei = SE_Alpha_varT_HH, method = "REML")
rma.varT_Alpha_EM.fit <- rma(measure = "GEN", yi = (AlphaHex_EM$reliability * Reg_prep$var_em), sei = SE_Alpha_varT_EM, method = "REML")
rma.varT_Alpha_EX.fit <- rma(measure = "GEN", yi = (AlphaHex_EX$reliability * Reg_prep$var_ex), sei = SE_Alpha_varT_EX, method = "REML")
rma.varT_Alpha_AG.fit <- rma(measure = "GEN", yi = (AlphaHex_AG$reliability * Reg_prep$var_ag), sei = SE_Alpha_varT_AG, method = "REML")
rma.varT_Alpha_CO.fit <- rma(measure = "GEN", yi = (AlphaHex_CO$reliability * Reg_prep$var_co), sei = SE_Alpha_varT_CO, method = "REML")
rma.varT_Alpha_OX.fit <- rma(measure = "GEN", yi = (AlphaHex_OX$reliability * Reg_prep$var_ox), sei = SE_Alpha_varT_OX, method = "REML")











SE_Alpha_varE_HH <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_HH]),
                 statistic = bootstrap_SE_varE,
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Alpha_varE_EM <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EM]),
                 statistic = bootstrap_SE_varE,
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Alpha_varE_EX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EX]),
                 statistic = bootstrap_SE_varE,
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Alpha_varE_AG <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_AG]),
                 statistic = bootstrap_SE_varE,
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Alpha_varE_CO <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_CO]),
                 statistic = bootstrap_SE_varE,
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Alpha_varE_OX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_OX]),
                 statistic = bootstrap_SE_varE,
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Alpha_varE.df <- data.frame(HH = SE_Alpha_varE_HH,
                               EM = SE_Alpha_varE_EM,
                               EX = SE_Alpha_varE_EX,
                               AG = SE_Alpha_varE_AG,
                               CO = SE_Alpha_varE_CO,
                               OX = SE_Alpha_varE_OX)

write.csv(SE_Alpha_varE.df, here("Reliability Estimates/BootstrappedSE_E_Alpha.csv"))

rma.varE_Alpha_HH.fit <- rma(measure = "GEN", yi = (Reg_prep$var_hh - AlphaHex_HH$reliability * Reg_prep$var_hh), sei = SE_Alpha_varE_HH, method = "REML")
rma.varE_Alpha_EM.fit <- rma(measure = "GEN", yi = (Reg_prep$var_em - AlphaHex_EM$reliability * Reg_prep$var_em), sei = SE_Alpha_varE_EM, method = "REML")
rma.varE_Alpha_EX.fit <- rma(measure = "GEN", yi = (Reg_prep$var_ex - AlphaHex_EX$reliability * Reg_prep$var_ex), sei = SE_Alpha_varE_EX, method = "REML")
rma.varE_Alpha_AG.fit <- rma(measure = "GEN", yi = (Reg_prep$var_ag - AlphaHex_AG$reliability * Reg_prep$var_ag), sei = SE_Alpha_varE_AG, method = "REML")
rma.varE_Alpha_CO.fit <- rma(measure = "GEN", yi = (Reg_prep$var_co - AlphaHex_CO$reliability * Reg_prep$var_co), sei = SE_Alpha_varE_CO, method = "REML")
rma.varE_Alpha_OX.fit <- rma(measure = "GEN", yi = (Reg_prep$var_ox - AlphaHex_OX$reliability * Reg_prep$var_ox), sei = SE_Alpha_varE_OX, method = "REML")


pdf(here("Graphics/RMA_varEvarT.pdf"))

pT_Alpha_HH <- my_forest_plot(rma.varT_Alpha_HH.fit, AlphaHex_HH, main.title = "Forest Plot - HEXACO HH", x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_Alpha_HH <- my_forest_plot(rma.varE_Alpha_HH.fit, AlphaHex_HH, main.title = "Forest Plot - HEXACO HH", x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_Alpha_HH, pE_Alpha_HH)

pT_Alpha_EM <- my_forest_plot(rma.varT_Alpha_EM.fit, AlphaHex_EM, main.title = "Forest Plot - HEXACO EM", x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_Alpha_EM <- my_forest_plot(rma.varE_Alpha_EM.fit, AlphaHex_EM, main.title = "Forest Plot - HEXACO EM", x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_Alpha_EM, pE_Alpha_EM)

pT_Alpha_EX <- my_forest_plot(rma.varT_Alpha_EX.fit, AlphaHex_EX, main.title = "Forest Plot - HEXACO EX", x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_Alpha_EX <- my_forest_plot(rma.varE_Alpha_EX.fit, AlphaHex_EX, main.title = "Forest Plot - HEXACO EX", x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_Alpha_EX, pE_Alpha_EX)

pT_Alpha_AG <- my_forest_plot(rma.varT_Alpha_AG.fit, AlphaHex_AG, main.title = "Forest Plot - HEXACO AG", x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_Alpha_AG <- my_forest_plot(rma.varE_Alpha_AG.fit, AlphaHex_AG, main.title = "Forest Plot - HEXACO AG", x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_Alpha_AG, pE_Alpha_AG)

pT_Alpha_CO <- my_forest_plot(rma.varT_Alpha_CO.fit, AlphaHex_CO, main.title = "Forest Plot - HEXACO CO", x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_Alpha_CO <- my_forest_plot(rma.varE_Alpha_CO.fit, AlphaHex_CO, main.title = "Forest Plot - HEXACO CO", x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_Alpha_CO, pE_Alpha_CO)

pT_Alpha_OX <- my_forest_plot(rma.varT_Alpha_OX.fit, AlphaHex_OX, main.title = "Forest Plot - HEXACO OX", x.lab = "Est. True Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
pE_Alpha_OX <- my_forest_plot(rma.varE_Alpha_OX.fit, AlphaHex_OX, main.title = "Forest Plot - HEXACO OX", x.lab = "Est. Error Variance (Alpha)", ci.lvl = .975, CI.display = TRUE)
gridExtra::grid.arrange(pT_Alpha_OX, pE_Alpha_OX)

dev.off()






# Using McDonald's Omega

# Issue: At times, bootstrapped sample leads to non-convergance in estimate of omega -> can't use this approach currently.


SE_Omega_varT_HH <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_HH]),
                 statistic = bootstrap_SE_varT,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Omega_varT_EM <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EM]),
                 statistic = bootstrap_SE_varT,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Omega_varT_EX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EX]),
                 statistic = bootstrap_SE_varT,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Omega_varT_AG <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_AG]),
                 statistic = bootstrap_SE_varT,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Omega_varT_CO <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_CO]),
                 statistic = bootstrap_SE_varT,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Omega_varT_OX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarTs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_OX]),
                 statistic = bootstrap_SE_varT,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarTs$t)
})

SE_Omega_varT.df <- data.frame(HH = SE_Omega_varT_HH,
                               EM = SE_Omega_varT_EM,
                               EX = SE_Omega_varT_EX,
                               AG = SE_Omega_varT_AG,
                               CO = SE_Omega_varT_CO,
                               OX = SE_Omega_varT_OX)

write.csv(SE_varT.df, here("Reliability Estimates/BootstrappedSE_Omega.csv"))

rma.varT_Omega_HH.fit <- rma(measure = "GEN", yi = (OmegaHex_HH$reliability * Reg_prep$var_hh), sei = SE_Omega_varT_HH, method = "REML")
rma.varT_Omega_EM.fit <- rma(measure = "GEN", yi = (OmegaHex_EM$reliability * Reg_prep$var_em), sei = SE_Omega_varT_EM, method = "REML")
rma.varT_Omega_EX.fit <- rma(measure = "GEN", yi = (OmegaHex_EX$reliability * Reg_prep$var_ex), sei = SE_Omega_varT_EX, method = "REML")
rma.varT_Omega_AG.fit <- rma(measure = "GEN", yi = (OmegaHex_AG$reliability * Reg_prep$var_ag), sei = SE_Omega_varT_AG, method = "REML")
rma.varT_Omega_CO.fit <- rma(measure = "GEN", yi = (OmegaHex_CO$reliability * Reg_prep$var_co), sei = SE_Omega_varT_CO, method = "REML")
rma.varT_Omega_OX.fit <- rma(measure = "GEN", yi = (OmegaHex_OX$reliability * Reg_prep$var_ox), sei = SE_Omega_varT_OX, method = "REML")











SE_Omega_varE_HH <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_HH]),
                 statistic = bootstrap_SE_varE,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Omega_varE_EM <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EM]),
                 statistic = bootstrap_SE_varE,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Omega_varE_EX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_EX]),
                 statistic = bootstrap_SE_varE,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Omega_varE_AG <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_AG]),
                 statistic = bootstrap_SE_varE,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Omega_varE_CO <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_CO]),
                 statistic = bootstrap_SE_varE,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Omega_varE_OX <- sapply(unique(pc_df$source), FUN = function(x){
  bvarEs <- boot(data = as.matrix(pc_df[pc_df$source == x, names_items_hex_OX]),
                 statistic = bootstrap_SE_varE,
                 stat = "OMEGA",
                 R = 1000)
  
  sd(bvarEs$t)
})

SE_Omega_varE.df <- data.frame(HH = SE_Omega_varE_HH,
                               EM = SE_Omega_varE_EM,
                               EX = SE_Omega_varE_EX,
                               AG = SE_Omega_varE_AG,
                               CO = SE_Omega_varE_CO,
                               OX = SE_Omega_varE_OX)

write.csv(SE_varE.df, here("Reliability Estimates/BootstrappedSE_E_Omega.csv"))

rma.varE_Omega_HH.fit <- rma(measure = "GEN", yi = (Reg_prep$var_hh - OmegaHex_HH$reliability * Reg_prep$var_hh), sei = SE_Omega_varE_HH, method = "REML")
rma.varE_Omega_EM.fit <- rma(measure = "GEN", yi = (Reg_prep$var_em - OmegaHex_EM$reliability * Reg_prep$var_em), sei = SE_Omega_varE_EM, method = "REML")
rma.varE_Omega_EX.fit <- rma(measure = "GEN", yi = (Reg_prep$var_ex - OmegaHex_EX$reliability * Reg_prep$var_ex), sei = SE_Omega_varE_EX, method = "REML")
rma.varE_Omega_AG.fit <- rma(measure = "GEN", yi = (Reg_prep$var_ag - OmegaHex_AG$reliability * Reg_prep$var_ag), sei = SE_Omega_varE_AG, method = "REML")
rma.varE_Omega_CO.fit <- rma(measure = "GEN", yi = (Reg_prep$var_co - OmegaHex_CO$reliability * Reg_prep$var_co), sei = SE_Omega_varE_CO, method = "REML")
rma.varE_Omega_OX.fit <- rma(measure = "GEN", yi = (Reg_prep$var_ox - OmegaHex_OX$reliability * Reg_prep$var_ox), sei = SE_Omega_varE_OX, method = "REML")

