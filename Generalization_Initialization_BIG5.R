### Reliability Generalization Big Five TIPI ###

## 10/03/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and generation of aggregates #
# and reliability coefficients                                                                    #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary


# relevant R packages
packages <- c("metafor", "tidyverse", "here", "data.table", "lavaan")

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






### Five Factor Model ###


## Ten Item Personality Inventory (TIPI) ##

ML2_S1 <- fread(here("Data/ManyLabs2/ML2_S1.csv"))

S1_df <- as.data.frame(ML2_S1)

BF_S1 <- S1_df[,names(S1_df)[c(grep("^tipi", names(S1_df)), grep("Source.Primary", names(S1_df)))]]
BF_S1 <- BF_S1[-grep("^tipi.t", names(BF_S1))]

items <- paste0("tipi_", c(1:10))

BF.EX_items <- items[c(grep("1$", items), grep("6$", items))]
BF.AG_items <- items[c(grep("2$", items), grep("7$", items))]
BF.CO_items <- items[c(grep("3$", items), grep("8$", items))]
BF.ES_items <- items[c(grep("4$", items), grep("9$", items))]
BF.OX_items <- items[c(grep("5$", items), grep("10$", items))]

rev_items <- items[grep("2$|4$|6$|8$|10$", items)]

rma_prep_BF <- rma_prep_function(S1_df %>%                            # generate mean aggregates at lab-level
                                   mutate(DV = tipi_1,                  # no DV - selection is arbitrary
                                          factor = ifelse(politics < 3, 1, 0),
                                          source = S1_df$Source.Primary), # group selection irrelevant as well
                                 factor.vec = c(1, 0))


# reverse coding
BF_S1[,rev_items] <- 8 - BF_S1[,rev_items]
BF_S1$source <- BF_S1$Source.Primary # add proper "source" column

# Extraversion
est_BF.EX <- rel_extractor(BF_S1, BF.EX_items, "BF-EX") # generate Cronbach's alpha reliability estimates 
omega_BF.EX <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.EX_items])) == 0),], BF.EX_items, "BF-EX") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.EX_items])) == 0),])

est_BF.EX_transform_prepped <- Bonett_prep(est_BF.EX, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.EX <- Bonett_transformation(est_BF.EX_transform_prepped, "BF-EX")  # Bonett-transformation of alpha-coefficient


# Agreeableness
est_BF.AG <- rel_extractor(BF_S1, BF.AG_items, "BF-AG") # generate Cronbach's alpha reliability estimates 
omega_BF.AG <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.AG_items])) == 0),], BF.AG_items, "BF-AG") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.AG_items])) == 0),])

est_BF.AG_transform_prepped <- Bonett_prep(est_BF.AG, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.AG <- Bonett_transformation(est_BF.AG_transform_prepped, "BF-AG")  # Bonett-transformation of alpha-coefficient


# Conscientiousness
est_BF.CO <- rel_extractor(BF_S1, BF.CO_items, "BF-CO") # generate Cronbach's alpha reliability estimates 
omega_BF.CO <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.CO_items])) == 0),], BF.CO_items, "BF-CO") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.CO_items])) == 0),])

est_BF.CO_transform_prepped <- Bonett_prep(est_BF.CO, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.CO <- Bonett_transformation(est_BF.CO_transform_prepped, "BF-CO")  # Bonett-transformation of alpha-coefficient


# Emotional Stability
est_BF.ES <- rel_extractor(BF_S1, BF.ES_items, "BF-ES") # generate Cronbach's alpha reliability estimates 
omega_BF.ES <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.ES_items])) == 0),], BF.ES_items, "BF-ES") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.ES_items])) == 0),])

est_BF.ES_transform_prepped <- Bonett_prep(est_BF.ES, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.ES <- Bonett_transformation(est_BF.ES_transform_prepped, "BF-ES")  # Bonett-transformation of alpha-coefficient


# Openness to Experiences
est_BF.OX <- rel_extractor(BF_S1, BF.OX_items, "BF-OX") # generate Cronbach's alpha reliability estimates 
omega_BF.OX <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.OX_items])) == 0),], BF.OX_items, "BF-OX") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.OX_items])) == 0),])

est_BF.OX_transform_prepped <- Bonett_prep(est_BF.OX, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.OX <- Bonett_transformation(est_BF.OX_transform_prepped, "BF-OX")  # Bonett-transformation of alpha-coefficient





moderation_vars <- c("Language", "Weird", "Country", "Location", "Tablet", "Pencil", "StudyOrder", "IDiffOrder")
