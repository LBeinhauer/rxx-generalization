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
est_BF.EX <- rel_extractor(BF_S1, BF.EX_items, "BF_EX") # generate Cronbach's alpha reliability estimates 
# omega_BF.EX <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.EX_items])) == 0),], BF.EX_items, "BF_EX") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.EX_items])) == 0),])

est_BF.EX_alpha_transform_prepped <- Bonett_prep(est_BF.EX, rma_prep_BF, 2) # prepare alpha transformation
transformed_alpha_BF.EX <- Bonett_transformation(est_BF.EX_alpha_transform_prepped, "AlphaBF_EX")  # Bonett-transformation of alpha-coefficient

# est_BF.EX_omega_transform_prepped <- Bonett_prep(omgea_BF.EX, rma_prep_BF, 2) # prepare alpha transformation
# transformed_omega_BF.EX <- Bonett_transformation(est_BF.EX_omega_transform_prepped, "OmegaBF_EX")  # Bonett-transformation of omega-coefficient


# Agreeableness
est_BF.AG <- rel_extractor(BF_S1, BF.AG_items, "BF_AG") # generate Cronbach's alpha reliability estimates 
# omega_BF.AG <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.AG_items])) == 0),], BF.AG_items, "BF_AG") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.AG_items])) == 0),])

est_BF.AG_alpha_transform_prepped <- Bonett_prep(est_BF.AG, rma_prep_BF, 2) # prepare alpha transformation
transformed_alpha_BF.AG <- Bonett_transformation(est_BF.AG_alpha_transform_prepped, "AlphaBF_AG")  # Bonett-transformation of alpha-coefficient

# est_BF.AG_omega_transform_prepped <- Bonett_prep(omgea_BF.AG, rma_prep_BF, 2) # prepare alpha transformation
# transformed_omega_BF.AG <- Bonett_transformation(est_BF.AG_omega_transform_prepped, "OmegaBF_AG")  # Bonett-transformation of omega-coefficient


# Conscientiousness
est_BF.CO <- rel_extractor(BF_S1, BF.CO_items, "BF_CO") # generate Cronbach's alpha reliability estimates 
# omega_BF.CO <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.CO_items])) == 0),], BF.CO_items, "BF_CO") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.CO_items])) == 0),])

est_BF.CO_alpha_transform_prepped <- Bonett_prep(est_BF.CO, rma_prep_BF, 2) # prepare alpha transformation
transformed_alpha_BF.CO <- Bonett_transformation(est_BF.CO_alpha_transform_prepped, "AlphaBF_CO")  # Bonett-transformation of alpha-coefficient

# est_BF.CO_omega_transform_prepped <- Bonett_prep(omgea_BF.CO, rma_prep_BF, 2) # prepare alpha transformation
# transformed_omega_BF.CO <- Bonett_transformation(est_BF.CO_omega_transform_prepped, "OmegaBF_CO")  # Bonett-transformation of omega-coefficient


# Emotional Stability
est_BF.ES <- rel_extractor(BF_S1, BF.ES_items, "BF_ES") # generate Cronbach's alpha reliability estimates 
# omega_BF.ES <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.ES_items])) == 0),], BF.ES_items, "BF_ES") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.ES_items])) == 0),])

est_BF.ES_alpha_transform_prepped <- Bonett_prep(est_BF.ES, rma_prep_BF, 2) # prepare alpha transformation
transformed_alpha_BF.ES <- Bonett_transformation(est_BF.ES_alpha_transform_prepped, "AlphaBF_ES")  # Bonett-transformation of alpha-coefficient

# est_BF.ES_omega_transform_prepped <- Bonett_prep(omgea_BF.ES, rma_prep_BF, 2) # prepare alpha transformation
# transformed_omega_BF.ES <- Bonett_transformation(est_BF.ES_omega_transform_prepped, "OmegaBF_ES")  # Bonett-transformation of omega-coefficient


# Openness to Experiences
est_BF.OX <- rel_extractor(BF_S1, BF.OX_items, "BF_OX") # generate Cronbach's alpha reliability estimates 
# omega_BF.OX <- omega_extractor(BF_S1[which(rowSums(is.na(BF_S1[,BF.OX_items])) == 0),], BF.OX_items, "BF_OX") # generate McDonald's omega reliability estimates
nrow(BF_S1[which(rowSums(is.na(BF_S1[,BF.OX_items])) == 0),])

est_BF.OX_alpha_transform_prepped <- Bonett_prep(est_BF.OX, rma_prep_BF, 2) # prepare alpha transformation
transformed_alpha_BF.OX <- Bonett_transformation(est_BF.OX_alpha_transform_prepped, "AlphaBF_OX")  # Bonett-transformation of alpha-coefficient

# est_BF.OX_omega_transform_prepped <- Bonett_prep(omgea_BF.OX, rma_prep_BF, 2) # prepare alpha transformation
# transformed_omega_BF.OX <- Bonett_transformation(est_BF.OX_omega_transform_prepped, "OmegaBF_OX")  # Bonett-transformation of omega-coefficient





moderation_vars <- c("Language", "Weird", "Country", "Location", "Tablet", "Pencil", "StudyOrder", "IDiffOrder", "Source.Primary")


BF_S1_mod <- ML2_S1[,..moderation_vars] %>%
  mutate(source = Source.Primary)


## Moderator preparation

BF_S1_mod <- BF_S1_mod %>% mutate(
  Language = as.factor(Language),
  Tablet = as.factor(Tablet)
)

# language

lang <- sapply(as.matrix(unique(BF_S1_mod$source)), FUN = function(x){
  table(BF_S1_mod$Language[which(BF_S1_mod$source == x)])
})

lang_t <- t(lang) # Özdogru 2 languages (8 English, 357 Turkish)

lang_lab <- apply(lang_t, MARGIN = 1, FUN = function(x){
  names(x)[which(x == max(x))]
})

table(lang_lab)

lang_lab_r <- ifelse(
  lang_lab == "Chinese (simplified)", "Chinese", ifelse(
    lang_lab == "Chinese (traditional)", "Chinese", ifelse(
      lang_lab == "Dutch", "Dutch", ifelse(
        lang_lab == "English", "English", "Other"
    )
  )
  )
)

table(lang_lab_r)

# gender

# sex <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
#   gen <- pc_df$gender[which(pc_df$source == x)]
#   gen_01 <- ifelse(gen == "female", 1, 0)
#   mean(gen_01, na.rm = TRUE)
# })

# age

# mean_age <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
#   age <- pc_df$age[which(pc_df$source == x)]
#   mean(age, na.rm = TRUE)
# })  
# 
# sd_age <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
#   age <- pc_df$age[which(pc_df$source == x)]
#   sd(age, na.rm = TRUE)
# })  

# major

# maj <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
#   table(pc_df$major[which(pc_df$source == x)])
# })



Reg_prep <- data.frame(lang = lang_lab_r,
                       source = unique(BF_S1_mod$source))

write.csv(Reg_prep, here("Meta-Regression/meta_regression_dat_BIG5.csv"), row.names = FALSE)



