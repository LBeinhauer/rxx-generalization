### Reliability Generalization HEXACO ###

## 08/03/2022




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



rma_prep_pc <- rma_prep_function(pc_df %>%                            # generate mean aggregates at lab-level
                                   mutate(DV = hex1,                  # no DV - selection is arbitrary
                                          factor = maz.prime.cond),
                                 factor.vec = c("commandments", "books"))



# Honesty Humility
est_pc_hex_HH <- rel_extractor(pc_df, pc_hex_items_HH, "Hex_HH") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_HH <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_HH])) < length(pc_hex_items_HH) &
                                                 pc_df$source != unique(pc_df$source)[15]),], pc_hex_items_HH, "Hex_HH") # generate McDonald's omega reliability estimates
# with lab 15 (Sutan) does not converge

pc_hex_HH_alpha_transform_prepped <- Bonett_prep(est_pc_hex_HH, rma_prep_pc, length(pc_hex_items_HH)) # prepare alpha transformation
transformed_pc_alpha_hex_HH <- Bonett_transformation(pc_hex_HH_alpha_transform_prepped, "AlphaHex_HH")  # Bonett-transformation of alpha-coefficient

pc_hex_HH_omega_transform_prepped <- Bonett_prep(omega_pc_hex_HH, rma_prep_pc, length(pc_hex_items_HH)) # prepare alpha transformation
transformed_pc_omega_hex_HH <- Bonett_transformation(pc_hex_HH_omega_transform_prepped, "OmegaHex_HH")  # Bonett-transformation of alpha-coefficient


# Emotionality
est_pc_hex_EM <- rel_extractor(pc_df, pc_hex_items_EM, "Hex_EM") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EM <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_EM])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_EM)),], pc_hex_items_EM, "Hex_EM")

pc_hex_EM_alpha_transform_prepped <- Bonett_prep(est_pc_hex_EM, rma_prep_pc, length(pc_hex_items_EM)) # prepare alpha transformation
transformed_pc_alpha_hex_EM <- Bonett_transformation(pc_hex_EM_alpha_transform_prepped, "AlphaHex_EM")  # Bonett-transformation of alpha-coefficient

pc_hex_EM_omega_transform_prepped <- Bonett_prep(omega_pc_hex_EM, rma_prep_pc, length(pc_hex_items_EM)) # prepare alpha transformation
transformed_pc_omega_hex_EM <- Bonett_transformation(pc_hex_EM_omega_transform_prepped, "OmegaHex_EM")  # Bonett-transformation of alpha-coefficient


# Extraversion
est_pc_hex_EX <- rel_extractor(pc_df, pc_hex_items_EX, "Hex_EX") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EX <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_EX])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_EX)),], pc_hex_items_EX, "Hex_EX")

pc_hex_EX_alpha_transform_prepped <- Bonett_prep(est_pc_hex_EX, rma_prep_pc, length(pc_hex_items_EX)) # prepare alpha transformation
transformed_pc_alpha_hex_EX <- Bonett_transformation(pc_hex_EX_alpha_transform_prepped, "AlphaHex_EX")  # Bonett-transformation of alpha-coefficient

pc_hex_EX_omega_transform_prepped <- Bonett_prep(omega_pc_hex_EX, rma_prep_pc, length(pc_hex_items_EX)) # prepare alpha transformation
transformed_pc_omega_hex_EX <- Bonett_transformation(pc_hex_EX_omega_transform_prepped, "OmegaHex_EX")  # Bonett-transformation of alpha-coefficient


# Agreeableness
est_pc_hex_AG <- rel_extractor(pc_df, pc_hex_items_AG, "Hex_AG") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_AG <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_AG])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_AG)),], pc_hex_items_AG, "Hex_AG")

pc_hex_AG_alpha_transform_prepped <- Bonett_prep(est_pc_hex_AG, rma_prep_pc, length(pc_hex_items_AG)) # prepare alpha transformation
transformed_pc_alpha_hex_AG <- Bonett_transformation(pc_hex_AG_alpha_transform_prepped, "AlphaHex_AG")  # Bonett-transformation of alpha-coefficient

pc_hex_AG_omega_transform_prepped <- Bonett_prep(omega_pc_hex_AG, rma_prep_pc, length(pc_hex_items_AG)) # prepare alpha transformation
transformed_pc_omega_hex_AG <- Bonett_transformation(pc_hex_AG_omega_transform_prepped, "OmegaHex_AG")  # Bonett-transformation of alpha-coefficient


# Conscientiousness
est_pc_hex_CO <- rel_extractor(pc_df, pc_hex_items_CO, "Hex_CO") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_CO <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_CO])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_CO)),], pc_hex_items_CO, "Hex_CO")

pc_hex_CO_alpha_transform_prepped <- Bonett_prep(est_pc_hex_CO, rma_prep_pc, length(pc_hex_items_CO)) # prepare alpha transformation
transformed_pc_alpha_hex_CO <- Bonett_transformation(pc_hex_CO_alpha_transform_prepped, "AlphaHex_CO")  # Bonett-transformation of alpha-coefficient

pc_hex_CO_omega_transform_prepped <- Bonett_prep(omega_pc_hex_CO, rma_prep_pc, length(pc_hex_items_CO)) # prepare alpha transformation
transformed_pc_omega_hex_CO <- Bonett_transformation(pc_hex_CO_omega_transform_prepped, "OmegaHex_CO")  # Bonett-transformation of alpha-coefficient


# Openness to Experiences
est_pc_hex_OX <- rel_extractor(pc_df, pc_hex_items_OX, "Hex_OX") # generate Cronbach's alpha reliability estimates 
# lab 3 does not converge (Evans), cov-mat is singular, no inverse
omega_pc_hex_OX <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_OX])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_OX) &
                                                 !pc_df$source %in% unique(pc_df$source)[3]),], pc_hex_items_OX, "Hex_OX")

pc_hex_OX_alpha_transform_prepped <- Bonett_prep(est_pc_hex_OX, rma_prep_pc, length(pc_hex_items_OX)) # prepare alpha transformation
transformed_pc_alpha_hex_OX <- Bonett_transformation(pc_hex_OX_alpha_transform_prepped, "AlphaHex_OX")  # Bonett-transformation of alpha-coefficient

pc_hex_OX_omega_transform_prepped <- Bonett_prep(omega_pc_hex_OX, rma_prep_pc, length(pc_hex_items_OX)) # prepare alpha transformation
transformed_pc_omega_hex_OX <- Bonett_transformation(pc_hex_OX_omega_transform_prepped, "OmegaHex_OX")  # Bonett-transformation of alpha-coefficient














### Analysing the facets ###


## HH

hh_facet_items <- list(
  items_hex_HH_Sincerity <- c(6, 30, 54),
  items_hex_HH_Fairness <- c(12, 36, 60),
  items_hex_HH_GreedAvoidance <- c(18, 42),
  items_hex_HH_Modesty <- c(24, 48)
)

hh_facet_items_names <- lapply(hh_facet_items, nameprep_hex)

hh_facet_means <- sapply(hh_facet_items_names, FUN = function(x){
  rowMeans(pc_df[,x])
})%>%
  as.data.frame() %>%
  rename("Sincerity" = "V1", "Fairness" = "V2", "GreedAvoidance" = "V3", "Modesty" = "V4")


## EM

em_facet_items <- list(
  items_hex_EM_Fearfulness <- c(5, 29, 53),
  items_hex_EM_Anxiety <- c(11, 35),
  items_hex_EM_Dependence <- c(17, 41),
  items_hex_EM_Sentimentality <- c(23, 47, 59)
)

em_facet_items_names <- lapply(em_facet_items, nameprep_hex)

em_facet_means <- sapply(em_facet_items_names, FUN = function(x){
  rowMeans(pc_df[,x])
})%>%
  as.data.frame() %>%
  rename("Fearfulness" = "V1", "Anxiety" = "V2", "Dependence" = "V3", "Sentimentality" = "V4")


## EX

ex_facet_items <- list(
  items_hex_EX_SocialSelfEsteem <- c(4, 28, 52),
  items_hex_EX_SocialBoldness <- c(10, 34, 58),
  items_hex_EX_Sociability <- c(16, 40),
  items_hex_EX_Liveliness <- c(22, 46)
)

ex_facet_items_names <- lapply(ex_facet_items, nameprep_hex)

ex_facet_means <- sapply(ex_facet_items_names, FUN = function(x){
  rowMeans(pc_df[,x])
})%>%
  as.data.frame() %>%
  rename("SocialSelfEsteem" = "V1", "SocialBoldness" = "V2", "Sociability" = "V3", "Liveliness" = "V4")


## Agreeableness

ag_facet_items <- list(
  items_hex_AG_Forgiveness <- c(3, 27),
  items_hex_AG_Gentleness <- c(9, 33, 51),
  items_hex_AG_Flexibility <- c(15, 39, 57),
  items_hex_AG_Patience <- c(21, 45)
  
)

ag_facet_items_names <- lapply(ag_facet_items, nameprep_hex)

ag_facet_means <- sapply(ag_facet_items_names, FUN = function(x){
  rowMeans(pc_df[,x])
})%>%
  as.data.frame() %>%
  rename("Forgiveness" = "V1", "Gentleness" = "V2", "Flexibility" = "V3", "Patience" = "V4")


## Conscientiousness

co_facet_items <- list(
  items_hex_CO_Organization <- c(2, 26),
  items_hex_CO_Diligence <- c(8, 32),
  items_hex_CO_Perfectionism <- c(14, 38, 50),
  items_hex_CO_Prudence <- c(20, 44, 46)
)

co_facet_items_names <- lapply(co_facet_items, nameprep_hex)

co_facet_means <- sapply(co_facet_items_names, FUN = function(x){
  rowMeans(pc_df[,x])
})%>%
  as.data.frame() %>%
  rename("Organization" = "V1", "Diligence" = "V2", "Perfectionism" = "V3", "Prudence" = "V4")


## Openness to Experience

ox_facet_items <- list(
  items_hex_OX_AestheticApprecation <- c(1, 25),
  items_hex_OX_Inquisitveness <- c(7, 31),
  items_hex_OX_Creativity <- c(13, 37, 49),
  items_hex_OX_Unconventionality <- c(19, 43, 55)
)

ox_facet_items_names <- lapply(ox_facet_items, nameprep_hex)

ox_facet_means <- sapply(ox_facet_items_names, FUN = function(x){
  rowMeans(pc_df[,x])
})%>%
  as.data.frame() %>%
  rename("AestheticAppreciation" = "V1", "Inquisitiveness" = "V2", "Creativity" = "V3", "Unconventionality" = "V4")



all_facet_means <- cbind(hh_facet_means, em_facet_means, ex_facet_means, ag_facet_means, co_facet_means, ox_facet_means)

facet_df <- data.frame(all_facet_means, 
                       source = pc_df$source)














## Moderator preparation

pc_df <- pc_df %>% mutate(
  language = as.factor(language),
  compensation = as.factor(compensation),
  major = as.factor(major)
)

# language

lang <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  table(pc_df$language[which(pc_df$source == x)])
})

lang_t <- t(lang) # Özdogru 2 languages (8 English, 357 Turkish)

lang_lab <- apply(lang_t, MARGIN = 1, FUN = function(x){
  names(x)[which(x == max(x))]
})

table(lang_lab)

lang_lab_r <- ifelse(
  lang_lab == "Dutch", "Dutch", ifelse(
    lang_lab == "English", "English", ifelse(
      lang_lab == "German", "German", "Other"
    )
  ))

table(lang_lab_r)

# compensation

comp <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  table(pc_df$compensation[which(pc_df$source == x)])
})

comp_t <- t(comp) # only McCarthy 2 (1 Course Credit, 317 Entry into drawing), 
# Laine & Ferreira-Santos report no compensation (not none!)

comp_lab <- apply(comp_t, MARGIN = 1, FUN = function(x){
  ifelse(length(names(x)[which(x == max(x))]) == 1 , names(x)[which(x == max(x))], "Other")
})

table(comp_lab)

comp_lab_r <- ifelse(comp_lab == "course credit", "Course Credit", "Other")

table(comp_lab_r)

# gender

sex <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  gen <- pc_df$gender[which(pc_df$source == x)]
  gen_01 <- ifelse(gen == "female", 1, 0)
  mean(gen_01, na.rm = TRUE)
})

# age

mean_age <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  age <- pc_df$age[which(pc_df$source == x)]
  mean(age, na.rm = TRUE)
})  

sd_age <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  age <- pc_df$age[which(pc_df$source == x)]
  sd(age, na.rm = TRUE)
})  

# major

# maj <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
#   table(pc_df$major[which(pc_df$source == x)])
# })



## Score Variance

var_hh <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  scores <- pc_df[which(pc_df$source == x),names_items_hex_HH]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_em <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  scores <- pc_df[which(pc_df$source == x),names_items_hex_EM]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_ex <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  scores <- pc_df[which(pc_df$source == x),names_items_hex_EX]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_ag <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  scores <- pc_df[which(pc_df$source == x),names_items_hex_AG]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_co <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  scores <- pc_df[which(pc_df$source == x),names_items_hex_CO]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_ox <- sapply(as.matrix(unique(pc_df$source)), FUN = function(x){
  scores <- pc_df[which(pc_df$source == x),names_items_hex_OX]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})



var_facets_hh <- sapply(as.matrix(unique(facet_df$source)), FUN = function(x){
  scores <- facet_df[which(facet_df$source == x), names(hh_facet_means)]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_facets_em <- sapply(as.matrix(unique(facet_df$source)), FUN = function(x){
  scores <- facet_df[which(facet_df$source == x), names(em_facet_means)]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_facets_ex <- sapply(as.matrix(unique(facet_df$source)), FUN = function(x){
  scores <- facet_df[which(facet_df$source == x), names(ex_facet_means)]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_facets_ag <- sapply(as.matrix(unique(facet_df$source)), FUN = function(x){
  scores <- facet_df[which(facet_df$source == x), names(ag_facet_means)]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_facets_co <- sapply(as.matrix(unique(facet_df$source)), FUN = function(x){
  scores <- facet_df[which(facet_df$source == x), names(co_facet_means)]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})

var_facets_ox <- sapply(as.matrix(unique(facet_df$source)), FUN = function(x){
  scores <- facet_df[which(facet_df$source == x), names(ox_facet_means)]
  mean_scores <- rowMeans(scores)
  var <- var(mean_scores, na.rm = T)
})



Reg_prep <- data.frame(lang = lang_lab_r,
                       comp = comp_lab_r,
                       sex = sex,
                       #                       major = maj,
                       mean_age = mean_age,
                       source = labs_in_data[labs_in_data %in% labs_in_paper],
                       var_hh = var_hh,
                       var_em = var_em,
                       var_ex = var_ex,
                       var_ag = var_ag,
                       var_co = var_co,
                       var_ox = var_ox,
                       var_facets_hh = var_facets_hh,
                       var_facets_em = var_facets_em,
                       var_facets_ex = var_facets_ex,
                       var_facets_ag = var_facets_ag,
                       var_facets_co = var_facets_co,
                       var_facets_ox = var_facets_ox)

write.csv(Reg_prep, here("Meta-Regression/meta_regression_dat_HEXACO.csv"), row.names = FALSE)














# Honesty Humility
est_pc_hex_facets_HH <- rel_extractor(facet_df, names(hh_facet_means), "Hex_facets_HH") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_facets_HH <- omega_extractor(facet_df[which(rowSums(is.na(facet_df[,names(hh_facet_means)])) 
                                                         < length(names(hh_facet_means))),], names(hh_facet_means), "Hex_facets_HH") # generate McDonald's omega reliability estimates

pc_hex_facets_HH_alpha_transform_prepped <- Bonett_prep(est_pc_hex_facets_HH, rma_prep_pc, length(names(hh_facet_means))) # prepare alpha transformation
transformed_pc_alpha_hex_HH <- Bonett_transformation(pc_hex_facets_HH_alpha_transform_prepped, "AlphaHex_facets_HH")  # Bonett-transformation of alpha-coefficient

pc_hex_facets_HH_omega_transform_prepped <- Bonett_prep(omega_pc_hex_facets_HH, rma_prep_pc, length(names(hh_facet_means))) # prepare alpha transformation
transformed_pc_omega_hex_facets_HH <- Bonett_transformation(pc_hex_facets_HH_omega_transform_prepped, "OmegaHex_facets_HH")  # Bonett-transformation of alpha-coefficient


# Emotionality
est_pc_hex_facets_EM <- rel_extractor(facet_df, names(em_facet_means), "Hex_facets_EM") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_facets_EM <- omega_extractor(facet_df[which(rowSums(is.na(facet_df[,names(em_facet_means)])) # generate McDonald's omega reliability estimates
                                               < length(names(em_facet_means))),], names(em_facet_means), "Hex_facets_EM")

pc_hex_facets_EM_alpha_transform_prepped <- Bonett_prep(est_pc_hex_facets_EM, rma_prep_pc, length(names(em_facet_means))) # prepare alpha transformation
transformed_pc_alpha_hex_facets_EM <- Bonett_transformation(pc_hex_facets_EM_alpha_transform_prepped, "AlphaHex_facets_EM")  # Bonett-transformation of alpha-coefficient

pc_hex_facets_EM_omega_transform_prepped <- Bonett_prep(omega_pc_hex_facets_EM, rma_prep_pc, length(names(em_facet_means))) # prepare alpha transformation
transformed_pc_omega_hex_facets_EM <- Bonett_transformation(pc_hex_facets_EM_omega_transform_prepped, "OmegaHex_facets_EM")  # Bonett-transformation of alpha-coefficient


# Extraversion
est_pc_hex_facets_EX <- rel_extractor(facet_df, names(ex_facet_means), "Hex_facets_EX") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_facets_EX <- omega_extractor(facet_df[which(rowSums(is.na(facet_df[,names(ex_facet_means)])) # generate McDonald's omega reliability estimates
                                               < length(names(ex_facet_means))),], names(ex_facet_means), "Hex_facets_EX")

pc_hex_facets_EX_alpha_transform_prepped <- Bonett_prep(est_pc_hex_facets_EX, rma_prep_pc, length(names(ex_facet_means))) # prepare alpha transformation
transformed_pc_alpha_hex_facets_EX <- Bonett_transformation(pc_hex_facets_EX_alpha_transform_prepped, "AlphaHex_facets_EX")  # Bonett-transformation of alpha-coefficient

pc_hex_facets_EX_omega_transform_prepped <- Bonett_prep(omega_pc_hex_facets_EX, rma_prep_pc, length(names(ex_facet_means))) # prepare alpha transformation
transformed_pc_omega_hex_facets_EX <- Bonett_transformation(pc_hex_facets_EX_omega_transform_prepped, "OmegaHex_facets_EX")  # Bonett-transformation of alpha-coefficient


# Agreeableness
est_pc_hex_facets_AG <- rel_extractor(facet_df, names(ag_facet_means), "Hex_facets_AG") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_facets_AG <- omega_extractor(facet_df[which(rowSums(is.na(facet_df[,names(ag_facet_means)])) # generate McDonald's omega reliability estimates
                                               < length(names(ag_facet_means))),], names(ag_facet_means), "Hex_facets_AG")

pc_hex_facets_AG_alpha_transform_prepped <- Bonett_prep(est_pc_hex_facets_AG, rma_prep_pc, length(names(ag_facet_means))) # prepare alpha transformation
transformed_pc_alpha_facets_hex_AG <- Bonett_transformation(pc_hex_facets_AG_alpha_transform_prepped, "AlphaHex_facets_AG")  # Bonett-transformation of alpha-coefficient

pc_hex_facets_AG_omega_transform_prepped <- Bonett_prep(omega_pc_hex_facets_AG, rma_prep_pc, length(names(ag_facet_means))) # prepare alpha transformation
transformed_pc_omega_facets_hex_AG <- Bonett_transformation(pc_hex_facets_AG_omega_transform_prepped, "OmegaHex_facets_AG")  # Bonett-transformation of alpha-coefficient


# Conscientiousness
est_pc_hex_facets_CO <- rel_extractor(facet_df, names(co_facet_means), "Hex_facets_CO") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_facets_CO <- omega_extractor(facet_df[which(rowSums(is.na(facet_df[,names(co_facet_means)])) # generate McDonald's omega reliability estimates
                                               < length(names(co_facet_means))),], names(co_facet_means), "Hex_facets_CO")

pc_hex_facets_CO_alpha_transform_prepped <- Bonett_prep(est_pc_hex_facets_CO, rma_prep_pc, length(names(co_facet_means))) # prepare alpha transformation
transformed_pc_alpha_hex_facets_CO <- Bonett_transformation(pc_hex_facets_CO_alpha_transform_prepped, "AlphaHex_facets_CO")  # Bonett-transformation of alpha-coefficient

pc_hex_facets_CO_omega_transform_prepped <- Bonett_prep(omega_pc_hex_facets_CO, rma_prep_pc, length(names(co_facet_means))) # prepare alpha transformation
transformed_pc_omega_hex_facets_CO <- Bonett_transformation(pc_hex_facets_CO_omega_transform_prepped, "OmegaHex_facets_CO")  # Bonett-transformation of alpha-coefficient


# Openness to Experiences
est_pc_hex_facets_OX <- rel_extractor(facet_df, names(ox_facet_means), "Hex_facets_OX") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_facets_OX <- omega_extractor(facet_df[which(rowSums(is.na(facet_df[,names(ox_facet_means)])) # generate McDonald's omega reliability estimates
                                               < length(names(ox_facet_means)) ),], names(ox_facet_means), "Hex_facets_OX")

pc_hex_facets_OX_alpha_transform_prepped <- Bonett_prep(est_pc_hex_facets_OX, rma_prep_pc, length(names(ox_facet_means))) # prepare alpha transformation
transformed_pc_alpha_hex_facets_OX <- Bonett_transformation(pc_hex_facets_OX_alpha_transform_prepped, "AlphaHex_facets_OX")  # Bonett-transformation of alpha-coefficient

pc_hex_facets_OX_omega_transform_prepped <- Bonett_prep(omega_pc_hex_facets_OX, rma_prep_pc, length(names(ox_facet_means))) # prepare alpha transformation
transformed_pc_omega_hex_facets_OX <- Bonett_transformation(pc_hex_facets_OX_omega_transform_prepped, "OmegaHex_facets_OX")  # Bonett-transformation of alpha-coefficient




##### All Samples Combined Analysis #####


est_pc_hex_items_full_HH <- psych::alpha(pc_df[,names_items_hex_HH])
est_pc_hex_items_full_EM <- psych::alpha(pc_df[,names_items_hex_EM])
est_pc_hex_items_full_EX <- psych::alpha(pc_df[,names_items_hex_EX])
est_pc_hex_items_full_AG <- psych::alpha(pc_df[,names_items_hex_AG])
est_pc_hex_items_full_CO <- psych::alpha(pc_df[,names_items_hex_CO])
est_pc_hex_items_full_OX <- psych::alpha(pc_df[,names_items_hex_OX])



omega_pc_hex_items_full_HH <- coefficientalpha::omega(pc_df[which(rowSums(is.na(pc_df[,names_items_hex_HH])) < length(names_items_hex_HH)),
                                                            names_items_hex_HH], se = T, varphi = 0, test = F)
omega_pc_hex_items_full_EM <- coefficientalpha::omega(pc_df[which(rowSums(is.na(pc_df[,names_items_hex_EM])) < length(names_items_hex_EM)),
                                                            names_items_hex_EM], se = T, varphi = 0, test = F)
omega_pc_hex_items_full_EX <- coefficientalpha::omega(pc_df[which(rowSums(is.na(pc_df[,names_items_hex_EX])) < length(names_items_hex_EX)),
                                                            names_items_hex_EX], se = T, varphi = 0, test = F)
omega_pc_hex_items_full_AG <- coefficientalpha::omega(pc_df[which(rowSums(is.na(pc_df[,names_items_hex_AG])) < length(names_items_hex_AG)),
                                                            names_items_hex_AG], se = T, varphi = 0, test = F)
omega_pc_hex_items_full_CO <- coefficientalpha::omega(pc_df[which(rowSums(is.na(pc_df[,names_items_hex_CO])) < length(names_items_hex_CO)),
                                                            names_items_hex_CO], se = T, varphi = 0, test = F)
omega_pc_hex_items_full_OX <- coefficientalpha::omega(pc_df[which(rowSums(is.na(pc_df[,names_items_hex_OX])) < length(names_items_hex_OX)),
                                                            names_items_hex_OX], se = T, varphi = 0, test = F)



est_pc_hex_facets_full_HH <- psych::alpha(facet_df[,names(hh_facet_means)])
est_pc_hex_facets_full_EM <- psych::alpha(facet_df[,names(em_facet_means)])
est_pc_hex_facets_full_EX <- psych::alpha(facet_df[,names(ex_facet_means)])
est_pc_hex_facets_full_AG <- psych::alpha(facet_df[,names(ag_facet_means)])
est_pc_hex_facets_full_CO <- psych::alpha(facet_df[,names(co_facet_means)])
est_pc_hex_facets_full_OX <- psych::alpha(facet_df[,names(ox_facet_means)])



omega_pc_hex_facets_full_HH <- coefficientalpha::omega(facet_df[which(rowSums(is.na(facet_df[,names(hh_facet_means)])) < length(names(hh_facet_means))),
                                                                names(hh_facet_means)], se = T, varphi = 0, test = F)
omega_pc_hex_facets_full_EM <- coefficientalpha::omega(facet_df[which(rowSums(is.na(facet_df[,names(em_facet_means)])) < length(names(em_facet_means))),
                                                                names(em_facet_means)], se = T, varphi = 0, test = F)
omega_pc_hex_facets_full_EX <- coefficientalpha::omega(facet_df[which(rowSums(is.na(facet_df[,names(ex_facet_means)])) < length(names(ex_facet_means))),
                                                                names(ex_facet_means)], se = T, varphi = 0, test = F)
omega_pc_hex_facets_full_AG <- coefficientalpha::omega(facet_df[which(rowSums(is.na(facet_df[,names(ag_facet_means)])) < length(names(ag_facet_means))),
                                                                names(ag_facet_means)], se = T, varphi = 0, test = F)
omega_pc_hex_facets_full_CO <- coefficientalpha::omega(facet_df[which(rowSums(is.na(facet_df[,names(co_facet_means)])) < length(names(co_facet_means))),
                                                                names(co_facet_means)], se = T, varphi = 0, test = F)
omega_pc_hex_facets_full_OX <- coefficientalpha::omega(facet_df[which(rowSums(is.na(facet_df[,names(ox_facet_means)])) < length(names(ox_facet_means))),
                                                                names(ox_facet_means)], se = T, varphi = 0, test = F)


