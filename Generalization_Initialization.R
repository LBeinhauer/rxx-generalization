### Reliability Generalization HEXACO | Big Five TIPI ###

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

# identify relevant labs for analsysis
labs_in_paper <- c("Laine", "klein Selle & Rozmann", "Aczel", "Ferreira-Santos", "Meijer", "Loschelder", "Wick", "Suchotzki", 
                   "Sutan", "Vanpaemel", "Verschuere", "Wiggins", "Gonzalez-Iraizoz", "Koppel", "Birt", "McCarthy", "Evans", 
                   "Holzmeister", "Ozdogru")
labs_in_data <- unique(pc_df$source)
labs_in_data[8] <- "Gonzalez-Iraizoz"
labs_in_data[16] <- "Ozdogru"

# remove labs from data, which we do not need for analysis
labs_excl <- labs_in_data[!labs_in_data %in% labs_in_paper]
pc_df <- pc_df[which(!pc_df$source %in% labs_excl),]

pc_df$source[which(pc_df$source == "GonzÃ¡lez-Iraizoz")] <- "Gonzalez-Iraizoz"
pc_df$source[which(pc_df$source == "Ã-zdoÄYru")] <- "Ozdogru"


# include only participants in cheat condition (design was 2x2, cheat - no cheat x commandment - books)
pc_df <- pc_df[which(pc_df$maz.cheat.cond == "cheat"),]

names(pc_df)

# retain only those columns, which are needed for subsequent analysis.
pc_df <- pc_df[,c(which(names(pc_df) %in% c("lab.name", "inclusion", "maz.prime.cond", "maz.cheat.cond", "source",
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
est_pc_hex_HH <- rel_extractor(pc_df, pc_hex_items_HH, "HH") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_HH <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_HH])) < length(pc_hex_items_HH) &
                                                 pc_df$source != unique(pc_df$source)[15]),], pc_hex_items_HH, "HH") # generate McDonald's omega reliability estimates
# with lab 15 (Sutan) does not converge

pc_hex_HH_transform_prepped <- Bonett_prep(est_pc_hex_HH, rma_prep_pc, length(pc_hex_items_HH)) # prepare alpha transformation
transformed_pc_hex_HH <- Bonett_transformation(pc_hex_HH_transform_prepped, "HH")  # Bonett-transformation of alpha-coefficient


# Emotionality
est_pc_hex_EM <- rel_extractor(pc_df, pc_hex_items_EM, "EM") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EM <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_EM])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_EM)),], pc_hex_items_EM, "EM")

pc_hex_EM_transform_prepped <- Bonett_prep(est_pc_hex_EM, rma_prep_pc, length(pc_hex_items_EM)) # prepare alpha transformation
transformed_pc_hex_EM <- Bonett_transformation(pc_hex_EM_transform_prepped, "EM")  # Bonett-transformation of alpha-coefficient


# Extraversion
est_pc_hex_EX <- rel_extractor(pc_df, pc_hex_items_EX, "EX") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EX <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_EX])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_EX)),], pc_hex_items_EX, "EX")

pc_hex_EX_transform_prepped <- Bonett_prep(est_pc_hex_EX, rma_prep_pc, length(pc_hex_items_EX)) # prepare alpha transformation
transformed_pc_hex_EX <- Bonett_transformation(pc_hex_EX_transform_prepped, "EX")  # Bonett-transformation of alpha-coefficient


# Agreeableness
est_pc_hex_AG <- rel_extractor(pc_df, pc_hex_items_AG, "AG") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_AG <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_AG])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_AG)),], pc_hex_items_AG, "AG")

pc_hex_AG_transform_prepped <- Bonett_prep(est_pc_hex_AG, rma_prep_pc, length(pc_hex_items_AG)) # prepare alpha transformation
transformed_pc_hex_AG <- Bonett_transformation(pc_hex_AG_transform_prepped, "AG")  # Bonett-transformation of alpha-coefficient


# Conscientiousness
est_pc_hex_CO <- rel_extractor(pc_df, pc_hex_items_CO, "CO") # generate Cronbach's alpha reliability estimates 
omega_pc_hex_CO <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_CO])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_CO)),], pc_hex_items_CO, "CO")

pc_hex_CO_transform_prepped <- Bonett_prep(est_pc_hex_CO, rma_prep_pc, length(pc_hex_items_CO)) # prepare alpha transformation
transformed_pc_hex_CO <- Bonett_transformation(pc_hex_CO_transform_prepped, "CO")  # Bonett-transformation of alpha-coefficient


# Openness to Experiences
est_pc_hex_OX <- rel_extractor(pc_df, pc_hex_items_OX, "OX") # generate Cronbach's alpha reliability estimates 
# lab 3 does not converge (Evans), cov-mat is singular, no inverse
omega_pc_hex_OX <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_OX])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_OX) &
                                                 !pc_df$source %in% unique(pc_df$source)[3]),], pc_hex_items_OX, "OX")

pc_hex_OX_transform_prepped <- Bonett_prep(est_pc_hex_OX, rma_prep_pc, length(pc_hex_items_OX)) # prepare alpha transformation
transformed_pc_hex_OX <- Bonett_transformation(pc_hex_OX_transform_prepped, "OX")  # Bonett-transformation of alpha-coefficient








## Moderator preparation

pc_df <- pc_df %>% mutate(
  language = as.factor(language),
  compensation = as.factor(compensation),
  major = as.factor(major)
)

# language

lang <- sapply(as.matrix(unique(pc_df$lab.name)), FUN = function(x){
  table(pc_df$language[which(pc_df$lab.name == x)])
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

comp <- sapply(as.matrix(unique(pc_df$lab.name)), FUN = function(x){
  table(pc_df$compensation[which(pc_df$lab.name == x)])
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

sex <- sapply(as.matrix(unique(pc_df$lab.name)), FUN = function(x){
  gen <- pc_df$gender[which(pc_df$lab.name == x)]
  gen_01 <- ifelse(gen == "female", 1, 0)
  mean(gen_01, na.rm = TRUE)
})

# age

mean_age <- sapply(as.matrix(unique(pc_df$lab.name)), FUN = function(x){
  age <- pc_df$age[which(pc_df$lab.name == x)]
  mean(age, na.rm = TRUE)
})  

sd_age <- sapply(as.matrix(unique(pc_df$lab.name)), FUN = function(x){
  age <- pc_df$age[which(pc_df$lab.name == x)]
  sd(age, na.rm = TRUE)
})  

# major

# maj <- sapply(as.matrix(unique(pc_df$lab.name)), FUN = function(x){
#   table(pc_df$major[which(pc_df$lab.name == x)])
# })



Reg_prep <- data.frame(lang = lang_lab_r,
                       comp = comp_lab_r,
                       sex = sex,
#                      major = maj,
                       mean_age = mean_age,
                       source = labs_in_data[labs_in_data %in% labs_in_paper])

write.csv(Reg_prep, here("Meta-Regression/meta_regression_dat.csv"), row.names = FALSE)
