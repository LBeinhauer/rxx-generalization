### Reliability Generalization HEXACO | Big Five TIPI ###

## 08/03/2022

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
est_pc_hex_HH <- rel_extractor(pc_df, pc_hex_items_HH) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_HH <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_HH])) < length(pc_hex_items_HH) &
                                                 pc_df$source != unique(pc_df$source)[15]),], pc_hex_items_HH) # generate McDonald's omega reliability estimates
# with lab 15 (Sutan) does not converge

pc_hex_HH_transform_prepped <- Bonett_prep(est_pc_hex_HH, rma_prep_pc, length(pc_hex_items_HH)) # prepare alpha transformation
transformed_pc_hex_HH <- Bonett_transformation(pc_hex_HH_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Emotionality
est_pc_hex_EM <- rel_extractor(pc_df, pc_hex_items_EM) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EM <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_EM])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_EM)),], pc_hex_items_EM)

pc_hex_EM_transform_prepped <- Bonett_prep(est_pc_hex_EM, rma_prep_pc, length(pc_hex_items_EM)) # prepare alpha transformation
transformed_pc_hex_EM <- Bonett_transformation(pc_hex_EM_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Extraversion
est_pc_hex_EX <- rel_extractor(pc_df, pc_hex_items_EX) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EX <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_EX])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_EX)),], pc_hex_items_EX)

pc_hex_EX_transform_prepped <- Bonett_prep(est_pc_hex_EX, rma_prep_pc, length(pc_hex_items_EX)) # prepare alpha transformation
transformed_pc_hex_EX <- Bonett_transformation(pc_hex_EX_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Agreeableness
est_pc_hex_AG <- rel_extractor(pc_df, pc_hex_items_AG) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_AG <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_AG])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_AG)),], pc_hex_items_AG)

pc_hex_AG_transform_prepped <- Bonett_prep(est_pc_hex_AG, rma_prep_pc, length(pc_hex_items_AG)) # prepare alpha transformation
transformed_pc_hex_AG <- Bonett_transformation(pc_hex_AG_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Conscientiousness
est_pc_hex_CO <- rel_extractor(pc_df, pc_hex_items_CO) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_CO <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_CO])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_CO)),], pc_hex_items_CO)

pc_hex_CO_transform_prepped <- Bonett_prep(est_pc_hex_CO, rma_prep_pc, length(pc_hex_items_CO)) # prepare alpha transformation
transformed_pc_hex_CO <- Bonett_transformation(pc_hex_CO_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Openness to Experiences
est_pc_hex_OX <- rel_extractor(pc_df, pc_hex_items_OX) # generate Cronbach's alpha reliability estimates 
# lab 3 does not converge (Evans), cov-mat is singular, no inverse
omega_pc_hex_OX <- omega_extractor(pc_df[which(rowSums(is.na(pc_df[,pc_hex_items_OX])) # generate McDonald's omega reliability estimates
                                               < length(pc_hex_items_OX) &
                                                 !pc_df$source %in% unique(pc_df$source)[3]),], pc_hex_items_OX)

pc_hex_OX_transform_prepped <- Bonett_prep(est_pc_hex_OX, rma_prep_pc, length(pc_hex_items_OX)) # prepare alpha transformation
transformed_pc_hex_OX <- Bonett_transformation(pc_hex_OX_transform_prepped)  # Bonett-transformation of alpha-coefficient



# fitting random-effects meta-analysis to estimates of reliability using Cronbach's alpha & McDonald's omega
rel.rma.fit_pc_hex_HH <- rma(measure = "GEN", yi = est_pc_hex_HH$reliability, sei = est_pc_hex_HH$StandardError) # alpha HH
o.rel.rma.fit_pc_hex_HH <- rma(measure = "GEN", yi = omega_pc_hex_HH$reliability, sei = omega_pc_hex_HH$StandardError) # omega HH
t.rel.rma.fit_pc_hex_HH <- rma(measure = "GEN", yi = transformed_pc_hex_HH$reliability, sei = transformed_pc_hex_HH$StandardError) # transformed HH

rel.rma.fit_pc_hex_EM <- rma(measure = "GEN", yi = est_pc_hex_EM$reliability, sei = est_pc_hex_EM$StandardError) # alpha EM
o.rel.rma.fit_pc_hex_EM <- rma(measure = "GEN", yi = omega_pc_hex_EM$reliability, sei = omega_pc_hex_EM$StandardError) # omega EM
t.rel.rma.fit_pc_hex_EM <- rma(measure = "GEN", yi = transformed_pc_hex_EM$reliability, sei = transformed_pc_hex_EM$StandardError) # transformed HH

rel.rma.fit_pc_hex_EX <- rma(measure = "GEN", yi = est_pc_hex_EX$reliability, sei = est_pc_hex_EX$StandardError) # alpha EX
o.rel.rma.fit_pc_hex_EX <- rma(measure = "GEN", yi = omega_pc_hex_EX$reliability, sei = omega_pc_hex_EX$StandardError) # omega EX
t.rel.rma.fit_pc_hex_EX <- rma(measure = "GEN", yi = transformed_pc_hex_EX$reliability, sei = transformed_pc_hex_EX$StandardError) # transformed HH

rel.rma.fit_pc_hex_AG <- rma(measure = "GEN", yi = est_pc_hex_AG$reliability, sei = est_pc_hex_AG$StandardError) # alpha AG
o.rel.rma.fit_pc_hex_AG <- rma(measure = "GEN", yi = omega_pc_hex_AG$reliability, sei = omega_pc_hex_AG$StandardError) # omega AG
t.rel.rma.fit_pc_hex_AG <- rma(measure = "GEN", yi = transformed_pc_hex_AG$reliability, sei = transformed_pc_hex_AG$StandardError) # transformed HH

rel.rma.fit_pc_hex_CO <- rma(measure = "GEN", yi = est_pc_hex_CO$reliability, sei = est_pc_hex_CO$StandardError) # alpha CO
o.rel.rma.fit_pc_hex_CO <- rma(measure = "GEN", yi = omega_pc_hex_CO$reliability, sei = omega_pc_hex_CO$StandardError) # omega CO
t.rel.rma.fit_pc_hex_CO <- rma(measure = "GEN", yi = transformed_pc_hex_CO$reliability, sei = transformed_pc_hex_CO$StandardError) # transformed HH

rel.rma.fit_pc_hex_OX <- rma(measure = "GEN", yi = est_pc_hex_OX$reliability, sei = est_pc_hex_OX$StandardError) # alpha OX
o.rel.rma.fit_pc_hex_OX <- rma(measure = "GEN", yi = omega_pc_hex_OX$reliability, sei = omega_pc_hex_OX$StandardError) # omega OX
t.rel.rma.fit_pc_hex_OX <- rma(measure = "GEN", yi = transformed_pc_hex_OX$reliability, sei = transformed_pc_hex_OX$StandardError) # transformed HH









#### Regression


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



### actual regression


## Honesty Humility

Reg_est_pc_hex_HH <- data.frame(est_pc_hex_HH,
                                lang = lang_lab_r,
                                comp = comp_lab_r,
                                sex = sex,
                                mean_age = mean_age)

reg.rel.rma.fit_pc_hex_HH <- rma(yi = reliability, sei = StandardError,
                                 mods = ~ lang + comp + sex + mean_age,
                                 data = Reg_est_pc_hex_HH)


## Emotionality

Reg_est_pc_hex_EM <- data.frame(est_pc_hex_EM,
                                lang = lang_lab_r,
                                comp = comp_lab_r,
                                sex = sex,
                                mean_age = mean_age)

reg.rel.rma.fit_pc_hex_EM <- rma(yi = reliability, sei = StandardError,
                                 mods = ~ lang + comp + sex + mean_age ,
                                 data = Reg_est_pc_hex_EM)


## Extraversion

Reg_est_pc_hex_EX <- data.frame(est_pc_hex_EX,
                                lang = lang_lab_r,
                                comp = comp_lab_r,
                                sex = sex,
                                mean_age = mean_age)

reg.rel.rma.fit_pc_hex_EX <- rma(yi = reliability, sei = StandardError,
                                 mods = ~ lang + comp + sex + mean_age ,
                                 data = Reg_est_pc_hex_EX)



## Agreeableness

Reg_est_pc_hex_AG <- data.frame(est_pc_hex_AG,
                                lang = lang_lab_r,
                                comp = comp_lab_r,
                                sex = sex,
                                mean_age = mean_age)

reg.rel.rma.fit_pc_hex_AG <- rma(yi = reliability, sei = StandardError,
                                 mods = ~ lang + comp + sex + mean_age ,
                                 data = Reg_est_pc_hex_AG)



## Conscientiousness

Reg_est_pc_hex_CO <- data.frame(est_pc_hex_CO,
                                lang = lang_lab_r,
                                comp = comp_lab_r,
                                sex = sex,
                                mean_age = mean_age)

reg.rel.rma.fit_pc_hex_CO <- rma(yi = reliability, sei = StandardError,
                                 mods = ~ lang + comp + sex + mean_age ,
                                 data = Reg_est_pc_hex_CO)





## Openness to Experience

Reg_est_pc_hex_OX <- data.frame(est_pc_hex_OX,
                                lang = lang_lab_r,
                                comp = comp_lab_r,
                                sex = sex,
                                mean_age = mean_age)

reg.rel.rma.fit_pc_hex_OX <- rma(yi = reliability, sei = StandardError,
                                 mods = ~ lang + comp + sex + mean_age ,
                                 data = Reg_est_pc_hex_OX)


  
# cov_Aczel_hex_OX <- cov(pc_df[which(pc_df$lab.name == "Aczel"),names_items_hex_OX])
# psych::glb.algebraic(cov_Aczel_hex_OX)






### Analysing the facets ###

nameprep_hex <- function(items){
  ifelse(items %in% items_hex_recode, paste0("hex", items, "_R"), paste0("hex", items))
}

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




all_facet_means3 <- all_facet_means

names(all_facet_means3) <- c(paste0("HH", 1:4), paste0("EM", 1:4), paste0("EX", 1:4),
                             paste0("AG", 1:4), paste0("CO", 1:4), paste0("OX", 1:4))


all_facet_means3 <- all_facet_means3 %>%
  mutate(source = pc_df$source)




est_pc_hex_HH.f <- rel_extractor(all_facet_means3, paste0("HH", 1:4)) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_HH.f <- omega_extractor(all_facet_means3[which(rowSums(is.na(all_facet_means3[,paste0("HH", 1:4)])) # generate McDonald's omega reliability estimates
                                               < length(paste0("HH", 1:4))),], paste0("HH", 1:4))

pc_hex_HH_transform_prepped.f <- Bonett_prep(est_pc_hex_HH.f, rma_prep_pc, length(paste0("HH", 1:4))) # prepare alpha transformation
transformed_pc_hex_HH.f <- Bonett_transformation(pc_hex_HH_transform_prepped.f)  # Bonett-transformation of alpha-coefficient



est_pc_hex_EM.f <- rel_extractor(all_facet_means3, paste0("EM", 1:4)) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EM.f <- omega_extractor(all_facet_means3[which(rowSums(is.na(all_facet_means3[,paste0("EM", 1:4)])) # generate McDonald's omega reliability estimates
                                                            < length(paste0("EM", 1:4))),], paste0("EM", 1:4))

pc_hex_EM_transform_prepped.f <- Bonett_prep(est_pc_hex_EM.f, rma_prep_pc, length(paste0("EM", 1:4))) # prepare alpha transformation
transformed_pc_hex_EM.f <- Bonett_transformation(pc_hex_EM_transform_prepped.f)  # Bonett-transformation of alpha-coefficient




est_pc_hex_EX.f <- rel_extractor(all_facet_means3, paste0("EX", 1:4)) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_EX.f <- omega_extractor(all_facet_means3[which(rowSums(is.na(all_facet_means3[,paste0("EX", 1:4)])) # generate McDonald's omega reliability estimates
                                                            < length(paste0("EX", 1:4))),], paste0("EX", 1:4))

pc_hex_EX_transform_prepped.f <- Bonett_prep(est_pc_hex_EX.f, rma_prep_pc, length(paste0("EX", 1:4))) # prepare alpha transformation
transformed_pc_hex_EX.f <- Bonett_transformation(pc_hex_EX_transform_prepped.f)  # Bonett-transformation of alpha-coefficient



est_pc_hex_AG.f <- rel_extractor(all_facet_means3, paste0("AG", 1:4)) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_AG.f <- omega_extractor(all_facet_means3[which(rowSums(is.na(all_facet_means3[,paste0("AG", 1:4)])) # generate McDonald's omega reliability estimates
                                                            < length(paste0("AG", 1:4))),], paste0("AG", 1:4))

pc_hex_AG_transform_prepped.f <- Bonett_prep(est_pc_hex_AG.f, rma_prep_pc, length(paste0("AG", 1:4))) # prepare alpha transformation
transformed_pc_hex_AG.f <- Bonett_transformation(pc_hex_AG_transform_prepped.f)  # Bonett-transformation of alpha-coefficient




est_pc_hex_CO.f <- rel_extractor(all_facet_means3, paste0("CO", 1:4)) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_CO.f <- omega_extractor(all_facet_means3[which(rowSums(is.na(all_facet_means3[,paste0("CO", 1:4)])) # generate McDonald's omega reliability estimates
                                                            < length(paste0("CO", 1:4))),], paste0("CO", 1:4))

pc_hex_CO_transform_prepped.f <- Bonett_prep(est_pc_hex_CO.f, rma_prep_pc, length(paste0("CO", 1:4))) # prepare alpha transformation
transformed_pc_hex_CO.f <- Bonett_transformation(pc_hex_CO_transform_prepped.f)  # Bonett-transformation of alpha-coefficient




est_pc_hex_OX.f <- rel_extractor(all_facet_means3, paste0("OX", 1:4)) # generate Cronbach's alpha reliability estimates 
omega_pc_hex_OX.f <- omega_extractor(all_facet_means3[which(rowSums(is.na(all_facet_means3[,paste0("OX", 1:4)])) # generate McDonald's omega reliability estimates
                                                            < length(paste0("OX", 1:4))),], paste0("OX", 1:4))

pc_hex_OX_transform_prepped.f <- Bonett_prep(est_pc_hex_OX.f, rma_prep_pc, length(paste0("OX", 1:4))) # prepare alpha transformation
transformed_pc_hex_OX.f <- Bonett_transformation(pc_hex_OX_transform_prepped.f)  # Bonett-transformation of alpha-coefficient








### Compare differences in reliability estimates between facet and item-level analysis of score reliability on personality dimensions ###

est_pc_hex_HH - est_pc_hex_HH.f
est_pc_hex_EM - est_pc_hex_EM.f
est_pc_hex_EX - est_pc_hex_EX.f
est_pc_hex_AG - est_pc_hex_AG.f
est_pc_hex_CO - est_pc_hex_CO.f
est_pc_hex_OX - est_pc_hex_OX.f

omega_pc_hex_HH - omega_pc_hex_HH.f
omega_pc_hex_EM - omega_pc_hex_EM.f
omega_pc_hex_EX - omega_pc_hex_EX.f
omega_pc_hex_AG - omega_pc_hex_AG.f
omega_pc_hex_CO - omega_pc_hex_CO.f
omega_pc_hex_OX - omega_pc_hex_OX.f

transformed_pc_hex_HH - transformed_pc_hex_HH.f
transformed_pc_hex_EM - transformed_pc_hex_EM.f
transformed_pc_hex_EX - transformed_pc_hex_EX.f
transformed_pc_hex_AG - transformed_pc_hex_AG.f
transformed_pc_hex_CO - transformed_pc_hex_CO.f
transformed_pc_hex_OX - transformed_pc_hex_OX.f







rel.rma.fit_pc_hex_HH.f <- rma(measure = "GEN", yi = est_pc_hex_HH.f$reliability, sei = est_pc_hex_HH.f$StandardError) # alpha HH
o.rel.rma.fit_pc_hex_HH.f <- rma(measure = "GEN", yi = omega_pc_hex_HH.f$reliability, sei = omega_pc_hex_HH.f$StandardError) # omega HH
t.rel.rma.fit_pc_hex_HH.f <- rma(measure = "GEN", yi = transformed_pc_hex_HH.f$reliability, sei = transformed_pc_hex_HH.f$StandardError) # transformed HH


rel.rma.fit_pc_hex_EM.f <- rma(measure = "GEN", yi = est_pc_hex_EM.f$reliability, sei = est_pc_hex_EM.f$StandardError) # alpha EM
o.rel.rma.fit_pc_hex_EM.f <- rma(measure = "GEN", yi = omega_pc_hex_EM.f$reliability, sei = omega_pc_hex_EM.f$StandardError) # omega EM
t.rel.rma.fit_pc_hex_EM.f <- rma(measure = "GEN", yi = transformed_pc_hex_EM.f$reliability, sei = transformed_pc_hex_EM.f$StandardError) # transformed EM


rel.rma.fit_pc_hex_EX.f <- rma(measure = "GEN", yi = est_pc_hex_EX.f$reliability, sei = est_pc_hex_EX.f$StandardError) # alpha EX
o.rel.rma.fit_pc_hex_EX.f <- rma(measure = "GEN", yi = omega_pc_hex_EX.f$reliability, sei = omega_pc_hex_EX.f$StandardError) # omega EX
t.rel.rma.fit_pc_hex_EX.f <- rma(measure = "GEN", yi = transformed_pc_hex_EX.f$reliability, sei = transformed_pc_hex_EX.f$StandardError) # transformed EX


rel.rma.fit_pc_hex_AG.f <- rma(measure = "GEN", yi = est_pc_hex_AG.f$reliability, sei = est_pc_hex_AG.f$StandardError) # alpha AG
o.rel.rma.fit_pc_hex_AG.f <- rma(measure = "GEN", yi = omega_pc_hex_AG.f$reliability, sei = omega_pc_hex_AG.f$StandardError) # omega AG
t.rel.rma.fit_pc_hex_AG.f <- rma(measure = "GEN", yi = transformed_pc_hex_AG.f$reliability, sei = transformed_pc_hex_AG.f$StandardError) # transformed AG


rel.rma.fit_pc_hex_CO.f <- rma(measure = "GEN", yi = est_pc_hex_CO.f$reliability, sei = est_pc_hex_CO.f$StandardError) # alpha CO
o.rel.rma.fit_pc_hex_CO.f <- rma(measure = "GEN", yi = omega_pc_hex_CO.f$reliability, sei = omega_pc_hex_CO.f$StandardError) # omega CO
t.rel.rma.fit_pc_hex_CO.f <- rma(measure = "GEN", yi = transformed_pc_hex_CO.f$reliability, sei = transformed_pc_hex_CO.f$StandardError) # transformed CO


rel.rma.fit_pc_hex_OX.f <- rma(measure = "GEN", yi = est_pc_hex_OX.f$reliability, sei = est_pc_hex_OX.f$StandardError) # alpha OX
o.rel.rma.fit_pc_hex_OX.f <- rma(measure = "GEN", yi = omega_pc_hex_OX.f$reliability, sei = omega_pc_hex_OX.f$StandardError) # omega OX
t.rel.rma.fit_pc_hex_OX.f <- rma(measure = "GEN", yi = transformed_pc_hex_OX.f$reliability, sei = transformed_pc_hex_OX.f$StandardError) # transformed OX


# Differences in heterogeneity estimates Cronbach's Alpha, comparing facet- and item-level analysis #

rel.rma.fit_pc_hex_HH$tau2 - rel.rma.fit_pc_hex_HH.f$tau2
rel.rma.fit_pc_hex_EM$tau2 - rel.rma.fit_pc_hex_EM.f$tau2
rel.rma.fit_pc_hex_EX$tau2 - rel.rma.fit_pc_hex_EX.f$tau2
rel.rma.fit_pc_hex_AG$tau2 - rel.rma.fit_pc_hex_AG.f$tau2
rel.rma.fit_pc_hex_CO$tau2 - rel.rma.fit_pc_hex_CO.f$tau2
rel.rma.fit_pc_hex_OX$tau2 - rel.rma.fit_pc_hex_OX.f$tau2

rel.rma.fit_pc_hex_HH$I2 - rel.rma.fit_pc_hex_HH.f$I2
rel.rma.fit_pc_hex_EM$I2 - rel.rma.fit_pc_hex_EM.f$I2
rel.rma.fit_pc_hex_EX$I2 - rel.rma.fit_pc_hex_EX.f$I2
rel.rma.fit_pc_hex_AG$I2 - rel.rma.fit_pc_hex_AG.f$I2
rel.rma.fit_pc_hex_CO$I2 - rel.rma.fit_pc_hex_CO.f$I2
rel.rma.fit_pc_hex_OX$I2 - rel.rma.fit_pc_hex_OX.f$I2


# Differences in heterogeneity estimates McDonald's Omega, comparing facet- and item-level analysis #

o.rel.rma.fit_pc_hex_HH$tau2 - o.rel.rma.fit_pc_hex_HH.f$tau2
o.rel.rma.fit_pc_hex_EM$tau2 - o.rel.rma.fit_pc_hex_EM.f$tau2
o.rel.rma.fit_pc_hex_EX$tau2 - o.rel.rma.fit_pc_hex_EX.f$tau2
o.rel.rma.fit_pc_hex_AG$tau2 - o.rel.rma.fit_pc_hex_AG.f$tau2
o.rel.rma.fit_pc_hex_CO$tau2 - o.rel.rma.fit_pc_hex_CO.f$tau2
o.rel.rma.fit_pc_hex_OX$tau2 - o.rel.rma.fit_pc_hex_OX.f$tau2

o.rel.rma.fit_pc_hex_HH$I2 - o.rel.rma.fit_pc_hex_HH.f$I2
o.rel.rma.fit_pc_hex_EM$I2 - o.rel.rma.fit_pc_hex_EM.f$I2
o.rel.rma.fit_pc_hex_EX$I2 - o.rel.rma.fit_pc_hex_EX.f$I2
o.rel.rma.fit_pc_hex_AG$I2 - o.rel.rma.fit_pc_hex_AG.f$I2
o.rel.rma.fit_pc_hex_CO$I2 - o.rel.rma.fit_pc_hex_CO.f$I2
o.rel.rma.fit_pc_hex_OX$I2 - o.rel.rma.fit_pc_hex_OX.f$I2


# Differences in heterogeneity estimates Bonnett-transformed Cronbach's Alpha, comparing facet- and item-level analysis #

t.rel.rma.fit_pc_hex_HH$tau2 - t.rel.rma.fit_pc_hex_HH.f$tau2
t.rel.rma.fit_pc_hex_EM$tau2 - t.rel.rma.fit_pc_hex_EM.f$tau2
t.rel.rma.fit_pc_hex_EX$tau2 - t.rel.rma.fit_pc_hex_EX.f$tau2
t.rel.rma.fit_pc_hex_AG$tau2 - t.rel.rma.fit_pc_hex_AG.f$tau2
t.rel.rma.fit_pc_hex_CO$tau2 - t.rel.rma.fit_pc_hex_CO.f$tau2
t.rel.rma.fit_pc_hex_OX$tau2 - t.rel.rma.fit_pc_hex_OX.f$tau2

t.rel.rma.fit_pc_hex_HH$I2 - t.rel.rma.fit_pc_hex_HH.f$I2
t.rel.rma.fit_pc_hex_EM$I2 - t.rel.rma.fit_pc_hex_EM.f$I2
t.rel.rma.fit_pc_hex_EX$I2 - t.rel.rma.fit_pc_hex_EX.f$I2
t.rel.rma.fit_pc_hex_AG$I2 - t.rel.rma.fit_pc_hex_AG.f$I2
t.rel.rma.fit_pc_hex_CO$I2 - t.rel.rma.fit_pc_hex_CO.f$I2
t.rel.rma.fit_pc_hex_OX$I2 - t.rel.rma.fit_pc_hex_OX.f$I2




# Differences in heterogeneity estimates Cronbach's Alpha and Bonett-transformed Cronbach's Alpha, at item-level analysis #

rel.rma.fit_pc_hex_HH$tau2 - t.rel.rma.fit_pc_hex_HH$tau2
rel.rma.fit_pc_hex_EM$tau2 - t.rel.rma.fit_pc_hex_EM$tau2
rel.rma.fit_pc_hex_EX$tau2 - t.rel.rma.fit_pc_hex_EX$tau2
rel.rma.fit_pc_hex_AG$tau2 - t.rel.rma.fit_pc_hex_AG$tau2
rel.rma.fit_pc_hex_CO$tau2 - t.rel.rma.fit_pc_hex_CO$tau2
rel.rma.fit_pc_hex_OX$tau2 - t.rel.rma.fit_pc_hex_OX$tau2

rel.rma.fit_pc_hex_HH$I2 - t.rel.rma.fit_pc_hex_HH$I2
rel.rma.fit_pc_hex_EM$I2 - t.rel.rma.fit_pc_hex_EM$I2
rel.rma.fit_pc_hex_EX$I2 - t.rel.rma.fit_pc_hex_EX$I2
rel.rma.fit_pc_hex_AG$I2 - t.rel.rma.fit_pc_hex_AG$I2
rel.rma.fit_pc_hex_CO$I2 - t.rel.rma.fit_pc_hex_CO$I2
rel.rma.fit_pc_hex_OX$I2 - t.rel.rma.fit_pc_hex_OX$I2




# Differences in heterogeneity estimates Cronbach's Alpha and Bonett-transformed Cronbach's Alpha, at facet-level analysis #

rel.rma.fit_pc_hex_HH.f$tau2 - t.rel.rma.fit_pc_hex_HH.f$tau2
rel.rma.fit_pc_hex_EM.f$tau2 - t.rel.rma.fit_pc_hex_EM.f$tau2
rel.rma.fit_pc_hex_EX.f$tau2 - t.rel.rma.fit_pc_hex_EX.f$tau2
rel.rma.fit_pc_hex_AG.f$tau2 - t.rel.rma.fit_pc_hex_AG.f$tau2
rel.rma.fit_pc_hex_CO.f$tau2 - t.rel.rma.fit_pc_hex_CO.f$tau2
rel.rma.fit_pc_hex_OX.f$tau2 - t.rel.rma.fit_pc_hex_OX.f$tau2

rel.rma.fit_pc_hex_HH.f$I2 - t.rel.rma.fit_pc_hex_HH.f$I2
rel.rma.fit_pc_hex_EM.f$I2 - t.rel.rma.fit_pc_hex_EM.f$I2
rel.rma.fit_pc_hex_EX.f$I2 - t.rel.rma.fit_pc_hex_EX.f$I2
rel.rma.fit_pc_hex_AG.f$I2 - t.rel.rma.fit_pc_hex_AG.f$I2
rel.rma.fit_pc_hex_CO.f$I2 - t.rel.rma.fit_pc_hex_CO.f$I2
rel.rma.fit_pc_hex_OX.f$I2 - t.rel.rma.fit_pc_hex_OX.f$I2






model_facets <- "Honesty Humility =~ Sincerity + Fairness + GreedAvoidance + Modesty 
Emotionality =~ Fearfulness + Anxiety + Dependence + Sentimentality 
Extraversion =~ SocialSelfEsteem + SocialBoldness + Sociability + Liveliness 
Agreeableness =~ Forgiveness + Gentleness + Flexibility + Patience 
Conscientiousness =~ Organization + Diligence + Perfectionism + Prudence 
Openness to Experience =~ AestheticAppreciation + Inquisitiveness + Creativity + Unconventionality"

cat(model_facets)

cfafit_facets1 <- cfa(model_facets, data = all_facet_means)

summary(cfafit_facets1, fit.measures = TRUE)




library(GPArotation)
library(GGally)


#esem_efa <- psych::fa(all_facet_means, nfactors = 6, rotate = "geominQ", fm = "ml", delta = .5)

Targ_key <- psych::make.keys(24,list(H=1:4,E=5:8, X=9:12, A=13:16, C=17:20, O=21:24))
Targ_key <- psych::scrub(Targ_key,isvalue=1)  #fix the 0s, allow the NAs to be estimated

esem_efa <- psych::fa(all_facet_means, nfactors = 6, rotate = "TargetT", fm = "ml", Target = Targ_key)


esem_efa$loadings



esem_loadings <- data.table(matrix(esem_efa$loadings, nrow = 24, 6))


names(esem_loadings) <- c("OpennessToExperience", "Emotionality", "HonestyHumility", "Conscientiousness", "Extraversion", "Agreeableness")

esem_loadings$item <- paste0("x", c(1:24))

esem_loadings <- melt(esem_loadings, "item", variable.name = "latent")

sapply(unique(esem_loadings$latent), FUN = function(x){
  loadings <- esem_loadings[which(esem_loadings$latent == x),]
  loadings$item[which(loadings$value == max(loadings$value))]
})

anchors <- c(HonestyHumility = "x1", Emotionality = "x8", Extraversion = "x12", Agreeableness = "X16", 
             Conscientiousness = "X19", OpennesstoExperience = "x21")


make_esem_model <- function(loadings_dt, anchor){
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors)) loadings_dt[latent != l & item == anchors[l], is_anchor := 1]
  
  # make syntax column per item; syntax is different depending on is_anchor
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]
  
  #Make syntax for each latent variable
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"),"\n")
  }
  
  # Put all syntaxes together
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

esem_model <- make_esem_model(esem_loadings, anchors)

writeLines(esem_model)


all_facet_means2 <- all_facet_means

names(all_facet_means2) <- paste0("x", c(1:24))

esem_fit <- cfa(esem_model, all_facet_means2, std.lv = TRUE)


summary(esem_fit, fit.measures = TRUE)










 # item-level analysis


hex_data_recoded_sorted <- cbind(pc_df[,names_items_hex_HH], pc_df[,names_items_hex_EM], pc_df[,names_items_hex_EX], 
                                 pc_df[,names_items_hex_AG], pc_df[,names_items_hex_CO], pc_df[,names_items_hex_OX], 
                                 pc_df[,"source"])

names(hex_data_recoded_sorted) <- c(names_items_hex_HH, names_items_hex_EM, 
                                    names_items_hex_EX, names_items_hex_AG,
                                    names_items_hex_CO, names_items_hex_OX,
                                    "source")



Targ_key <- psych::make.keys(60,list(H=1:10,E=11:20, X=21:30, A=31:40, C=41:50, O=51:60))
Targ_key <- psych::scrub(Targ_key,isvalue=1)  #fix the 0s, allow the NAs to be estimated

esem_efa <- psych::fa(hex_data_recoded_sorted[,-ncol(hex_data_recoded_sorted)], 
                      nfactors = 6, rotate = "TargetT", fm = "ml", Target = Targ_key)


esem_efa$loadings



esem_loadings <- data.table(matrix(esem_efa$loadings, nrow = 60, 6))


names(esem_loadings) <- c("OpennessToExperience", "Emotionality", "HonestyHumility", "Conscientiousness", "Extraversion", "Agreeableness")

esem_loadings$item <- names(hex_data_recoded_sorted[,-ncol(hex_data_recoded_sorted)])

esem_loadings <- melt(esem_loadings, "item", variable.name = "latent")

max_loadings.f <- sapply(unique(esem_loadings$latent), FUN = function(x){
  loadings <- esem_loadings[which(esem_loadings$latent == x),]
  loadings$item[which(loadings$value == max(loadings$value))]
})

anchors <- c(HonestyHumility = max_loadings.f[which(max_loadings.f %in% names_items_hex_HH)], 
             Emotionality = max_loadings.f[which(max_loadings.f %in% names_items_hex_EM)], 
             Extraversion = max_loadings.f[which(max_loadings.f %in% names_items_hex_EX)], 
             Agreeableness = max_loadings.f[which(max_loadings.f %in% names_items_hex_AG)], 
             Conscientiousness = max_loadings.f[which(max_loadings.f %in% names_items_hex_CO)], 
             OpennesstoExperience = max_loadings.f[which(max_loadings.f %in% names_items_hex_OX)])


make_esem_model <- function(loadings_dt, anchor){
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors)) loadings_dt[latent != l & item == anchors[l], is_anchor := 1]
  
  # make syntax column per item; syntax is different depending on is_anchor
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]
  
  #Make syntax for each latent variable
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"),"\n")
  }
  
  # Put all syntaxes together
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}

esem_model <- make_esem_model(esem_loadings, anchors)

writeLines(esem_model)


esem_fit <- cfa(esem_model, hex_data_recoded_sorted, std.lv = TRUE)

esem_fit_group <- cfa(esem_model, hex_data_recoded_sorted, std.lv = TRUE,
                      group = "source")


summary(esem_fit, fit.measures = TRUE)

summary(esem_fit_group, fit.measures = TRUE)
















psych::esem(all_facet_means, varsX = 1:12, nfX = 3, varsY = 13:24, nfY = 3)


psych::esem()






all_facet_means3 <- all_facet_means

names(all_facet_means3) <- c(paste0("HH", 1:4), paste0("EM", 1:4), paste0("EX", 1:4),
                             paste0("AG", 1:4), paste0("CO", 1:4), paste0("OX", 1:4))


model <- paste(paste0("H =~ ", paste(paste0("HH", 1:4), collapse = " + ")),
               paste0("E =~ ", paste(paste0("EM", 1:4), collapse = " + ")),
               paste0("X =~ ", paste(paste0("EX", 1:4), collapse = " + ")),
               paste0("A =~ ", paste(paste0("AG", 1:4), collapse = " + ")),
               paste0("C =~ ", paste(paste0("CO", 1:4), collapse = " + ")),
               paste0("O =~ ", paste(paste0("OX", 1:4), collapse = " + ")), sep = "\n")
cat(model)


fit <- lavaan::cfa(model = model, data = all_facet_means3)

library(readr)

#esem_model <- lavaan::mplus2lavaan.modelSyntax(read_file(here("ESEM_MPlus_Model1.txt")))
esem_model <- read_file(here("ESEM_lavaan_Model1.txt"))

cat(esem_model)

fit1 <- lavaan::cfa(model = esem_model, data = all_facet_means3)

summary(fit, fit.measures = TRUE)

######  Testing for Measurement Invariance  ######





ind_H <- paste0(names_items_hex_HH, collapse = " + ")
ind_E <- paste0(names_items_hex_EM, collapse = " + ")
ind_X <- paste0(names_items_hex_EX, collapse = " + ")
ind_A <- paste0(names_items_hex_AG, collapse = " + ")
ind_C <- paste0(names_items_hex_CO, collapse = " + ")
ind_O <- paste0(names_items_hex_OX, collapse = " + ")

model <- paste(paste0("H =~ ", ind_H),
               paste0("E =~ ", ind_E),
               paste0("X =~ ", ind_X),
               paste0("A =~ ", ind_A),
               paste0("C =~ ", ind_C),
               paste0("O =~ ", ind_O), sep = "\n")
cat(model)


cfafit1 <- cfa(model, data = pc_df)

summary(cfafit1, fit.measures = TRUE)


cfafit_H <- cfa(paste0("H =~ ", ind_H), data = pc_df)
summary(cfafit_H, fit.measures = TRUE)

cfafit_E <- cfa(paste0("E =~ ", ind_E), data = pc_df)
summary(cfafit_E, fit.measures = TRUE)

cfafit_X <- cfa(paste0("X =~ ", ind_X), data = pc_df)
summary(cfafit_X, fit.measures = TRUE)

cfafit_A <- cfa(paste0("A =~ ", ind_A), data = pc_df)
summary(cfafit_A, fit.measures = TRUE)

cfafit_C <- cfa(paste0("C =~ ", ind_C), data = pc_df)
summary(cfafit_C, fit.measures = TRUE)

cfafit_O <- cfa(paste0("O =~ ", ind_O), data = pc_df)
summary(cfafit_O, fit.measures = TRUE)



























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
S1_df[,rev_items] <- 8 - S1_df[,rev_items]
S1_df$source <- S1_df$Source.Primary # add proper "source" column

# Extraversion
est_BF.EX <- rel_extractor(S1_df, BF.EX_items) # generate Cronbach's alpha reliability estimates 
omega_BF.EX <- omega_extractor(S1_df[which(rowSums(is.na(S1_df[,BF.EX_items])) == 0),], BF.EX_items) # generate McDonald's omega reliability estimates
nrow(S1_df[which(rowSums(is.na(S1_df[,BF.EX_items])) == 0),])

est_BF.EX_transform_prepped <- Bonett_prep(est_BF.EX, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.EX <- Bonett_transformation(est_BF.EX_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Agreeableness
est_BF.AG <- rel_extractor(S1_df, BF.AG_items) # generate Cronbach's alpha reliability estimates 
omega_BF.AG <- omega_extractor(S1_df[which(rowSums(is.na(S1_df[,BF.AG_items])) == 0),], BF.AG_items) # generate McDonald's omega reliability estimates
nrow(S1_df[which(rowSums(is.na(S1_df[,BF.AG_items])) == 0),])

est_BF.AG_transform_prepped <- Bonett_prep(est_BF.AG, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.AG <- Bonett_transformation(est_BF.AG_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Conscientiousness
est_BF.CO <- rel_extractor(S1_df, BF.CO_items) # generate Cronbach's alpha reliability estimates 
omega_BF.CO <- omega_extractor(S1_df[which(rowSums(is.na(S1_df[,BF.CO_items])) == 0),], BF.CO_items) # generate McDonald's omega reliability estimates
nrow(S1_df[which(rowSums(is.na(S1_df[,BF.CO_items])) == 0),])

est_BF.CO_transform_prepped <- Bonett_prep(est_BF.CO, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.CO <- Bonett_transformation(est_BF.CO_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Emotional Stability
est_BF.ES <- rel_extractor(S1_df, BF.ES_items) # generate Cronbach's alpha reliability estimates 
omega_BF.ES <- omega_extractor(S1_df[which(rowSums(is.na(S1_df[,BF.ES_items])) == 0),], BF.ES_items) # generate McDonald's omega reliability estimates
nrow(S1_df[which(rowSums(is.na(S1_df[,BF.ES_items])) == 0),])

est_BF.ES_transform_prepped <- Bonett_prep(est_BF.ES, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.ES <- Bonett_transformation(est_BF.ES_transform_prepped)  # Bonett-transformation of alpha-coefficient


# Openness to Experiences
est_BF.OX <- rel_extractor(S1_df, BF.OX_items) # generate Cronbach's alpha reliability estimates 
omega_BF.OX <- omega_extractor(S1_df[which(rowSums(is.na(S1_df[,BF.OX_items])) == 0),], BF.OX_items) # generate McDonald's omega reliability estimates
nrow(S1_df[which(rowSums(is.na(S1_df[,BF.OX_items])) == 0),])

est_BF.OX_transform_prepped <- Bonett_prep(est_BF.OX, rma_prep_BF, 2) # prepare alpha transformation
transformed_BF.OX <- Bonett_transformation(est_BF.OX_transform_prepped)  # Bonett-transformation of alpha-coefficient




# fitting random-effects meta-analysis to estimates of reliability using Cronbach's alpha & McDonald's omega
rel.rma.fit_BF.EX <- rma(measure = "GEN", yi = est_BF.EX$reliability, sei = est_BF.EX$StandardError) # alpha HH
o.rel.rma.fit_BF.EX <- rma(measure = "GEN", yi = omega_BF.EX$reliability, sei = omega_BF.EX$StandardError) # omega HH
t.rel.rma.fit_BF.EX <- rma(measure = "GEN", yi = transformed_BF.EX$reliability, sei = transformed_BF.EX$StandardError) # transformed HH

rel.rma.fit_BF.AG <- rma(measure = "GEN", yi = est_BF.AG$reliability, sei = est_BF.AG$StandardError) # alpha HH
o.rel.rma.fit_BF.AG <- rma(measure = "GEN", yi = omega_BF.AG$reliability, sei = omega_BF.AG$StandardError) # omega HH
t.rel.rma.fit_BF.AG <- rma(measure = "GEN", yi = transformed_BF.AG$reliability, sei = transformed_BF.AG$StandardError) # transformed HH

rel.rma.fit_BF.CO <- rma(measure = "GEN", yi = est_BF.CO$reliability, sei = est_BF.CO$StandardError) # alpha HH
o.rel.rma.fit_BF.CO <- rma(measure = "GEN", yi = omega_BF.CO$reliability, sei = omega_BF.CO$StandardError) # omega HH
t.rel.rma.fit_BF.CO <- rma(measure = "GEN", yi = transformed_BF.CO$reliability, sei = transformed_BF.CO$StandardError) # transformed HH

rel.rma.fit_BF.ES <- rma(measure = "GEN", yi = est_BF.ES$reliability, sei = est_BF.ES$StandardError) # alpha HH
o.rel.rma.fit_BF.ES <- rma(measure = "GEN", yi = omega_BF.ES$reliability, sei = omega_BF.ES$StandardError) # omega HH
t.rel.rma.fit_BF.ES <- rma(measure = "GEN", yi = transformed_BF.ES$reliability, sei = transformed_BF.ES$StandardError) # transformed HH

rel.rma.fit_BF.OX <- rma(measure = "GEN", yi = est_BF.OX$reliability, sei = est_BF.OX$StandardError) # alpha HH
o.rel.rma.fit_BF.OX <- rma(measure = "GEN", yi = omega_BF.OX$reliability, sei = omega_BF.OX$StandardError) # omega HH
t.rel.rma.fit_BF.OX <- rma(measure = "GEN", yi = transformed_BF.OX$reliability, sei = transformed_BF.OX$StandardError) # transformed HH


moderation_vars <- c("Language", "Weird", "Country", "Location", "Tablet", "Pencil", "StudyOrder", "IDiffOrder")
