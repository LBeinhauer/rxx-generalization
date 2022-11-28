# "basic" MLM reliability modelling

library(tidyverse)


sim_data <- readRDS(file = "MGCFA-MLSEM_test.RData")



testdata <- sim_data[[80]]$data


testdata$ID <- 1:nrow(testdata)


testdata_long <- data.table::melt(testdata, id.vars = c("group", "ID"))

testdata_long_aggV <- testdata %>%
  mutate(meanV = rowMeans(testdata[,1:10]))

testdata_long_dummy <- fastDummies::dummy_cols(testdata_long, 
                                               select_columns = c("variable", "group"))

mlmodel <- paste0("value ~ (1|", paste0("variable_V", 1:10, collapse = " + "), ") + (1|",
                  paste0("group_", 1:20, collapse = " + "), ")")



fit_simple <- lme4::lmer(meanV ~ 1|group, 
                         data = testdata_long_aggV)


summary(fit_simple)

