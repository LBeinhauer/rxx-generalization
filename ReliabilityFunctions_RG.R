##### Function Definition Data Handling Reliability Analysis #####


## Lukas Beinhauer 24/01/21 ##


################################################################################################################################
# The following code defines a number of functions, which will be used during the data analysis process.                       #
# Specifically, functions are defined to: automatically generate Cronbach's Alpha estimates and standard errors, McDonald's    #
#   Omega estimates and standard errors, automatically aggregate data at a lab-level or generate specific random-effects       #
#   meta-analysis estimates.                                                                                                   #
################################################################################################################################



# The function over the lines 18:38 is used to generate estimates of Cronbach's Alpha and Standard Errors.

rel_extractor <- function(data, item.vec, scale_info){
  
  # firstly, apply the alpha() function from the <psych> library - this applies the alpha function to the data from each lab separately
  alpha.fit <- apply(matrix(unique(data$source), ncol=1), MARGIN = 1, FUN = function(x) {
    psych::alpha(subset(data[,item.vec], subset = c(data$source == x)))
  })
  
  # secondly, the results from the alpha function are used - we can easily extract the Cronbach's alpha estimat from the object.
  rel.est <- apply(matrix(c(1:length(alpha.fit)), ncol=1), MARGIN = 1, FUN = function(x) {
    alpha.fit[[x]]$total$raw_alpha
  })
  
  # thirdly, we can also extract the standard error estimate of Cronbach's Alpha, according to Duhacheck & Iaboucci (2004).
  ase.est <- apply(matrix(c(1:length(alpha.fit)), ncol=1), MARGIN = 1, FUN = function(x) {
    alpha.fit[[x]]$total$ase
  })
  
  df <- data.frame(reliability = rel.est,
                   StandardError = ase.est,
                   source = unique(data$source))
  
  write.csv(df, paste0(here("Reliability Estimates/"), "/", "Alpha" ,scale_info, ".csv"), row.names = FALSE)
  
  # finally, the function returns a data.frame with 2 columns, 1 consiting of alpha estimates, one of standard error estimates
  return(df)
  
  
}


# The function over the lines 43:... is used to generate estimatesof McDonald's Omega and Standard Errors.

omega_extractor <- function(data, item.vec, scale_info){
  
  # firstly, apply the omega() function from the <coefficient alpha> library - this applies the omega function to the data from 
  #   each lab separately
  omega.fit <- apply(matrix(unique(data$source), ncol = 1), MARGIN = 1, FUN = function(x){
    coefficientalpha::omega(subset(data[,item.vec], subset = c(data$source == x)), se = T, varphi = 0, test = F)
  })
  
  # secondly, the results from the omega function are used - we can easily extract the McDonald's estimates from the object.
  rel.est <- apply(matrix(c(1:length(omega.fit)), ncol = 1), MARGIN = 1, FUN = function(x){
    omega.fit[[x]]$omega
  })
  
  # thirdly, we can also extract the standard error estimate of McDonald's Omega, according to ??.
  ose.est <- apply(matrix(c(1:length(omega.fit)), ncol = 1), MARGIN = 1, FUN = function(x){
    omega.fit[[x]]$se
  })
  
  df <- data.frame(reliability = rel.est,
                   StandardError = ose.est,
                   source = unique(data$source))
  
  write.csv(df, paste0(here("Reliability Estimates/"), "/", "Omega" ,scale_info, ".csv"), row.names = FALSE)
  
  # finally, the function returns a data.frame with 2 columns, 1 consisting of omega estimates, one of standard error estimates
  return(df)
}



# A second function to generate McDonald's Omega estimates is defined. This function used the <MBESS> package. However, this is
#   currently not implemented. It might be useful in the future, should estimation using psych break down. The function is defined
#   over the lines 72:87

omega_extractor2 <- function(data, item.vec){
  omega.fit <- apply(matrix(unique(data$source), ncol = 1), MARGIN = 1, FUN = function(x){
    MBESS::ci.reliability(subset(data[,item.vec], subset = c(data$source == x)), type = "omega")
  })
  
  rel.est <- apply(matrix(c(1:length(omega.fit)), ncol = 1), MARGIN = 1, FUN = function(x){
    omega.fit[[x]]$est
  })
  
  ose.est <- apply(matrix(c(1:length(omega.fit)), ncol = 1), MARGIN = 1, FUN = function(x){
    omega.fit[[x]]$se
  })
  
  return(data.frame(reliability = rel.est,
                    StandardError = ose.est))
}


# function to calc means, sd, n per group per replication attempt

rma_prep_function <- function(replication_df, factor.vec = c(0,1)){
  mean_0 <- apply(matrix(unique(replication_df$source), ncol=1), 1, FUN = function(x){
    mean(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[1])], na.rm=T)
  })
  
  mean_1 <- apply(matrix(unique(replication_df$source), ncol=1), 1, FUN = function(x){
    mean(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[2])], na.rm=T)
  })
  
  sd_0 <- apply(matrix(unique(replication_df$source), ncol=1), 1, FUN = function(x){
    sd(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[1])], na.rm=T)
    # sqrt(sum((replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[1])] -
    #            mean(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[1])], na.rm = T))^2, na.rm = T)/
    #        length(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[1])]))
  })
  
  sd_1 <- apply(matrix(unique(replication_df$source), ncol=1), 1, FUN = function(x){
    sd(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[2])], na.rm=T)
    # sqrt(sum((replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[2])] -
    #             mean(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[2])], na.rm = T))^2, na.rm = T)/
    #        length(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[2])]))
  })
  
  n_0 <- apply(matrix(unique(replication_df$source), ncol=1), 1, FUN = function(x){
    length(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[1])])
  })
  
  n_1 <- apply(matrix(unique(replication_df$source), ncol=1), 1, FUN = function(x){
    length(replication_df$DV[which(replication_df$source == x & replication_df$factor == factor.vec[2])])
  })
  
  source <- as.character(apply(matrix(unique(replication_df$source), ncol=1), 1, FUN = function(x){
    unique(replication_df$source[which(replication_df$source == x & replication_df$factor == factor.vec[2])])
  }))
  
  returndf <- data.frame(mean_0 = mean_0,
                         mean_1 = mean_1,
                         sd_0 = sd_0,
                         sd_1 = sd_1,
                         n_0 = n_0,
                         n_1 = n_1,
                         source = source)
  
  returndf[(nrow(returndf)+1),1:6] <- c(mean(replication_df$DV[(replication_df$factor == factor.vec[[1]])], na.rm = T),
                                     mean(replication_df$DV[(replication_df$factor == factor.vec[[2]])], na.rm = T),
                                     sd(replication_df$DV[(replication_df$factor == factor.vec[[1]])], na.rm = T),
                                     sd(replication_df$DV[(replication_df$factor == factor.vec[[2]])], na.rm = T),
                                     length(replication_df$DV[(replication_df$factor == factor.vec[[1]])]),
                                     length(replication_df$DV[(replication_df$factor == factor.vec[[2]])]))
  
  returndf[(nrow(returndf)), 7] <- "Total"
  
  return(returndf)
  
}

unw_d <- function(dataframe){
  unw_mean_1 <- mean(dataframe$DV[which(dataframe$factor == 1)], na.rm = T) 
  unw_mean_0 <- mean(dataframe$DV[which(dataframe$factor == 0)], na.rm = T) 
  unw_N_1 <- length(dataframe$DV[which(dataframe$factor == 1)])
  unw_N_0 <- length(dataframe$DV[which(dataframe$factor == 0)])
  unw_var_1 <- var(dataframe$DV[which(dataframe$factor == 1)], na.rm = T)
  unw_var_0 <- var(dataframe$DV[which(dataframe$factor == 0)], na.rm = T)
  
  unw_d <- (unw_mean_1 - unw_mean_0)/sqrt((unw_N_1*unw_var_1 + unw_N_0 + unw_var_0)/(unw_N_1 + unw_N_0))
  
  return(unw_d)
}



lab_d <- function(dataframe){
  d <- (dataframe$mean_1 - dataframe$mean_0)/sqrt((dataframe$n_1*dataframe$sd_1^2 + dataframe$n_0*dataframe$sd_0^2)/(dataframe$n_1 + dataframe$n_0))
  
  d <- data.frame(d = d, source = dataframe$source)
  return(d)
}

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

rma_effect <- function(dataframe, measure = "SMD"){
  dataframe <- dataframe[which(dataframe$source != "Total"),]
  metafor::rma(measure = measure, method="REML",
      m1i = dataframe$mean_1, m2i = dataframe$mean_0, 
      sd1i = dataframe$sd_1, sd2i = dataframe$sd_0,
      n1i = dataframe$n_1, n2i = dataframe$n_0, level = 95)
}

rma_effect2 <- function(dataframe, measure = "SMD"){
  dataframe <- dataframe[which(dataframe$source != "Total"),]
  metafor::rma(measure = measure, method="REML",
      m1i = round2(dataframe$mean_1, 2), m2i = round2(dataframe$mean_0, 2), 
      sd1i = round2(dataframe$sd_1, 2), sd2i = round2(dataframe$sd_0, 2),
      n1i = dataframe$n_1, n2i = dataframe$n_0, level = 95)
}

rma_effect3 <- function(dataframe, measure = "SMD"){
  dataframe <- dataframe[which(dataframe$source != "Total"),]
  es <- metafor::escalc(measure = "MD", m1i = dataframe$mean_1, m2i = dataframe$mean_0, 
                   sd1i = dataframe$sd_1, sd2i = dataframe$sd_0,
                   n1i = dataframe$n_1, n2i = dataframe$n_0, vtype = "HO")
  metafor::rma(measure = measure, method = "REML",
      yi = es$yi, sei = sqrt(es$vi),
      level = 95,
      test = "knha")
}


Bonett_prep <- function(df_rel, df_rma_prep, J){
  df_rma_prep <- df_rma_prep[-which(df_rma_prep$source == "Total"),]
  return(data.frame(reliability = df_rel$reliability,
                    n = df_rma_prep$n_1 + df_rma_prep$n_0,
                    j = rep(J, length(df_rel$reliability)),
                    source = df_rma_prep$source))
}

Bonett_transformation <- function(df_Bonett_prepped, scale_info){
  alpha <- df_Bonett_prepped$reliability
  n <- df_Bonett_prepped$n
  j <- df_Bonett_prepped$j
  
  coeff_t <- log(1 - abs(alpha))
  SE_coeff_t <- sqrt((2 * j)/((j - 1) * (n - 2)))
  
  df <- data.frame(reliability = coeff_t,
                   StandardError = SE_coeff_t,
                   source = unique(df_Bonett_prepped$source))
  
  write.csv(df, paste0(here("Reliability Estimates/"), "/", "Bonett" ,scale_info, ".csv"), row.names = FALSE)
  
  return(df)
}

rel_rma <- function(df){
  rma(measure = "GEN", yi = df$reliability, sei = df$StandardError)
}





rel_rma.reg <- function(df){
  rma(yi = reliability, sei = StandardError,
      mods = ~ lang + comp + sex + mean_age,
      data = df)
}




my_forest_plot <- function(rma.fit, rma.data, main.title = "Forest Plot", 
                           x.lab = "Estimate", ci.lvl = .975, CI.display = FALSE){
  
  # Calculate lower and upper limits of confidence levels, for each replication's estimate
  cil <- rma.fit$yi[1:length(rma.fit$yi)] - qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
  ciu <- rma.fit$yi[1:length(rma.fit$yi)] + qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
  
  
  
  p <- ggplot() + # initialize ggplot
    
    # plot point estimates
    geom_point(aes(x = rma.fit$yi, y = c(5:(length(rma.fit$yi)+4))), shape = 15) + 
    
    # vertical line at x = 0
    # geom_vline(xintercept = 0, linetype = "dashed") +
    
    # add horizontal line for CI of each replication's estimate
    geom_segment(aes(x = cil, y = c(5:(length(rma.fit$yi)+4)), xend = ciu, yend = c(5:(length(rma.fit$yi)+4)))) +
    
    # ggplot theme
    theme_minimal() +
    
    # plot meta analytic point estimate
    geom_point(aes(x = rma.fit$b[1], y = 1), shape = 18) +
    
    #add CI-line for meta-analytic point estimate
    geom_segment(aes(x = rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, y = 1, 
                     xend = rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, yend = 1)) +
    
    # add vertical upper & lower-limit "fence"-lines, for each replication's estimate
    geom_segment(aes(x = cil, xend = cil, y = (c(5:(length(rma.fit$yi)+4))+.3), yend = (c(5:(length(rma.fit$yi)+4))-.3) )) +
    geom_segment(aes(x = ciu, xend = ciu, y = (c(5:(length(rma.fit$yi)+4))+.3), yend = (c(5:(length(rma.fit$yi)+4))-.3) )) +
    
    # add vertical upper- & lower-limit "fence lines, for meta-analytic point estimate
    geom_segment(aes(x = rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, y = (1+.3), 
                     xend = rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, yend = (1-.3))) +
    geom_segment(aes(x = rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, y = (1+.3), 
                     xend = rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, yend = (1-.3))) +
    
    
    
    
    # labs & titles
    xlab(x.lab) +
    ylab("Lab") +
    ggtitle(main.title)
  
  if(CI.display){
    p <- p + 
      scale_y_continuous(breaks = c(1, (5:(length(rma.fit$yi)+4))), 
                         labels = c("RE Model", unique(as.character(rma.data$source))),
                         
                         sec.axis = dup_axis(breaks = c(1, (5:(length(rma.fit$yi)+4))),
                                             labels = c(paste0("[", round(rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, 2), ";", round(rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, 2), "]"), 
                                                        paste0("[", round(cil, 2), ";", round(ciu, 2), "]")),
                                             name = ""))
    # p <- p + geom_text(aes(y = c(5:(length(rma.fit$yi)+4)), x = (max(ciu) + abs(max(ciu))*.05),
    #               label = paste0("[", round(cil, 2), ";", round(ciu, 2), "]")))
  }else{
    p <- p +     # adjust labels on y-axis, to display lab-abreviations
      scale_y_continuous(breaks = c(1, (5:(length(rma.fit$yi)+4))), labels = c("RE Model", unique(as.character(rma.data$source)))) 
  }
  
  print(p)
}
