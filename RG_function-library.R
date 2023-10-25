### Reliability Generalization HEXACO ###

## 02/06/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "psych", "coefficientalpha", "spsUtil", "future.apply", "boot")

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


## A function to estimate Cronbach's Alpha, using the package psych

#  data-argument expects a data.frame. This data.frame may only contain columns containing
# responses concerning a respective item and a single column containg the source-ID, named
# "source" 
estimate_alpha <- function(data, csv = FALSE, project.title = NULL){
  
  # compute nr. of groups
  k <- length(unique(data$source))
  # find index of "source"
  s.idx <- grep("source", names(data))
  
  # apply-loop to generate estimate of Cronbach's Alpha and its Standard Error, using psych
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    
    # identify responses belonging to group of current iteration, remove source column
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    # compute Cronbach' Alpha & Standard Error - mute psych-output to console
    spsUtil::quiet(psych::alpha(d))
  })  
  
  # store results in a data.frame
  df <- data.frame(Reliability = sapply(est, FUN = function(x){x$total$raw_alpha}),
                   StandardError = sapply(est, FUN = function(x){x$total$ase}),
                   source = unique(data$source))
  
  # if desired, store data.frame in a .csv-file
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Alpha.csv")), row.names = FALSE)
  }
  
  return(df)
}


## A function to estimate the Bonnett-transform of Cronbach's Alpha

#  data-argument expects a data.frame. This data.frame may only contain columns containing
# responses concerning a respective item and a single column containg the source-ID, named
# "source" 
estimate_Bonett_alpha <- function(data, csv = FALSE, project.title = NULL){
  
  # compute nr. of groups
  k <- length(unique(data$source))
  # identify "source"-colum
  s.idx <- grep("source", names(data))
  # compute nr. of items
  j <- length(names(data[,-s.idx]))
  # compute group-sample size
  n <- data %>%
    group_by(source) %>%
    summarise(n = n())
  n <- n$n
  
  # apply-loop to generate estimate of Cronbach's Alpha and its Standard Error, using psych
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    
    # identify responses belonging to group of current iteration, remove source column
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    # compute Cronbach' Alpha & Standard Error - mute psych-output to console
    spsUtil::quiet(psych::alpha(d))
  })  
  
  # extract alpha-estimates from list
  Alpha <- sapply(est, FUN = function(x){x$total$raw_alpha})
  
  # generate Bonett-transform of Cronbach's Alpha
  B.Alpha <- log(1 - Alpha)
  
  # generate Standard Error of Bonett-transform of Cronbach's Alpha
  SE_B.Alpha <- sqrt((2 * j)/((j - 1) * (k - 2)))                
                  
  # store results in a data.frame
  df <- data.frame(Reliability = B.Alpha,
                   StandardError = SE_B.Alpha,
                   source = unique(data$source))
  
  # if desired, store results in a .csv-file
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Bonett-Alpha.csv")), row.names = FALSE)
  }
  
  return(df)
}






bootstrap_SE_varT <- function(data, indices, stat = "ALPHA"){
  
  d <- data[indices,]
  
  if(stat == "ALPHA"){
    # alpha_fit <- psych::alpha(d, warnings = FALSE)
    # 
    # alpha <- alpha_fit$total[1]
    C <- cov(d)
    n <- dim(C)[1]
    
    alpha <- (1 - sum(diag(C))/sum(C)) * (n/(n - 1))
    
    
    rel <- alpha
  }
  if(stat == "OMEGA"){
    omega_fit <- coefficientalpha::omega(d, se = F, varphi = 0, test = F)
    
    omega <- omega_fit$omega
    
    rel <- omega
  }
  
  
  var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
  
  var_T <- as.numeric(rel * var_X)
  
  return(var_T)
  
}



bootstrap_SE_varE <- function(data, indices, stat = "ALPHA"){
  
  d <- data[indices,]
  
  if(stat == "ALPHA"){
    # alpha_fit <- psych::alpha(d, warnings = FALSE)
    # 
    # alpha <- alpha_fit$total[1]
    # 
    C <- cov(d)
    n <- dim(C)[1]
    
    alpha <- (1 - sum(diag(C))/sum(C)) * (n/(n - 1))
    
    rel <- alpha
  }
  if(stat == "OMEGA"){
    omega_fit <- coefficientalpha::omega(d, se = F, varphi = 0, test = F)
    
    omega <- omega_fit$omega
    
    rel <- omega
  }
  
  var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
  
  var_T <- as.numeric(rel * var_X)
  
  var_E <- var_X - var_T
  
  return(var_E)
  
}



apply_Bootstrap_SE_Project.specific <- function(data, var.component = c("TRUE", "ERROR"), R = 100){
  if(length(var.component) != 1){
    stop("Set var.component as either TRUE or ERROR.")
  }
  if(var.component == "TRUE"){
    stat.f <- bootstrap_SE_varT
  }
  if(var.component == "ERROR"){
    stat.f <- bootstrap_SE_varE
  }
  suppressMessages(
  df <- apply(as.matrix(seq_along(unique(data$source))), MARGIN = 1, FUN = function(x){
    bvar <- boot(data = na.omit(data[data$source == unique(data$source)[x],-grep("source", names(data))]),
                 statistic = stat.f,
                 stat = "ALPHA",
                 R = R)
    
    d <- data[data$source == unique(data$source)[x],-grep("source", names(data))]
    
    D <- na.omit(d)
    
    C <- cov(D)
    n <- dim(C)[1]
    
    alpha <- (1 - sum(diag(C))/sum(C)) * (n/(n - 1))
    
    varX <- var(rowMeans(D))
    
    if(var.component == "TRUE"){
      var_est <- as.numeric(varX * alpha )
    }
    if(var.component == "ERROR"){
      var_est <- as.numeric(varX * (1-alpha))
    }
    
    return(data.frame(SE = sd(log(bvar$t)), 
                      boot.mean = mean(log(bvar$t)),
                      var.emp = log(var_est)))
  })
  )
  
  df.formatted <- data.frame(SE = sapply(df, FUN = function(x){x$SE}),
                             boot.mean = sapply(df, FUN = function(x){x$boot.mean}),
                             var.est = sapply(df, FUN = function(x){x$var.emp}),
                             source = unique(data$source))
  
}

apply_Bootstrap_SE_nonspecific <- function(data.L, var.component = c("TRUE", "ERROR"), R = 100){
  if(length(var.component) != 1){
    stop("Set var.component as either TRUE or ERROR.")
  }
  if(!is.list(data.L)){
    stop("data.L needs to be a List.")
  }
  if(var.component == "TRUE"){
    stat.f <- bootstrap_SE_varT
  }
  if(var.component == "ERROR"){
    stat.f <- bootstrap_SE_varE
  }
  suppressMessages(
    vboot.L <- lapply(data.L, FUN = function(x){
      bvar <- boot(data = x,
                   statistic = stat.f,
                   stat = "ALPHA",
                   R = R)
      
      
      #C <- cov(x[,-grep("source", names(x))])
      C <- cov(x)
      n <- dim(C)[1]
      
      alpha <- (1 - sum(diag(C))/sum(C)) * (n/(n - 1))
      
      #varX <- var(rowMeans(x[,-"source"]))
      varX <- var(rowMeans(x))
      
      if(var.component == "TRUE"){
        var_est <- varX * alpha 
      }
      if(var.component == "ERROR"){
        var_est <- varX * (1-alpha)
      }
      
      
      return(list(SE = sd(log(bvar$t)), 
                  boot.mean = mean(log(bvar$t)),
                  var.emp = log(var_est),
                  boot.est = log(bvar$t)))
    })
  )
  
  df.formatted <- data.frame(SE = sapply(vboot.L, FUN = function(x){x$SE}),
                             boot.mean = sapply(vboot.L, FUN = function(x){x$boot.mean}),
                             var.est = sapply(vboot.L, FUN = function(x){x$var.emp}))
  
  return(list(df.formatted = df.formatted,
              boot.est = lapply(vboot.L, FUN = function(x){x$boot.est})))
  
}


future_apply_Bootstrap_SE_nonspecific <- function(data.L, var.component = c("TRUE", "ERROR"), R = 100){
  if(length(var.component) != 1){
    stop("Set var.component as either TRUE or ERROR.")
  }
  if(!is.list(data.L)){
    stop("data.L needs to be a List.")
  }
  if(var.component == "TRUE"){
    stat.f <- bootstrap_SE_varT
  }
  if(var.component == "ERROR"){
    stat.f <- bootstrap_SE_varE
  }
  suppressMessages(
    vboot.L <- future_lapply(data.L, future.seed = TRUE, FUN = function(x){
      bvar <- boot(data = x,
                   statistic = stat.f,
                   stat = "ALPHA",
                   R = R)
      
      C <- cov(x[,-grep("source", names(x))])
      j <- dim(C)[1]
      
      alpha <- (1 - sum(diag(C))/sum(C)) * (j/(j - 1))
      
      varX <- var(rowMeans(x[,-"source"]))
      
      if(var.component == "TRUE"){
       var_est <- varX * alpha 
      }
      if(var.component == "ERROR"){
        var_est <- varX * (1-alpha)
      }
      
      return(list(SE = sd(bvar$t), 
                  boot.mean = mean(bvar$t),
                  var.emp = var_est))
    })
  )
  
  df.formatted <- data.frame(SE = sapply(vboot.L, FUN = function(x){x$SE}),
                             boot.mean = sapply(vboot.L, FUN = function(x){x$boot.mean}),
                             var.est = sapply(vboot.L, FUN = function(x){x$var.emp}))
  
}



my_forest_plot <- function(rma.fit, rma.data, main.title = "Forest Plot", 
                           x.lab = "Estimate", ci.lvl = .975, CI.display = FALSE){
  
  # Calculate lower and upper limits of confidence levels, for each replication's estimate
  cil <- rma.fit$yi[1:length(rma.fit$yi)] - qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
  ciu <- rma.fit$yi[1:length(rma.fit$yi)] + qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
  
  
  # weights <- 1/sqrt(rma.fit$vi+rma.fit$tau2)
  # weights.scaled <- weights/mean(weights)
  # weights.rescaled <- weights.scaled/mean(weights.scaled)
  
  p <- ggplot() + # initialize ggplot
    
    # plot point estimates
    geom_point(aes(x = rma.fit$yi, y = c(5:(length(rma.fit$yi)+4))
    #                , size = weights.rescaled
                   ), shape = 15) + 
    
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
  
  p
}


# function to simulate data with heterogeneity in true and/or error score variance
#  User may change nr. of items (j), sample size (n), nr. of samples (k), reliability, mean_score,
#  mean observed variance (mean_observed_var), heterogeneity in true score variance - CV (CV_var_T),
#  heterogeneity in error score variance - CV (CV_var_E), alternatively (!) heterogeneity in true
#  score variance - tau (tau_var_T), heterogeneity in error score variance - tau (tau_var_E), 
#  whether sampled data should show the variance exactly (empirical = TRUE), or not (FALSE)
# Heterogeneity can only be supplied EITHER in terms of tau (sd), or CV (sd/mu)!
sim_het_VC <- function(j, n, k, reliability = 0.5, mean_score = 0, mean_observed_var = 10, 
                       CV_var_T = 0, CV_var_E = 0, tau_var_T = 0, tau_var_E = 0, 
                       empirical = FALSE){
  
  # generate means for true and error score variance components!
  mean_var_T <- mean_observed_var * reliability
  mean_var_E <- mean_observed_var - mean_var_T
  
  # making sure heterogeneity is supplied EITHER in terms of tau OR CV 
  if(CV_var_E != 0 | CV_var_T !=0){
    if(tau_var_T != 0 | tau_var_E != 0){
      stop("Supply heterogeneity in heterogeneity either in terms of the coefficient of variation (CV) or in terms of tau - not both!")
    }
  } 
  
  # generate estimates of tau, if CV was supplied
  if(CV_var_E != 0 | CV_var_T !=0){
    tau_var_T <- mean_var_T * CV_var_T
    tau_var_E <- mean_var_E * CV_var_E
  }
  
  # compute heterogeneity of log-transformed score variance estimates
  tau_ln_var_T <- sqrt(log(((tau_var_T^2) / (mean_var_T^2)) + 1))
  tau_ln_var_E <- sqrt(log(((tau_var_E^2) / (mean_var_E^2)) + 1))
  
  # compute mean of log-transformed score variance esitmates
  mu_ln_var_T <- log(mean_var_T) - (1/2) * tau_ln_var_T^2
  mu_ln_var_E <- log(mean_var_E) - (1/2) * tau_ln_var_E^2
  
  # sample log-trannsformed score variance estimates from normal distribution
  ln_true_var <- MASS::mvrnorm(n = k, 
                               mu = mu_ln_var_T, 
                               Sigma = tau_ln_var_T^2,
                               empirical = empirical)
  ln_error_var <- MASS::mvrnorm(n = k, 
                                mu = mu_ln_var_E, 
                                Sigma = tau_ln_var_E^2,
                                empirical = empirical)
  
  # back-transform to original scale of variances
  true_var <- exp(ln_true_var)
  error_var <- exp(ln_error_var)

  # generate k samples, looped using apply-function
  sim_d.L <- lapply(as.matrix(1:k), FUN = function(x){
    
    # extract respective sampled true & error score variance component
    var_T1 <- true_var[x]
    var_E1 <- error_var[x]*j
    
    # construct covariance matrix to sample from multivariate normal distr.
    # constructed according to Lord & Novick (2008)
    mat <- matrix(var_T1, nrow = j, ncol = j)
    diag(mat) <- var_T1 + var_E1
    
    # generate sample data
    obs_scores <- MASS::mvrnorm(n = n, mu = rep(mean_score, j), Sigma = mat,
                                empirical = FALSE)
    
    sim_data <- obs_scores
    
    # return sample data
    return(sim_data)
  })
  
  return(sim_d.L)
}
