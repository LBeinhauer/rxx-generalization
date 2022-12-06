### Reliability Generalization HEXACO ###

## 02/06/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "psych", "coefficientalpha", "spsUtil", "future.apply")

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




estimate_alpha <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    spsUtil::quiet(psych::alpha(d))
  })  
  
  df <- data.frame(Reliability = sapply(est, FUN = function(x){x$total$raw_alpha}),
                   StandardError = sapply(est, FUN = function(x){x$total$ase}),
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Alpha.csv")), row.names = FALSE)
  }
  
  return(df)
}




estimate_omega <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  
  est <- sapply(1:k, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    fit <- spsUtil::quiet(coefficientalpha::omega(d, se = TRUE, test = FALSE, silent = TRUE))
    
    return(c(fit$omega, fit$se))
  })  
  
  df <- data.frame(Reliability = t(est)[,1],
                   StandardError = t(est)[,2],
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Omega.csv")), row.names = FALSE)
  }
  
  return(df)
}


estimate_Bonett_alpha <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  j <- k-1
  n <- data %>%
    group_by(source) %>%
    summarise(n = n())
  n <- n$n
  
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    spsUtil::quiet(psych::alpha(d))
  })  
  
  Alpha <- sapply(est, FUN = function(x){x$total$raw_alpha})
  
  B.Alpha <- log(1 - Alpha)
  SE_B.Alpha <- sqrt((2 * j)/((j - 1) * (n - 2)))                
                  
  df <- data.frame(Reliability = B.Alpha,
                   StandardError = SE_B.Alpha,
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Bonett-Alpha.csv")), row.names = FALSE)
  }
  
  return(df)
}


estimate_Bonett_omega <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  j <- k-1
  n <- data %>%
    group_by(source) %>%
    summarise(n = n())
  n <- n$n
  
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    fit <- spsUtil::quiet(coefficientalpha::omega(d, se = TRUE, test = FALSE, silent = TRUE))
  })  
  
  Omega <- sapply(est, FUN = function(x){x$omega})
  
  B.Omega <- log(1 - Omega)
  SE_B.Omega <- sqrt((2 * j)/((j - 1) * (n - 2)))                
  
  df <- data.frame(Reliability = B.Omega,
                   StandardError = SE_B.Omega,
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Bonett-Omega.csv")), row.names = FALSE)
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
    bvar <- boot(data = data[data$source == unique(data$source)[x],-grep("source", names(data))],
                 statistic = stat.f,
                 stat = "ALPHA",
                 R = R)
    
    return(data.frame(SE = sd(bvar$t), 
                      boot.mean = mean(bvar$t)))
  })
  )
  
  df.formatted <- data.frame(SE = sapply(df, FUN = function(x){x$SE}),
                             boot.mean = sapply(df, FUN = function(x){x$boot.mean}),
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
      
      return(list(SE = sd(bvar$t), 
                  boot.mean = mean(bvar$t)))
    })
  )
  
  df.formatted <- data.frame(SE = sapply(vboot.L, FUN = function(x){x$SE}),
                             boot.mean = sapply(vboot.L, FUN = function(x){x$boot.mean}))
  
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
      
      return(list(SE = sd(bvar$t), 
                  boot.mean = mean(bvar$t)))
    })
  )
  
  df.formatted <- data.frame(SE = sapply(vboot.L, FUN = function(x){x$SE}),
                             boot.mean = sapply(vboot.L, FUN = function(x){x$boot.mean}))
  
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


sim_het_VC <- function(j, n, k, reliability = 0.5, mean_score = 0, mean_observed_var = 10, 
                       CV_var_T = 0, CV_var_E = 0, tau_var_T = 0, tau_var_E = 0, 
                       empirical = FALSE){
  
  mean_var_T <- mean_observed_var * reliability
  
  mean_var_E <- mean_observed_var - mean_var_T
  
  if(CV_var_E != 0 | CV_var_T !=0){
    if(tau_var_T != 0 | tau_var_E != 0){
      stop("Supply heterogeneity in heterogeneity either in terms of the coefficient of variation (CV) or in terms of tau - not both!")
    }
  } 
  
  if(CV_var_E != 0 | CV_var_T !=0){
    tau_var_T <- mean_var_T * CV_var_T
    tau_var_E <- mean_var_E * CV_var_E
  }
  
  
  true_var <- truncnorm::rtruncnorm(n = k, mean = mean_var_T, sd = tau_var_T, a = 0)
  
  error_var <- truncnorm::rtruncnorm(n = k, mean = mean_var_E, sd = tau_var_E, a = 0)
  
  sim_d.L <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    
    var_T1 <- true_var[x]
    
    var_E1 <- error_var[x]*j
    
    mat <- matrix(var_T1, nrow = j, ncol = j)
    
    diag(mat) <- var_T1 + var_E1
    
    obs_scores <- mvrnorm(n = n, mu = rep(mean_score, j), Sigma = mat,
                          empirical = empirical)
    
    sim_data <- obs_scores
    
    # rel <- spsUtil::quiet(psych::alpha(sim_data, warnings = FALSE))
    
    return(list(# reliability = rel$total$raw_alpha,
                # StandardError = rel$total$ase,
                data = sim_data))
  })
  
  sim_data.L <- lapply(sim_d.L, FUN = function(x){x$data})
  
  # sim_d.df <- data.frame(Reliability = sapply(sim_d.L, FUN = function(x){x$reliability}),
  #                        StandardError = sapply(sim_d.L, FUN = function(x){x$StandardError}))
  
  return(list(sim_data.L = sim_data.L #,
              # reliability.df = sim_d.df
              ))
}
