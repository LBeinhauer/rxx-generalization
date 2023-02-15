


summary(alpha_estimates.list[[1]]$Reliability)


Alpha_rma.list$Cacioppo_Need_Cognition


Bonett.Alpha_rma.list$Cacioppo_Need_Cognition


1 - exp(Bonett.Alpha_rma.list$Cacioppo_Need_Cognition$b[1])
1 - exp(Bonett.Alpha_rma.list$Cacioppo_Need_Cognition$ci.lb)
1 - exp(Bonett.Alpha_rma.list$Cacioppo_Need_Cognition$ci.ub)




summary(omega_estimates.list[[1]]$Reliability)

Omega_rma.list$Cacioppo_Need_Cognition

Bonett.Omega_rma.list$Cacioppo_Need_Cognition

1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$b[1])
1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$ci.lb)
1 - exp(Bonett.Omega_rma.list$Cacioppo_Need_Cognition$ci.ub)



varT_rma.list$Cacioppo_Need_Cognition$yi
sqrt(varT_rma.list$Cacioppo_Need_Cognition$vi)

summary(varT_rma.list$Cacioppo_Need_Cognition$yi)

varE_rma.list$Cacioppo_Need_Cognition$yi
sqrt(varE_rma.list$Cacioppo_Need_Cognition$vi)





varT_rma.list$Cacioppo_Need_Cognition$yi + varE_rma.list$Cacioppo_Need_Cognition$yi
varX_rma.list$Cacioppo_Need_Cognition$yi


# varT_yi & varE_yi currently bootstrapped means -> keep or change?
plot(varT_rma.list$Cacioppo_Need_Cognition$yi + varE_rma.list$Cacioppo_Need_Cognition$yi,
     varX_rma.list$Cacioppo_Need_Cognition$yi)


varT_rma.list$Cacioppo_Need_Cognition
varE_rma.list$Cacioppo_Need_Cognition

