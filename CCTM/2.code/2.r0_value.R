# calculate R0
rm(list = ls())
source("2.code/init_value & fixed parameters.R")
list2env(as.list(parms.fixed), envir = .GlobalEnv)

set.seed(2025)
posterior_samples <- readRDS("3.result/posterior_samples_foshan.rds") %>% 
  .[sample(nrow(.), 100), ]

control_time = 31
R0 <- c()

for (j in 1:nrow(posterior_samples)) {
  k = posterior_samples[j,"k"]
  beta_mp0 = posterior_samples[j,"beta_mp0"]
  beta_pm0 = posterior_samples[j,"beta_pm0"]
  Nm <- k*Np*0.859
  
  R00 <- c()
  for(i in 1:control_time){
    T_shift = 30
    T_period = 365 
    c <- 0.5 * (cos(2 * pi * (i - T_shift) / T_period) + 1)
    #
    ## 
    v11 = mu_a + e_p * lambda - e_p * mu_a
    v22 = mu_m + omega_m
    
    alpha1 = a * c *n *lambda * e_p/(mu_m*v11)
    A = beta_pm0 * b *lambda*e_p*a*c*Nm*(gamma1-gamma1*q+eta*gamma*q)/(Np*gamma*gamma1*mu_m*v11)
    beta2 = beta_mp0 * b *omega_m/(mu_m * v22)
    
    R00[i] = (alpha1+sqrt(alpha1^2+4*A*beta2))/2
    
  }
  R001 <- median(R00)
  R0 <- c(R0,R001)
}


R0
quantile(R0,0.025)
median(R0)
quantile(R0,0.975)
