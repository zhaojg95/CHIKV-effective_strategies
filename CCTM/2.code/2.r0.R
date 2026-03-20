# calculate R0
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
  w_m = omega_m
  Nm <- k*Np*0.859
  w_p = omega_p
  
  R00 <- c()
  for(i in 1:control_time){
    T_shift = 30
    T_period = 365 
    c <- 0.5 * (cos(2 * pi * (i - T_shift) / T_period) + 1)
    #
    ## 基本再生数
    R00[i] <-   (b*((Nm*a*beta_mp0*beta_pm0*c*e_p*lambda*omega_m*(gamma1 - gamma1*q + eta*gamma*q))/(Np*gamma1*gamma*(mu_m + w_m)*(mu_a + e_p*lambda - e_p*mu_a)))^(1/2))/mu_m 
  }
  R001 <- median(R00)
  R0 <- c(R0,R00)
  
  
}


R0
quantile(R0,0.025)
median(R0)
quantile(R0,0.975)
