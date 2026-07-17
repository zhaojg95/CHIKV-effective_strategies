
# 1. baseline scenario ----------------------------------------
CHIKV_model <- function(t, Y, parms) {
  #The CHIKV model is divided into three compartments:
  # aquatic stage mosquitoes(Sa\Ia), adult stage mosquitoes(Sm\Em\Im),and human population(Sp\Ep\Ip\Rp)

  with(as.list(c(Y, parms)), {
    # period function - seasonal 
    T_period <- 365
    T_shift  <- 30
    c <- 0.5 * (cos(2*pi*(t - T_shift)/T_period) + 1)

    # population
    Np <- Sp + Ep + Ip + Ap + Rp
    Nm <- Sm + Im + Em
    # ODE function
    dSa  <- a * c * (Nm - n * Im) - lambda * e_p * Sa - (1-e_p)*mu_a *Sa
    dIa  <- a * c * n * Im       - lambda * e_p * Ia - (1-e_p) *mu_a *Ia

    dSm  <- lambda * e_p * Sa - beta_pm0 * b  * Sm * (Ip + eta * Ap) / Np - mu_m * Sm
    dEm  <- beta_pm0 * b * Sm * (Ip + eta * Ap) / Np - omega_m * Em - mu_m * Em
    dIm  <- lambda * e_p * Ia + omega_m * Em - mu_m * Im

    dSp  <- -beta_mp0 * b  * Sp * Im / Np
    dEp  <-  beta_mp0 * b  * Sp * Im / Np - omega_p * Ep
    dIp  <- (1 - q) * omega_p * Ep - gamma * Ip
    dAp  <- q * omega_p * Ep - gamma1 * Ap
    dRp  <- gamma * Ip + gamma1 * Ap

    #sum - new cases
    dX   <- (1 - q) * omega_p * Ep

    list(c(dSa, dIa, dSm, dEm, dIm, dSp, dEp, dIp, dAp, dRp, dX))
  })
}



# 2.combined intervention measures ----------------------------------------
CHIKV_model_with_combind_intervention <- function(t, Y, parms) {
  
  with(as.list(c(Y, parms)), {
    # period function
    T_period <- 365
    T_shift  <- 30
    c <- 0.5 * (cos(2*pi*(t - T_shift)/T_period) + 1)
    
    #aquatic stage mosquitoes
    if(t <= control_time) ua_c = 0
    if(t > control_time) ua_c = ua_c
    
    #adult stage mosquitoes
    if(t <= control_time) um_c = 0
    if(t > control_time) um_c = um_c
    
    # reduce the bite rate
    if(t <= control_time) R_bite = 0
    if(t > control_time) R_bite = R_bite
    
    # infectious period shortening /transmission_time
    if(t <= control_time) gamma = 1/7
    if(t <= control_time) gamma1 = 1/7
    if(t > control_time) gamma = R_gamma
    if(t > control_time) gamma1 = R_gamma1
    

    # ODE function
    
    # population
    Np <- Sp + Ep + Ip + Ap + Rp
    Nm <- Sm + Im + Em
    dSa  <- a  * c * (Nm - n * Im) - lambda * e_p * Sa - (1-e_p) *mu_a *Sa - ua_c*Sa 
    dIa  <- a * c * n * Im       - lambda * e_p * Ia - (1-e_p) *mu_a *Ia- ua_c*Ia
    
    dSm  <- lambda * e_p * Sa - beta_pm0 * b *(1-R_bite) * Sm * (Ip + eta * Ap) / Np - mu_m * Sm - um_c*Sm
    dEm  <- beta_pm0 * b*(1-R_bite) * Sm * (Ip + eta * Ap) / Np - omega_m * Em - mu_m * Em - um_c*Em
    dIm  <- lambda * e_p * Ia + omega_m * Em - mu_m * Im - um_c*Im
    
    dSp  <- -beta_mp0 * b *(1-R_bite) * Sp * Im / Np
    dEp  <-  beta_mp0 * b *(1-R_bite) * Sp * Im / Np - omega_p * Ep
    dIp  <- (1 - q) * omega_p * Ep - gamma * Ip
    dAp  <- q * omega_p * Ep - gamma1 * Ap
    dRp  <- gamma * Ip + gamma1 * Ap
    
    #sum - new cases
    dX   <- (1 - q) * omega_p * Ep  
    
    #sum - Removed aquatic & adult mosquitoes
    
    dMaqua <-  ua_c*Sa +ua_c*Ia
    dMadult <-  um_c*Sm + um_c*Em + um_c*Im
    
    list(c(dSa, dIa, dSm, dEm, dIm, dSp, dEp, dIp, dAp, dRp, dX, dMaqua , dMadult))
  })
}







