# Metapopulation Model for Foshan (5 Districts) -- no intervention ---------------------------

Foshan_Meta_Model <- function(t, Y, parms) {
  
  # Y is a long vector, reshape it to Matrix: Rows = 5 Districts, Cols = Variables
  # Order: Sa, Ia, Sm, Em, Im, Sp, Ep, Ip, Ap, Rp, Xp
  # 5 districts * 11 compartments = 55 equations
  num_districts <- 5
  state_mat <- matrix(Y, nrow = num_districts, ncol = 11)
  colnames(state_mat) <- c("Sa", "Ia", "Sm", "Em", "Im", "Sp", "Ep", "Ip", "Ap", "Rp", "Xp")
  
  # Extract states for easier reading (These are vectors of length 5 now)
  Sa <- state_mat[, "Sa"]; Ia <- state_mat[, "Ia"]
  Sm <- state_mat[, "Sm"]; Em <- state_mat[, "Em"]; Im <- state_mat[, "Im"]
  Sp <- state_mat[, "Sp"]; Ep <- state_mat[, "Ep"]; Ip <- state_mat[, "Ip"]
  Ap <- state_mat[, "Ap"]; Rp <- state_mat[, "Rp"]; Xp <- state_mat[, "Xp"]
  
  with(as.list(parms), {
    
    # --- 1. Environmental & Seasonal Functions ---
    T_period <- 365
    T_shift  <- 30
    c_season <- 0.5 * (cos(2*pi*(t - T_shift)/T_period) + 1)
    
    # --- 2. Migration Dynamics (Time Dependent) ---
    
    # A. External Migration (Inter-city: Foshan <-> Other Cities)
    # Get values from interpolated functions (approxfun) defined in main script
    BD_in_t  <- func_BD_in(t)  # Total In-flow index to Foshan
    BD_out_t <- func_BD_out(t) # Total Out-flow index from Foshan
    
    # Calculate External Rates per district 
    # r_in/out[i] depends on District Pop / Total Pop ratio
    ratio_pop <- Pop_dis / sum(Pop_dis) 
    
    D_in_dis  <- BD_in_t  * ratio_pop
    D_out_dis <- BD_out_t * ratio_pop
    
    r_in_dis  <- (D_in_dis  * rho_mig) / Pop_dis # Vector length 5
    r_out_dis <- (D_out_dis * rho_mig) / Pop_dis # Vector length 5
    
    # B. Internal Migration (Intra-city: Between 5 Districts)
    # Gravity Model Intensity 
    # r_intra : human mobility between districts
    r_intra <- phi * ((BD_in_t + BD_out_t) * rho_mig) / (2 * sum(Pop_dis))
    
    # Gravity Matrix M (5x5) is pre-calculated in parameters
    # Flow Calculation: Net flow for each compartment 
    # Flow_In[i] = Sum_{j≠i}(r_intra * M_ji * X_j * Pj)
    # Flow_Out[i] = Sum_{j≠i}(r_intra * M_ij * X_i * Pi)
    
    calc_internal_flow <- function(State_Vec) {
      Flow_In  <- numeric(num_districts)
      Flow_Out <- numeric(num_districts)
      
      for (i in 1:num_districts) {
        # Inflow to i from all j≠i
        for (j in 1:num_districts) {
          if (j != i) {
            Flow_In[i] <- Flow_In[i] + r_intra * M_matrix[j, i] * State_Vec[j] * Pop_dis[j]
          }
        }
        # Outflow from i to all j≠i
        for (j in 1:num_districts) {
          if (j != i) {
            Flow_Out[i] <- Flow_Out[i] + r_intra * M_matrix[i, j] * State_Vec[i] * Pop_dis[i]
          }
        }
      }
      return((Flow_In - Flow_Out) / Pop_dis)  
    }
    
    # Calculate net flows for humans 
    Flow_Sp <- calc_internal_flow(Sp / Pop_dis)
    Flow_Ep <- calc_internal_flow(Ep / Pop_dis)
    Flow_Ip <- calc_internal_flow(Ip / Pop_dis)
    Flow_Ap <- calc_internal_flow(Ap / Pop_dis)
    Flow_Rp <- calc_internal_flow(Rp / Pop_dis)
    
    # --- 3. Population Sizes ---
    Np <- Sp + Ep + Ip + Ap + Rp      # Human Population (Vector)
    
    # k
    k_vec <- numeric(num_districts)
    for(i in 1:num_districts) {
      k_name <- paste0("k_", Districts[i])
      if(k_name %in% names(parms)) {
        k_vec[i] <- parms[[k_name]]
      } else {
        k_vec[i] <- k  
      }
    }
    
    # Total adult mosquitoes 
    # Nm_total <- k_vec * Np * e_p      
    Nm_total <- Sm + Em + Im
    # --- 4. Force of Infection ---
    beta_mp0_vec <- numeric(num_districts)
    beta_pm0_vec <- numeric(num_districts)
    
    for(i in 1:num_districts) {
      # beta_mp0 in different districts
      beta_mp_name <- paste0("beta_mp0_", Districts[i])
      if(beta_mp_name %in% names(parms)) {
        beta_mp0_vec[i] <- parms[[beta_mp_name]]
      } else if("beta_mp0" %in% names(parms)) {
        beta_mp0_vec[i] <- beta_mp0 
      } else {
        beta_mp0_vec[i] <- 0.15 
      }
      
      # beta_pm0 in different districts
      beta_pm_name <- paste0("beta_pm0_", Districts[i])
      if(beta_pm_name %in% names(parms)) {
        beta_pm0_vec[i] <- parms[[beta_pm_name]]
      } else if("beta_pm0" %in% names(parms)) {
        beta_pm0_vec[i] <- beta_pm0  
      } else {
        beta_pm0_vec[i] <- 0.30  
      }
    }
    
    # Force of infection for humans : β_mp0 * b * Im / Np
    FOI_to_Human <- beta_mp0_vec * b * Im / Np
    
    # Force of infection for mosquitoes : β_pm0 * b * (Ip + η*Ap) / Np
    FOI_to_Mosq <- beta_pm0_vec * b * (Ip + eta * Ap) / Np
    
    # --- 5. ODE Equations ---
    
    #Aquatic stage
    dSa <- a * c_season * (Nm_total - n * Im) - lambda * e_p * Sa - (1 - e_p) * mu_a * Sa
    dIa <- a * c_season * n * Im - lambda * e_p * Ia - (1 - e_p) * mu_a * Ia
    
    # Adult stage
    dSm <- lambda * e_p * Sa - FOI_to_Mosq * Sm - mu_m * Sm
    dEm <- FOI_to_Mosq * Sm - omega_m * Em - mu_m * Em
    dIm <- lambda * e_p * Ia + omega_m * Em - mu_m * Im
    
    # Human (With Migration)
 
    dSp <- -FOI_to_Human * Sp - theta_d * Sp + 
      r_in_dis * p_e_S * Np - r_out_dis * Sp + Flow_Sp*Sp
    
    dEp <- FOI_to_Human * Sp - omega_p * Ep - theta_d * Ep + 
      r_in_dis * p_e_E * Np - r_out_dis * Ep + Flow_Ep*Ep
    
    dIp <- (1 - q) * omega_p * Ep - gamma * Ip - theta_d * Ip + 
      r_in_dis * p_e_I * Np - r_out_dis * Ip + Flow_Ip*Ip
    
    dAp <- q * omega_p * Ep - gamma1 * Ap - theta_d * Ap + 
      r_in_dis * p_e_A * Np - r_out_dis * Ap + Flow_Ap*Ap
    
    dRp <- gamma * Ip + gamma1 * Ap - theta_d * Rp + 
      r_in_dis * p_e_R * Np - r_out_dis * Rp + Flow_Rp*Rp
    
    # Cumulative Cases
    dXp <- (1 - q) * omega_p * Ep
    
    # Flatten results back to a vector
    res <- c(dSa, dIa, dSm, dEm, dIm, dSp, dEp, dIp, dAp, dRp, dXp)
    
    return(list(res))
  })
}


# Metapopulation Model for Foshan (5 Districts) -- with intervention ---------------------------

Foshan_Meta_Model_intervention <- function(t, Y, parms) {
  
  # Y is a long vector, reshape it to Matrix: Rows = 5 Districts, Cols = Variables
  # Order: Sa, Ia, Sm, Em, Im, Sp, Ep, Ip, Ap, Rp, Xp
  # 5 districts * 11 compartments = 55 equations
  num_districts <- 5
  state_mat <- matrix(Y, nrow = num_districts, ncol = 11)
  colnames(state_mat) <- c("Sa", "Ia", "Sm", "Em", "Im", "Sp", "Ep", "Ip", "Ap", "Rp", "Xp")
  
  # Extract states for easier reading (These are vectors of length 5 now)
  Sa <- state_mat[, "Sa"]; Ia <- state_mat[, "Ia"]
  Sm <- state_mat[, "Sm"]; Em <- state_mat[, "Em"]; Im <- state_mat[, "Im"]
  Sp <- state_mat[, "Sp"]; Ep <- state_mat[, "Ep"]; Ip <- state_mat[, "Ip"]
  Ap <- state_mat[, "Ap"]; Rp <- state_mat[, "Rp"]; Xp <- state_mat[, "Xp"]
  
  with(as.list(parms), {
    
    # --- 1. Environmental & Seasonal Functions ---
    T_period <- 365
    T_shift  <- 30
    c_season <- 0.5 * (cos(2*pi*(t - T_shift)/T_period) + 1)
    
    # ---  control ---
    #LC
    if(t <= control_time) ua_c = 0
    if(t > control_time) ua_c = ua_c
    
    #AMC
    if(t <= control_time) um_c = 0
    if(t > control_time) um_c = um_c
    
    # BRR
    if(t <= control_time) R_bite = 0
    if(t > control_time) R_bite = R_bite
    
    # IPS
    if(t <= control_time) gamma = 1/7
    if(t <= control_time) gamma1 = 1/7
    if(t > control_time) gamma = R_gamma
    if(t > control_time) gamma1 = R_gamma1
    
    
    
    # --- 2. Migration Dynamics (Time Dependent) ---
    
    # A. External Migration (Inter-city: Foshan <-> Other Cities)
    # Get values from interpolated functions (approxfun) defined in main script
    BD_in_t  <- func_BD_in(t)  # Total In-flow index to Foshan
    BD_out_t <- func_BD_out(t) # Total Out-flow index from Foshan
    
    # Calculate External Rates per district 
    # r_in/out[i] depends on District Pop / Total Pop ratio
    ratio_pop <- Pop_dis / sum(Pop_dis) 
    
    D_in_dis  <- BD_in_t  * ratio_pop
    D_out_dis <- BD_out_t * ratio_pop
    
    r_in_dis  <- (D_in_dis  * rho_mig) / Pop_dis # Vector length 5
    r_out_dis <- (D_out_dis * rho_mig) / Pop_dis # Vector length 5
    
    # B. Internal Migration (Intra-city: Between 5 Districts)
    # Gravity Model Intensity 
    # r_intra : human mobility between districts
    r_intra <- phi * ((BD_in_t + BD_out_t) * rho_mig) / (2 * sum(Pop_dis))
    
    # Gravity Matrix M (5x5) is pre-calculated in parameters
    # Flow Calculation: Net flow for each compartment 
    # Flow_In[i] = Sum_{j≠i}(r_intra * M_ji * X_j * Pj)
    # Flow_Out[i] = Sum_{j≠i}(r_intra * M_ij * X_i * Pi)
    
    calc_internal_flow <- function(State_Vec) {
      Flow_In  <- numeric(num_districts)
      Flow_Out <- numeric(num_districts)
      
      for (i in 1:num_districts) {
        # Inflow to i from all j≠i
        for (j in 1:num_districts) {
          if (j != i) {
            Flow_In[i] <- Flow_In[i] + r_intra * M_matrix[j, i] * State_Vec[j] * Pop_dis[j]
          }
        }
        # Outflow from i to all j≠i
        for (j in 1:num_districts) {
          if (j != i) {
            Flow_Out[i] <- Flow_Out[i] + r_intra * M_matrix[i, j] * State_Vec[i] * Pop_dis[i]
          }
        }
      }
      return((Flow_In - Flow_Out) / Pop_dis) 
    }
    
    # Calculate net flows for humans 
    Flow_Sp <- calc_internal_flow(Sp / Pop_dis)
    Flow_Ep <- calc_internal_flow(Ep / Pop_dis)
    Flow_Ip <- calc_internal_flow(Ip / Pop_dis)
    Flow_Ap <- calc_internal_flow(Ap / Pop_dis)
    Flow_Rp <- calc_internal_flow(Rp / Pop_dis)
    
    # --- 3. Population Sizes ---
    Np <- Sp + Ep + Ip + Ap + Rp      # Human Population (Vector)
    
    # K
    k_vec <- numeric(num_districts)
    for(i in 1:num_districts) {
      k_name <- paste0("k_", Districts[i])
      if(k_name %in% names(parms)) {
        k_vec[i] <- parms[[k_name]]
      } else {
        k_vec[i] <- k  
      }
    }
    
    # Total adult mosquitoes 
    # Nm_total <- k_vec * Np * e_p      
    Nm_total <- Sm + Em + Im
    # --- 4. Force of Infection  ---

    beta_mp0_vec <- numeric(num_districts)
    beta_pm0_vec <- numeric(num_districts)
    
    for(i in 1:num_districts) {
      # beta_mp0 in different districts
      beta_mp_name <- paste0("beta_mp0_", Districts[i])
      if(beta_mp_name %in% names(parms)) {
        beta_mp0_vec[i] <- parms[[beta_mp_name]]
      } else if("beta_mp0" %in% names(parms)) {
        beta_mp0_vec[i] <- beta_mp0  
      } else {
        beta_mp0_vec[i] <- 0.15  
      }
      
      # beta_pm0 in different districts
      beta_pm_name <- paste0("beta_pm0_", Districts[i])
      if(beta_pm_name %in% names(parms)) {
        beta_pm0_vec[i] <- parms[[beta_pm_name]]
      } else if("beta_pm0" %in% names(parms)) {
        beta_pm0_vec[i] <- beta_pm0 
      } else {
        beta_pm0_vec[i] <- 0.30  
      }
    }
    
    # Force of infection for humans: β_mp0 * b * Im / Np
    FOI_to_Human <- beta_mp0_vec * b *(1-R_bite) * Im / Np
    
    # Force of infection for mosquitoes : β_pm0 * b * (Ip + η*Ap) / Np
    FOI_to_Mosq <- beta_pm0_vec * b *(1-R_bite) * (Ip + eta * Ap) / Np
    
    # --- 5. ODE Equations ---
    
    # Aquatic stage
    dSa <- a * c_season * (Nm_total - n * Im) - lambda * e_p * Sa - (1 - e_p) * mu_a * Sa - ua_c*Sa
    dIa <- a * c_season * n * Im - lambda * e_p * Ia - (1 - e_p) * mu_a * Ia - ua_c*Ia
    
    # Adult stage
    dSm <- lambda * e_p * Sa - FOI_to_Mosq * Sm - mu_m * Sm - um_c*Sm
    dEm <- FOI_to_Mosq * Sm - omega_m * Em - mu_m * Em - um_c*Em
    dIm <- lambda * e_p * Ia + omega_m * Em - mu_m * Im - um_c*Im
    
    # Humans (With Migration)
    
    dSp <- -FOI_to_Human * Sp - theta_d * Sp + 
      r_in_dis * p_e_S * Np - r_out_dis * Sp + Flow_Sp*Sp
    
    dEp <- FOI_to_Human * Sp - omega_p * Ep - theta_d * Ep + 
      r_in_dis * p_e_E * Np - r_out_dis * Ep + Flow_Ep*Ep
    
    dIp <- (1 - q) * omega_p * Ep - gamma * Ip - theta_d * Ip + 
      r_in_dis * p_e_I * Np - r_out_dis * Ip + Flow_Ip*Ip
    
    dAp <- q * omega_p * Ep - gamma1 * Ap - theta_d * Ap + 
      r_in_dis * p_e_A * Np - r_out_dis * Ap + Flow_Ap*Ap
    
    dRp <- gamma * Ip + gamma1 * Ap - theta_d * Rp + 
      r_in_dis * p_e_R * Np - r_out_dis * Rp + Flow_Rp*Rp
    
    # Cumulative Cases 
    dXp <- (1 - q) * omega_p * Ep
    
    res <- c(dSa, dIa, dSm, dEm, dIm, dSp, dEp, dIp, dAp, dRp, dXp)
    
    return(list(res))
  })
}


