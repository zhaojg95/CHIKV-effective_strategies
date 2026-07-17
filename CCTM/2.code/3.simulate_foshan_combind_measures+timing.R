rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes")
pacman::p_load(pkgs,character.only = T)

#lood model
source("2.code/CHIKV-model.R")

#load initial values and fixed parameters
source("2.code/init_value & fixed parameters.R")

#simulate-max day
max_day <- 180
#read posterior_samples
set.seed(2025)
posterior_samples <- readRDS("3.result/posterior_samples_foshan.rds") %>% 
  .[sample(nrow(.), 100), ]
times = 1:max_day 

# 1. Simulation baseline scenario -----------------------------

sim_cases <- apply(posterior_samples, 1, function(x) {
  p <- c(parms.fixed, as.list(x))
  k <- p["k"]
  Np <- 9698900
  Na <- Np*as.numeric(k)
  Nm <- Na*0.859  #e_p
  ps <- c(p) 
  
  init_value <- c(Sa = Na, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
  out   <- ode(y   = init_value,
               times = times,
               func  = CHIKV_model,
               parms = ps)
  simulate_cases <- diff(out[, "X"]) %>%
    c(out[1, "X"], .)                                  
}) %>% 
  as.data.frame()

names(sim_cases) <- paste0("X_",seq(1,100,1))

sum_sim_cases <- apply(sim_cases,MARGIN = 2, sum)

df <- data.frame(
  day   = times,
  intervention = NA,
  low   = apply(sim_cases, 1, quantile, probs = 0.025),
  high  = apply(sim_cases, 1, quantile, probs = 0.975),
  mid   = apply(sim_cases, 1, median) ,
  total_low = as.numeric(quantile(sum_sim_cases,probs = 0.025)),
  total_high  = as.numeric(quantile(sum_sim_cases,probs = 0.975)),
  total_mid   = as.numeric(quantile(sum_sim_cases,probs = 0.5)) ,
  type  = "baseline scenario",
  sim_cases
)

result_baseline <- df

saveRDS(result_baseline, file = "3.result/result_baseline.rds")


# 2.Simulation with interventions ---------------------------------
ua_c <- seq(0,0.9,0.1)
um_c <- seq(0,0.9,0.1)
R_bite <- seq(0,0.9,0.1)
transmission_time <- 1:7  #infectious period

combind_intervention_measures <- expand.grid(ua_c,um_c,R_bite,transmission_time) %>% 
  as.data.frame()

names(combind_intervention_measures) <- c("ua_c","um_c","R_bite","transmission_time")

#intervention timing
timing <- c(3,5,7,9,11,13,14,15,17,19,21,23,25,27,28,29,31)

for (cont in 1:length(timing)) {
  control_time <- timing[cont]
  combind_intervention_measures_control_time <- combind_intervention_measures %>% 
    mutate(control_time = control_time)
  
  t_total <- 0 
  result_combind_measures_control_time <- c()
  for (i in 1:nrow(combind_intervention_measures_control_time)) {
    t_start <- Sys.time()
    ua_c <- combind_intervention_measures_control_time[i,"ua_c"]
    um_c <- combind_intervention_measures_control_time[i,"um_c"]
    R_bite <- combind_intervention_measures_control_time[i,"R_bite"]
    transmission_time <- combind_intervention_measures_control_time[i,"transmission_time"]
    R_gamma1  <-  R_gamma <- 1/transmission_time
    control_time <- combind_intervention_measures_control_time[i,"control_time"]
    
    sim_cases <- apply(posterior_samples, 1, function(x) {
      p <- c(parms.fixed, as.list(x))
      k <- p["k"]
      Np <- 9698900
      Na <- Np*as.numeric(k)
      Nm <- Na*0.859  #e_p
      ps <- c(p) 
      
      init_value <- c(Sa = Na, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1,Maqua=0 , dMadult=0)
      out   <- ode(y   = init_value,
                   times = times,
                   func  = CHIKV_model_with_combind_intervention, #model change
                   parms = ps)
      simulate_cases <- diff(out[, "X"]) %>%
        c(out[1, "X"], .)
    })  %>% 
      as.data.frame()
    
    names(sim_cases) <- paste0("X_",seq(1,100,1))
    
    sum_sim_cases <- apply(sim_cases,MARGIN = 2, sum)
    
    df <- data.frame(
      day   = times,
      ua_c = combind_intervention_measures_control_time[i,"ua_c"],
      um_c = combind_intervention_measures_control_time[i,"um_c"],
      R_bite = combind_intervention_measures_control_time[i,"R_bite"],
      transmission_time = combind_intervention_measures_control_time[i,"transmission_time"],
      control_time = combind_intervention_measures_control_time[i,"control_time"],
      low   = apply(sim_cases, 1, quantile, probs = 0.025),
      high  = apply(sim_cases, 1, quantile, probs = 0.975),
      mid   = apply(sim_cases, 1, median),
      total_low = as.numeric(quantile(sum_sim_cases,probs = 0.025)),
      total_high  = as.numeric(quantile(sum_sim_cases,probs = 0.975)),
      total_mid   = as.numeric(quantile(sum_sim_cases,probs = 0.5)),
      sim_cases
    )
    
    result_combind_measures_control_time <- rbind(result_combind_measures_control_time,df)
    
    t_end <- Sys.time()
    t_total <- as.numeric(t_total + t_end - t_start, units = "secs")
    cat(sprintf("timing = %d : Completed the %d th run, taking %.2f s.\n", control_time, i, t_total))
  }
  
  saveRDS(result_combind_measures_control_time, file = paste0("3.result/result_combind_measures_control_time(",control_time,").rds"))  
  
}







