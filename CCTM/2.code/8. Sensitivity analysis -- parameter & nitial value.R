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
  .[sample(nrow(.), 1), ]
times = 1:max_day 



# Parameter sensitivity function ------------------------------------------

psf <- function(Name , From_value = NULL, To_value = NULL, length.out = 1000){
  daily_sim <- c()
  for (value in seq(from = From_value, to = To_value,length.out = length.out)) {
    parms.fixed[Name] = value
    
    beta_mp0 <- as.numeric(posterior_samples["beta_mp0"])
    beta_pm0 <- as.numeric(posterior_samples["beta_pm0"])
    k <- as.numeric(posterior_samples["k"])
    Np <- 9698900
    Na <- Np*as.numeric(k)
    Nm <- as.numeric(Na*parms.fixed["e_p"])  #e_p
    ps <- c(parms.fixed ,Nm = Nm,beta_mp0 = beta_mp0, beta_pm0=beta_pm0, k =k ) 
    
    init_value <- c(Sa = Na, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
    out   <- ode(y   = init_value,
                 times = times,
                 func  = CHIKV_model,
                 parms = ps)
    simulate_cases <- diff(out[, "X"]) %>%
      c(out[1, "X"], .)   
    
    daily_sim <- cbind(daily_sim,simulate_cases)
    
  }
  
  daily_sim_ci <- data.frame(
    parameter = Name,
    day = 1:max_day ,
    low   = apply(daily_sim, 1, quantile, probs = 0.025) ,
    mid = apply(daily_sim, 1, median),
    high  = apply(daily_sim, 1, quantile, probs = 0.975)
  )
  
  return(daily_sim_ci)
}

start_date <- as.Date("2025-6-16",origin = "1899-12-30")
day = max_day
psf_plot <- function(data,Name){
  p <- ggplot(data, aes(x = start_date + day-1)) +
    geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.5, fill = "#91D1C2FF") +
    scale_color_flatui()+
    geom_line(aes(y = mid/1000),  linewidth = 0.8) +
    labs(y = "Daily cases",
         x = " ") +
    theme_bw()+
    theme(
      axis.text.x = element_text(size = 8, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
      axis.text.y = element_text(size = 8, colour = "black",family = "sans"),
      plot.title = element_text(size = 10, face = "bold",hjust = 0.5),  
      axis.title.x = element_text(size = 8),  
      axis.title.y = element_text(size = 8),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 8,family = "sans"),  
      legend.position = "", 
      legend.background = element_blank(),   
      legend.key        = element_blank(),   
      strip.text = element_text(size = 8, face = "bold"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank()  
    )+labs(x = "", y = "Daily cases",title = parse(text = Name))+
    scale_x_date(date_labels = "%b %d\n2025", date_breaks = "2 month")+
    scale_y_continuous(labels = function(x) paste0(x, "k"))
  return(p)
}


length.out = 1000
# “n”  -- infection rate for vertical transmission ----------------------
Name = "n"
n_sensitivity <- psf(Name,From_value = 0.0076,To_value = 0.0286,length.out )
n <- psf_plot(n_sensitivity,Name = "n");n


# "a" -- The natural birth rate of mosquitoes --------------------------
Name = "a"
a_sensitivity <- psf(Name,From_value = 0.02,To_value = 0.27,length.out)
a <- psf_plot(a_sensitivity,Name = "a");a


# "mu_a" -- Mortality rate of aquatic stage ----------------------------
Name = "mu_a"
mu_a_sensitivity <- psf(Name,From_value = 0.0177,To_value = 0.0447,length.out)
mu_a <- psf_plot(mu_a_sensitivity,Name = "mu[a]");mu_a


# e_p -- Proportion of emergence  -----------------------------------------
Name = "e_p"
e_p_sensitivity <- psf(Name,From_value = 0.768,To_value = 0.95,length.out)
e_p <- psf_plot(e_p_sensitivity,Name = "e[p]");e_p


# b --biting rate ------------------------------------------------------
Name = "b"
b_sensitivity <- psf(Name,From_value = 0.33,To_value = 1,length.out)
b <- psf_plot(b_sensitivity,Name = "b");b


# lambda -- 1/the time of emergence ---------------------------------------
Name = "lambda"
lambda_sensitivity <- psf(Name,From_value = 1/14.463,To_value = 1/7.714,length.out)
lambda <- psf_plot(lambda_sensitivity,Name = "lambda");lambda


# mu_m -- 1/the survival time of adult mosquitoes -------------------------
Name = "mu_m"
mu_m_sensitivity <- psf(Name,From_value = 1/9.8,To_value = 1/4.5,length.out)
mu_m <- psf_plot(lambda_sensitivity,Name = "mu[m]");mu_m 


# omega_m -- 1/Extrinsic incubation period(EIP) ---------------------------
Name = "omega_m"
omega_m_sensitivity <- psf(Name,From_value = 1/8.2,To_value = 1/3,length.out)
omega_m <- psf_plot(omega_m_sensitivity,Name = "omega[m]");omega_m 



# omega_p -- Intrinsic incubation period ----------------------------------
Name = "omega_p"
omega_p_sensitivity <- psf(Name,From_value = 1/7,To_value = 1/2,length.out)
omega_p <- psf_plot(omega_p_sensitivity,Name = "omega[p]");omega_p 


# q -- Proportion of asymptomatic cases -----------------------------------
Name = "q"
q_sensitivity <- psf(Name,From_value = 0.03,To_value = 0.28,length.out)
q <- psf_plot(q_sensitivity,Name = "q");q


# eta -- Transmissibility coefficient of asymptomatic infections r --------
Name = "eta"
eta_sensitivity <- psf(Name,From_value = 0.8,To_value = 1.2,length.out)
eta <- psf_plot(eta_sensitivity,Name = "eta");eta


#bind plot###

library(patchwork)
sens_combined <- (n + a + mu_a + e_p + b + lambda + mu_m + omega_m + omega_p +q + eta) + 
  plot_layout(ncol = 3,widths = c(3, 3))
sens_combined

ggsave(filename = "3.result/plot fig/sens_combined.png",sens_combined,width = 25,height = 20,dpi = 400,units = "cm")
# save.image("3.result/parameter_sensitivity.RData")


# Initial value sensitivity function ------------------------------------------

ISF <- function(Ini_Name , From_value = NULL, To_value = NULL, length.out = 1000){
  daily_sim <- c()
  for (value in seq(from = From_value, to = To_value,length.out = length.out)) {
    
    beta_mp0 <- as.numeric(posterior_samples["beta_mp0"])
    beta_pm0 <- as.numeric(posterior_samples["beta_pm0"])
    k <- as.numeric(posterior_samples["k"])
    
    
    Np <- 9698900
    Na <- Np*as.numeric(k)
    Nm <- as.numeric(Na*parms.fixed["e_p"])  #e_p
    
    ps <- c(parms.fixed ,Nm = Nm,beta_mp0 = beta_mp0, beta_pm0=beta_pm0, k =k ) 
    
    init_value <- c(Sa = Na, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
    
    init_value[Ini_Name] = value
    
    out   <- ode(y   = init_value,
                 times = times,
                 func  = CHIKV_model,
                 parms = ps)
    simulate_cases <- diff(out[, "X"]) %>%
      c(out[1, "X"], .)   
    
    daily_sim <- cbind(daily_sim,simulate_cases)
    
  }
  
  daily_sim_ci <- data.frame(
    parameter = Ini_Name,
    day = 1:max_day ,
    low   = apply(daily_sim, 1, quantile, probs = 0.025) ,
    mid = apply(daily_sim, 1, median),
    high  = apply(daily_sim, 1, quantile, probs = 0.975)
  )
  
  return(daily_sim_ci)
}

start_date <- as.Date("2025-6-16",origin = "1899-12-30")
day = max_day
isf_plot <- function(data,title){
  p <- ggplot(data, aes(x = start_date + day-1)) +
    geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.6, fill = "#E64B35FF") +
    scale_color_flatui()+
    geom_line(aes(y = mid/1000),  linewidth = 0.8) +
    labs(y = "Daily cases",
         x = " ") +
    theme_bw()+
    theme(
      axis.text.x = element_text(size = 8, colour = "black", vjust = 0.5, hjust = 0.5, angle = 0),
      axis.text.y = element_text(size = 8, colour = "black"),
      plot.title = element_text(size = 10, face = "bold",hjust = 0.5),  
      axis.title.x = element_text(size = 8),  
      axis.title.y = element_text(size = 8),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 8),  
      legend.position = "", 
      legend.background = element_blank(),   
      legend.key        = element_blank(),   
      strip.text = element_text(size = 8, face = "bold"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank()  
    )+labs(x = "", y = "Daily cases",title = title)+
    scale_x_date(date_labels = "%b %d\n2025", date_breaks = "2 month")+
    scale_y_continuous(labels = function(x) paste0(x, "k"))
  return(p)
}



beta_mp0 <- as.numeric(posterior_samples["beta_mp0"])
beta_pm0 <- as.numeric(posterior_samples["beta_pm0"])
k <- as.numeric(posterior_samples["k"])
Np <- 9698900
Na <- Np*as.numeric(k)
Nm <- as.numeric(Na*parms.fixed["e_p"])


length.out = 1000
# Sa  ------------------------------------------------

#  Sa  +- 20%
Ini_Name = "Sa"
Sa_sensitivity <- ISF(Ini_Name,From_value = Na*(1-0.2),To_value = Na*(1+0.2))

title0 = bquote(italic(S)[a])
Sa <- isf_plot(Sa_sensitivity,title = title0);Sa


# Ia  ------------------------------------------------

#  Ia  +- From 
Ini_Name = "Ia"
Ia_sensitivity <- ISF(Ini_Name,From_value = 0,To_value = 5)

title0 = bquote(italic(I)[a])
Ia <- isf_plot(Ia_sensitivity,title = title0);Ia


# Sm  ------------------------------------------------

#  Sm  +- From 
Ini_Name = "Sm"
Sm_sensitivity <- ISF(Ini_Name,From_value = Nm*(1-0.2),To_value = Nm*(1+0.2))

title0 = bquote(italic(S)[m])
Sm <- isf_plot(Sm_sensitivity,title = title0);Sm 

# Em  ------------------------------------------------

#  Em  +- From 

Ini_Name = "Em"
Em_sensitivity <- ISF(Ini_Name,From_value = 0,To_value = 5)

title0 = bquote(italic(E)[m])
Em <- isf_plot(Em_sensitivity,title = title0);Em


# Im  ------------------------------------------------
#  Im  +- From 

Ini_Name = "Im"
Im_sensitivity <- ISF(Ini_Name,From_value = 0,To_value = 5)

title0 = bquote(italic(I)[m])
Im <- isf_plot(Im_sensitivity,title = title0);Im

# Sp  ------------------------------------------------

#  Sp  +- From 

Ini_Name = "Sp"
Sp_sensitivity <- ISF(Ini_Name,From_value = Np*(1-0.2),To_value = Np*(1+0.2))

title0 = bquote(italic(S)[p])
Sp <- isf_plot(Sp_sensitivity,title = title0);Sp 


# Ep  ------------------------------------------------

#  Ep  +- From 

Ini_Name = "Ep"
Ep_sensitivity <- ISF(Ini_Name,From_value = 0,To_value = 5)

title0 = bquote(italic(E)[p])
Ep <- isf_plot(Ep_sensitivity,title = title0);Ep


# Ip  ------------------------------------------------

#  Ip  +- From 
Ini_Name = "Ip"
Ip_sensitivity <- ISF(Ini_Name,From_value = 0,To_value = 5)

title0 = bquote(italic(I)[p])
Ip <- isf_plot(Ip_sensitivity,title = title0);Ip

# Ap  ------------------------------------------------

#  Ap  +- From 
Ini_Name = "Ap"
Ap_sensitivity <- ISF(Ini_Name,From_value = 0,To_value = 5)

title0 = bquote(italic(A)[p])
Ap <- isf_plot(Ap_sensitivity,title = title0);Ap

# Rp  ------------------------------------------------

#  Rp  +- From 
Ini_Name = "Rp"
Rp_sensitivity <- ISF(Ini_Name,From_value = 0,To_value = 5)

title0 = bquote(italic(R)[p])
Rp <- isf_plot(Rp_sensitivity,title = title0);Rp


sens_combined_initial <- Sa+Ia+Sm+Em+Im+Sp+Ep+Ip+Ap+Rp;sens_combined_initial

ggsave(filename = "3.result/plot fig/sens_combined_initial.png",sens_combined_initial,width = 25,height = 16,dpi = 400,units = "cm")
save.image("3.result/initial&parameter_sensitivity.RData")










