options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","ggsci","patchwork")
pacman::p_load(pkgs,character.only = T)


# 1. loading data ---------------------------------------------------------

cases <- Foshan_cases <- read.xlsx("1.data/Foshan_cases.xlsx", detectDates = T)

#lood model
source("2.code/CHIKV-model.R")

#load initial values and fixed parameters
source("2.code/init_value & fixed parameters.R")

# fitted parameters
set.seed(2025)
parms.fit <- c(beta_mp0 = 0.36,
               beta_pm0 = 0.72,
               k = 10)

# 4.fitting model (stage 1) -------------------------------------------------
times <- 1:31

model_cost <- function(p) {
  p <- c(parms.fixed, p)
  k <- p["k"]
  Np <- 9698900
  Na <- Np*as.numeric(k)
  Nm <- Na*0.859 
  ps <- c(p ) 
  init_value <- c(Sa = Na, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
  out   <- data.frame(ode(y = init_value, times = times, func = CHIKV_model, ps))
  fitInfected <- diff(out[, "X"]) %>% 
    c(out[1, "X"], .)
  rmse <- RMSE(fitInfected[times],cases[times,"Cases"])
  return(rmse)
}

fit_result <- modFit(f = model_cost,
                     p = parms.fit,
                     method = "L-BFGS-B",
                     lower = c(beta_mp0 = 0.000001,beta_pm0 = 0.000001, k = 10),
                     upper = c(beta_mp0 = 1,beta_pm0 = 1, k = 15)
)

##MCMC##
set.seed(3)

mcmc_result1 <- modMCMC(f = model_cost,
                        p = fit_result$par,   # using the result of modFit as the initial value
                        lower = c(beta_mp0 = 10e-6,beta_pm0 = 10e-6, k = 10),
                        upper = c(beta_mp0 = 1,beta_pm0 = 1, k = 15),
                        niter = 10000,
                        burninlength = 2000,
                        jump = 0.2,updatecov =100, ntrydr = 2)


plot(mcmc_result1)

hist(mcmc_result1$pars[,"k"],breaks = 100)
mcmc_chain1 <- as.mcmc(mcmc_result1$pars)

#Geweke diagnostic
geweke_res1 <- geweke.diag(
  mcmc_chain1,
  frac1 = 0.1,   # beginning of chain: 10%
  frac2 = 0.5    #  end of chain: 50%
);geweke_res1
geweke.plot(mcmc_chain1)
ess1 <- effectiveSize(mcmc_chain1)

plot(density(mcmc_result1$pars[,"k"]))
plot(density(mcmc_result1$pars[,"beta_mp0"]))
plot(density(mcmc_result1$pars[,"beta_pm0"]))

set.seed(5)
mcmc_result2 <- modMCMC(f = model_cost,
                        p = fit_result$par,   # using the result of modFit as the initial value
                        lower = c(beta_mp0 = 0.000001,beta_pm0 = 0.000001, k = 10),
                        upper = c(beta_mp0 = 1,beta_pm0 = 1, k = 15),
                        niter = 10000,
                        burninlength = 2000,
                        jump = 0.2,updatecov = 100, ntrydr = 2)
plot(mcmc_result2)

mcmc_chain2 <- as.mcmc(mcmc_result2$pars)

#Geweke diagnostic
geweke_res2 <- geweke.diag(
  mcmc_chain2,
  frac1 = 0.1,   # 10%
  frac2 = 0.5    #  50%
);geweke_res2
geweke.plot(mcmc_chain2)
ess2 <- effectiveSize(mcmc_chain2)

set.seed(7)
mcmc_result3 <-modMCMC(f = model_cost,
                       p = fit_result$par,   # using the result of modFit as the initial value
                       lower = c(beta_mp0 = 0.000001,beta_pm0 = 0.000001, k = 10),
                       upper = c(beta_mp0 = 1,beta_pm0 = 1, k = 15),
                       niter = 10000,
                       burninlength = 2000,
                       jump = 0.2,updatecov = 100, ntrydr = 2)
plot(mcmc_result3)

mcmc_chain3 <- as.mcmc(mcmc_result3$pars)

#Geweke diagnostic
geweke_res3 <- geweke.diag(
  mcmc_chain3,
  frac1 = 0.1,   #  10%
  frac2 = 0.5    # 50%
);geweke_res3
geweke.plot(mcmc_chain3)
ess3 <- effectiveSize(mcmc_chain3)


set.seed(9)
mcmc_result4 <- modMCMC(f = model_cost,
                        p = fit_result$par,   # using the result of modFit as the initial value
                        lower = c(beta_mp0 = 0.000001,beta_pm0 = 0.000001, k = 10),
                        upper = c(beta_mp0 = 1,beta_pm0 = 1, k = 15),
                        niter = 10000,
                        burninlength = 2000,
                        jump = 0.2,updatecov = 200, ntrydr = 2)
plot(mcmc_result4)

mcmc_chain4 <- as.mcmc(mcmc_result4$pars)

#Geweke diagnostic
geweke_res4 <- geweke.diag(
  mcmc_chain4,
  frac1 = 0.1,   #  10%
  frac2 = 0.5    #  50%
);geweke_res4
geweke.plot(mcmc_chain4)
ess4 <- effectiveSize(mcmc_chain4)


plot((mcmc_result1$pars[, "beta_mp0"]), type="l",ylim = c(0.2,1.1))
lines((mcmc_result2$pars[, "beta_mp0"]), col="red")
lines((mcmc_result3$pars[, "beta_mp0"]), col="blue")
lines((mcmc_result4$pars[, "beta_mp0"]), col="grey")

chain_beta_mp0 <- data.frame(i=rep(seq(1:8000),4),
                             value = c(mcmc_result1$pars[, "beta_mp0"],
                                                mcmc_result2$pars[, "beta_mp0"],
                                                mcmc_result3$pars[, "beta_mp0"],
                                                mcmc_result4$pars[, "beta_mp0"]),
                             Chain = c(rep("chain_1",length(mcmc_result1$pars[, "beta_mp0"])),
                                       rep("chain_2",length(mcmc_result2$pars[, "beta_mp0"])),
                                       rep("chain_3",length(mcmc_result3$pars[, "beta_mp0"])),
                                       rep("chain_4",length(mcmc_result4$pars[, "beta_mp0"]))))

chain_beta_pm0 <- data.frame(i=rep(seq(1:8000),4),
                             value = c(mcmc_result1$pars[, "beta_pm0"],
                                       mcmc_result2$pars[, "beta_pm0"],
                                       mcmc_result3$pars[, "beta_pm0"],
                                       mcmc_result4$pars[, "beta_pm0"]),
                             Chain = c(rep("chain_1",length(mcmc_result1$pars[, "beta_pm0"])),
                                       rep("chain_2",length(mcmc_result2$pars[, "beta_pm0"])),
                                       rep("chain_3",length(mcmc_result3$pars[, "beta_pm0"])),
                                       rep("chain_4",length(mcmc_result4$pars[, "beta_pm0"]))))

chain_k <- data.frame(i=rep(seq(1:8000),4),
                      value = c(mcmc_result1$pars[, "k"],
                                mcmc_result2$pars[, "k"],
                                mcmc_result3$pars[, "k"],
                                mcmc_result4$pars[, "k"]),
                      Chain = c(rep("chain_1",length(mcmc_result1$pars[, "k"])),
                                rep("chain_2",length(mcmc_result2$pars[, "k"])),
                                rep("chain_3",length(mcmc_result3$pars[, "k"])),
                                rep("chain_4",length(mcmc_result4$pars[, "k"]))))

p_trace_func <- function(data,title,legend.position,x){
  colors <- pal_npg()(10)[c(1,2,4,3)]
  p <- ggplot(data,aes(x =i,y =value,col = Chain))+
    geom_line(size = 0.2)+  
    theme_bw() +
    scale_color_manual(values =colors)+
    theme(
      strip.text = element_text(size = 6, face = "bold", colour = "black"), 
      axis.ticks.x = element_line(colour = "black", linewidth = 0.2),
      axis.ticks.y = element_line(colour = "black", linewidth = 0.2),
      axis.text.x  = element_text(size = 6, colour = "black"),
      axis.text.y  = element_text(size = 6, colour = "black"),
      plot.title   = element_text(size = 6, face = "bold"),
      axis.title.x = element_text(size = 6),
      axis.title.y = element_text(size = 6),
      legend.title = element_text(size = 6),
      legend.text  = element_text(size = 5),
      legend.position = legend.position,
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.15, "cm"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )  +
    labs(title = title,x = x,y = "value")
  return(p)
}

p_trace_beta_mp0 <- p_trace_func(chain_beta_mp0,title = expression(beta[mp0]),legend.position = "none",x = "")
p_trace_beta_pm0 <- p_trace_func(chain_beta_pm0,title = expression(beta[pm0]),legend.position = "none",x = "")
p_trace_k <- p_trace_func(chain_k,title = expression(k),legend.position = "bottom",x = "index")
p_trace <- p_trace_beta_mp0 /p_trace_beta_pm0 /p_trace_k

ggsave(filename = "3.result/plot fig/p_trace.png",p_trace,width = 13,height = 12,dpi = 1200,units = "cm")


#psrf
mcmc_list_beta_mp0 <- mcmc.list(
  as.mcmc(as.matrix((mcmc_result1$pars[, "beta_mp0"]))),
  as.mcmc(as.matrix((mcmc_result2$pars[, "beta_mp0"]))),
  as.mcmc(as.matrix((mcmc_result3$pars[, "beta_mp0"])))
)
gelman.diag(mcmc_list_beta_mp0)

mcmc_list_beta_pm0 <- mcmc.list(
  as.mcmc(as.matrix((mcmc_result1$pars[, "beta_pm0"]))),
  as.mcmc(as.matrix((mcmc_result2$pars[, "beta_pm0"]))),
  as.mcmc(as.matrix((mcmc_result3$pars[, "beta_pm0"])))
)
gelman.diag(mcmc_list_beta_pm0)

mcmc_list_k <- mcmc.list(
  as.mcmc(as.matrix((mcmc_result1$pars[, "k"]))),
  as.mcmc(as.matrix((mcmc_result2$pars[, "k"]))),
  as.mcmc(as.matrix((mcmc_result3$pars[, "k"])))
)
gelman.diag(mcmc_list_k)


#calculating the median and 95%CI of fitted parameters
posterior_samples  <-  data.frame(rbind(mcmc_result1$pars,
                             mcmc_result2$pars,
                             mcmc_result3$pars,
                             mcmc_result4$pars))


plot(density(posterior_samples$k))
plot(density(posterior_samples$beta_mp0))
plot(density(posterior_samples$beta_pm0))

ci_95 <- apply(posterior_samples, 2, quantile, probs = c(0.025,0.5, 0.975));ci_95

#prior & posterior
density_plot_func <- function(data,title, x="",values1 = c("Prior (Uniform(0,1))" = "#E74C3C", 
                                                          "Posterior" = "#3498DB"),
                              values2 = c("Prior (Uniform(0,1))" = "#C0392B", 
                                          "Posterior" = "#2980B9")){
  ggplot(data, aes(x = value, fill = Distribution)) +
    geom_density(alpha = 0.4, color = NA) +
    geom_density(aes(color = Distribution), fill = NA, linewidth = 0.5) +
    scale_fill_manual(values = values1) +
    scale_color_manual(values = values2) +
    # coord_cartesian(xlim = c(0, 1)) +
    labs(
      title = title,
      x = x,
      y = "Density",
      fill = "Distribution",
      color = "Distribution"
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 6, face = "bold", colour = "black"), 
      axis.ticks.x = element_line(colour = "black", linewidth = 0.2),
      axis.ticks.y = element_line(colour = "black", linewidth = 0.2),
      axis.text.x  = element_text(size = 6, colour = "black"),
      axis.text.y  = element_text(size = 6, colour = "black"),
      plot.title   = element_text(size = 6, face = "bold"),
      axis.title.x = element_text(size = 6),
      axis.title.y = element_text(size = 6),
      legend.title = element_text(size = 6),
      legend.text  = element_text(size = 5),
      legend.position = c(0.15,0.8),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.15, "cm"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) 
}

#beta_mp0

set.seed(123)
prior_data <- data.frame(
  value = runif(8000*4, 0, 1),
  Distribution = "Prior (Uniform(0,1))"
)
posterior_sample_beta_mp0 <- data.frame(value = pmax(pmin(posterior_samples$beta_mp0, 1), 0),
                                        Distribution = "Posterior")

beta_mp0_dis <- rbind(prior_data,posterior_sample_beta_mp0)
beta_mp0_dis$Distribution <- factor(beta_mp0_dis$Distribution , 
                                 levels = c("Prior (Uniform(0,1))", "Posterior"))

beta_mp0_density <- density_plot_func(beta_mp0_dis,title = expression(beta[mp0]))

#beta_pm0
posterior_sample_beta_pm0 <- data.frame(value = pmax(pmin(posterior_samples$beta_pm0, 1), 0),
                                        Distribution = "Posterior")
beta_pm0_dis <- rbind(prior_data,posterior_sample_beta_pm0)
beta_pm0_dis$Distribution <- factor(beta_pm0_dis$Distribution , 
                                    levels = c("Prior (Uniform(0,1))", "Posterior"))

beta_pm0_density <- density_plot_func(beta_pm0_dis,title = expression(beta[pm0]))

#k
set.seed(123)
prior_data <- data.frame(
  value = runif(8000*4, 5, 15),
  Distribution = "Prior (Uniform(5,15))"
)
posterior_sample_k <- data.frame(value = posterior_samples$k,
                                 Distribution = "Posterior")

k_dis <- rbind(prior_data,posterior_sample_k)
k_dis$Distribution <- factor(k_dis$Distribution , 
                             levels = c("Prior (Uniform(5,15))", "Posterior"))

k_density <- density_plot_func(k_dis,title = expression(k),x="Parameter Value",values1 = c("Prior (Uniform(5,15))" = "#E74C3C","Posterior" = "#3498DB"),
                               values2 = c("Prior (Uniform(5,15))" = "#C0392B", 
                                           "Posterior" = "#2980B9"))


density_bind_plot <- beta_mp0_density/beta_pm0_density/k_density

ggsave(filename = "3.result/plot fig/density_bind_plot.png",density_bind_plot,width = 13,height =15,dpi = 1200,units = "cm")


write_rds(posterior_samples,"3.result/posterior_samples_foshan.rds")

set.seed(2025)
posterior_samples <- readRDS("3.result/posterior_samples_foshan.rds") %>% 
  .[sample(nrow(.), 100), ]
#simulating the trend of disease based on the fitted parameters 
sim_cases <- apply(posterior_samples, 1, function(x) {
  p <- c(parms.fixed, as.list(x))
  k <- p["k"]
  Np <- 9698900
  Na <- Np*as.numeric(k)
  Nm <- Na*0.859  #e_p
  ps <- c(p ,Nm = Nm) 
  init_value <- c(Sa = Na, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
  out   <- data.frame(ode(y = init_value, times = times, func = CHIKV_model, ps))
  simulate_cases <- diff(out[, "X"]) %>%
    c(out[1, "X"], .)                                  
})
# sim_cases <- readRDS("3.result/sim_cases_foshan.rds")
ci_low  <- apply(sim_cases, 1, quantile, probs = 0.025)
ci_high <- apply(sim_cases, 1, quantile, probs = 0.975)
median  <- apply(sim_cases, 1, median) 

plot_df <- data.frame(
  day   = cases$real.time,
  low   = ci_low,
  high  = ci_high,
  mid   = median,
  cases = cases$Cases[times]
)


# plot
p <- ggplot(plot_df, aes(x = day)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.5,fill= "skyblue") +
  geom_line(aes(y = mid, color = "Fitted"), linewidth = 1.2) +
  geom_point(aes(y = cases, color = "Reported"), size = 1.1) +
  labs(
    y = "Number of reported cases",
    x = "",
    title = " " ,
    fill = " ",  
    color = "" 
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 6, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
    axis.text.y = element_text(size = 6, colour = "black",family = "sans"),
    plot.title  = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 7),  
    axis.title.y = element_text(size = 7), 
    legend.title = element_blank(), 
    legend.text = element_text(size = 7,family = "sans"), 
    legend.position = c(0.2, 0.6), 
    legend.key.size = unit(0.8, "cm"),  
    legend.spacing.x = unit(0.8, 'cm'),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  ) +
  scale_color_manual(values = c("Fitted" = "steelblue", "Reported" = "red")) +
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "4 days");p
ggsave(filename = "3.result/plot fig/fitted_result.png",p,width = 15,height = 7,dpi = 400,units = "cm")
ggsave(filename = "3.result/plot fig/fitted_result.jpg",p,width = 15,height = 7,dpi = 400,units = "cm")
ggsave(filename = "3.result/plot fig/fitted_result.pdf",p,width = 15,height = 7,dpi = 400,units = "cm")

#evaluation metrics
eval_func <- function(y_true,y_pred){
  R2 = MLmetrics::R2_Score(y_pred,y_true)
  RMSE = MLmetrics::RMSE(y_pred,y_true)
  MAPE = MLmetrics::MAPE(y_pred,y_true)
  MAE = MLmetrics::MAE(y_pred,y_true)
  print(plot(y_true,y_pred),
        abline(a = 0, b = 1, lty = 2, col = "red"))
  return(cat("R2 = ",R2,"\n",
             "RMSE = ",RMSE,"\n"))
}

y_true <- cases$Cases[times]
y_pred <- plot_df$mid
eval_func(y_true,y_pred)

# saveRDS(mcmc_result, file = "3.result/mcmc_result_foshan.rds")
saveRDS(posterior_samples, file = "3.result/posterior_samples_foshan.rds")
saveRDS(sim_cases, file = "3.result/sim_cases_foshan.rds")


# trends plot new ---------------------------------------------------------

Foshan_cases2 <- read.xlsx("1.data/Foshan_cases.xlsx", sheet = 2,detectDates = T)
names(Foshan_cases2)[3] <-  "Date"
dates <- seq(from = as.Date("2025-06-16"), to = as.Date("2025-08-16"), by = "8 days")
p1 <- ggplot(Foshan_cases2, aes(x = Date)) +
  geom_bar(aes(y = Cases), stat = "identity", fill = "red", color = "black",alpha = 0.5) +
  scale_fill_manual(values = c("red")) +  
  labs(
    y = "Number of reported cases",
    x = " ",
    title = " "
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white"),  
    panel.background = element_rect(fill = "white"), 
    axis.text.x = element_text(size = 6, colour = "black", family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),  # 调整X轴标签角度
    axis.text.y = element_text(size = 6, colour = "black", family = "sans"),
    plot.title  = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 10), 
    axis.title.y = element_text(size = 7), 
    legend.title = element_blank(),  
    legend.text = element_text(size = 7, family = "sans"),  
    legend.position = "none", 
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()  
  )+
  scale_y_continuous(limits = c(0,1000))+
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "4 days",limits = c(as.Date("2025-06-15"),as.Date("2025-08-18")));p1

ggsave(filename = "3.result/plot fig/trend_result.png",p1,width = 15,height = 8,dpi = 400,units = "cm")
ggsave(filename = "3.result/plot fig/trend_result.jpg",p1,width = 15,height = 8,dpi = 400,units = "cm")
ggsave(filename = "3.result/plot fig/trend_result.pdf",p1,width = 15,height = 8,dpi = 400,units = "cm")

save.image("3.result/fitted_beta0_11.Rdata")

