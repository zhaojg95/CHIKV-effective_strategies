options(scipen=200)
Sys.setlocale("LC_TIME","en_US.UTF-8")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","ggsci")
pacman::p_load(pkgs,character.only = T)

# 0. load data/model ----------------------------------------------------
source("2.code/Metapopulation_model.R")
source("2.code/init_meta_parameters.R")   

cases_obs <- read.xlsx("1.data/Foshan_cases_byDistrict.xlsx", detectDates = T) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>% 
  subset(date <= as.Date("2025/7/16") )

n_day   <- nrow(cases_obs)
times   <- 1:n_day

# 1. Baidu migration index ----------------------------------------------
baidu_mig <- read.xlsx("1.data/Foshan-baidu migration index.xlsx", detectDates = T) 
func_BD_in  <- approxfun(x = 1:n_day,
                         y = baidu_mig$value_move_in[1:n_day], rule = 2)
func_BD_out <- approxfun(x = 1:n_day,
                         y = baidu_mig$value_move_out[1:n_day], rule = 2)

# 2. cost function --------------------------------------------------------------
model_cost <- function(p){
  
  parms.run <- c(parms.fixed,
                 setNames(p, fit.par.names))  
  
  k_vec <- numeric(5)
  for(i in 1:5) {
    k_name <- paste0("k_", Districts[i])
    k_vec[i] <- parms.run[[k_name]]
  }
  
  init_Sa <- Pop_dis * k_vec
  init_Ia <- rep(0, 5)
  init_Sm <- init_Sa * 0.859
  init_Em <- rep(0, 5)
  init_Im <- rep(0, 5)
  
  init_Sp <- Pop_dis - c(1, 0, 0, 0, 0)   
  init_Ep <- rep(0, 5)
  init_Ip <- c(1, 0, 0, 0, 0)             
  init_Ap <- rep(0, 5)
  init_Rp <- rep(0, 5)
  init_Xp <- c(1, 0, 0, 0, 0)
  
  init <- c(init_Sa, init_Ia, init_Sm, init_Em, init_Im,
            init_Sp, init_Ep, init_Ip, init_Ap, init_Rp, init_Xp)
  
  out <- ode(y = init, times = times,
             func = Foshan_Meta_Model, parms = parms.run)
  
  #  cumulative cases : Columns 52 to 56
  xp_start <- 52
  xp_end <- 56
  cumulative <- out[, xp_start:xp_end]
  
  # daily cases
  sim <- matrix(0, nrow = n_day, ncol = 5)
  sim[1, ] <- cumulative[1, ]
  for(i in 2:n_day) {
    sim[i, ] <- cumulative[i, ] - cumulative[i-1, ]
  }
  
  rmse <- RMSE(c(sim), c(as.matrix(cases_obs[, -1])))
  return(rmse)
}

# 3. fitting ------------------------------------------------------------------
set.seed(2025)

#initial value (beta_mp0, beta_pm0, k)
p0 <- c(0.70,0.70,10,   #shunde
        0.42,0.42,10,   # chancheng
        0.31,0.32,10,   #nanhai
        0.31,0.32,10,   #gaoming
        0.31,0.32,10)   #sanshui

cat("Start fitting...\n")
cat("No. of parameters:", length(fit.par.names), "\n")
cat("name of parameters:\n")
print(fit.par.names)
fit.res_start <- Sys.time();fit.res_start
fit.res <- modFit(f = model_cost,
                  p = p0,
                  method = "L-BFGS-B",
                  lower = rep(c(1e-6, 1e-6, 10), 5),    #lower
                  upper = rep(c(1, 1, 15), 5))          # upper

fit.res 
cat("\n fitting finish!\n")
cat("Best parameters:\n")
print(fit.res$par)
cat("\nRMSE:", fit.res$ssr, "\n")
fit.res_end <- Sys.time();fit.res_end
# 4. MCMC  -------------------------------------------------------------
cat("\nMCMC sampling...\n")
set.seed(3)
mcmc.res <- modMCMC(f = model_cost,
                    p  = fit.res$par,
                    niter = 20000, 
                    burninlength = 6000,
                    jump = 0.15, 
                    updatecov = 150,
                    lower = rep(c(1e-6, 1e-6, 10), 5),
                    upper = rep(c(1, 1, 15), 5))

cat("MCMC sampling finish!\n")
cat("Acceptance:", mcmc.res$acceptance, "\n")
mcmc.res_end <- Sys.time();mcmc.res_end

mcmc_chain1 <- as.mcmc(mcmc.res$pars)

geweke_res1 <- geweke.diag(
  mcmc_chain1,
  frac1 = 0.1, 
  frac2 = 0.5   
);geweke_res1
geweke.plot(mcmc_chain1)
effectiveSize(mcmc_chain1)

# 5. Posterior sample ----------------------------------------------------------
posterior <- as.data.frame(mcmc.res$pars)
colnames(posterior) <- fit.par.names
saveRDS(posterior, "3.result/posterior_meta.rds")
cat("Posterior sample\n")

for(i in 1:5) {
  cat("\n", Districts[i], ":\n", sep="")
  cat("  beta_mp0: ", round(mean(posterior[, paste0("beta_mp0_", Districts[i])]), 4), 
      " (", round(sd(posterior[, paste0("beta_mp0_", Districts[i])]), 4), ")\n", sep="")
  cat("  beta_pm0: ", round(mean(posterior[, paste0("beta_pm0_", Districts[i])]), 4),
      " (", round(sd(posterior[, paste0("beta_pm0_", Districts[i])]), 4), ")\n", sep="")
  cat("  k: ", round(mean(posterior[, paste0("k_", Districts[i])]), 2),
      " (", round(sd(posterior[, paste0("k_", Districts[i])]), 2), ")\n", sep="")
}

# 6. simulating----------------------------------------------------
cat("\n Start simulation...\n")
set.seed(2025)

pos.sample <- posterior %>% sample_n(100)

sim_lst <- apply(pos.sample, 1, function(p){
  parms.run <- c(parms.fixed, setNames(p, fit.par.names))
  
  k_vec <- numeric(5)
  for(i in 1:5) {
    k_name <- paste0("k_", Districts[i])
    k_vec[i] <- parms.run[[k_name]]
  }
  
  init_Sa <- Pop_dis * k_vec
  init_Ia <- rep(0, 5)
  init_Sm <- init_Sa * 0.859
  init_Em <- rep(0, 5)
  init_Im <- rep(0, 5)
  init_Sp <- Pop_dis - c(1, 0, 0, 0, 0)  
  init_Ep <- rep(0, 5)
  init_Ip <- c(1, 0, 0, 0, 0)              
  init_Ap <- rep(0, 5)
  init_Rp <- rep(0, 5)
  init_Xp <- c(1, 0, 0, 0, 0)
  
  init <- c(init_Sa, init_Ia, init_Sm, init_Em, init_Im,
            init_Sp, init_Ep, init_Ip, init_Ap, init_Rp, init_Xp)
  
  out <- ode(y = init, times = times,
             func = Foshan_Meta_Model, parms = parms.run,method = "lsoda")
  
  xp_start <- 52
  xp_end <- 56
  cumulative <- out[, xp_start:xp_end]
  
  sim <- matrix(0, nrow = n_day, ncol = 5)
  sim[1, ] <- cumulative[1, ]
  for(i in 2:n_day) {
    sim[i, ] <- cumulative[i, ] - cumulative[i-1, ]
  }
  
  return(sim)
})

sim.array <- array(unlist(sim_lst), dim = c(n_day, 5, 100))  # day×district×sample

# sim.array <- readRDS("3.result/sim_array_meta.rds")
# list2env(readRDS("3.result/fit_summary_meta.rds"), envir = .GlobalEnv)

ci.low  <- apply(sim.array, c(1, 2), quantile, probs = 0.025)
ci.high <- apply(sim.array, c(1, 2), quantile, probs = 0.975)
med     <- apply(sim.array, c(1, 2), median)


# 7. plot ------------------------------------------------------------------

plot.df <- bind_rows(
  lapply(1:5, function(i){
    data.frame(
      date = cases_obs$date,
      District = Districts[i],
      obs  = cases_obs[[i+1]],
      low  = ci.low[, i],
      high = ci.high[, i],
      mid  = med[, i]
    )
  })
)

plot.df$District <- factor(plot.df$District, c("Shunde", "Chancheng", "Nanhai", "Gaoming", "Sanshui"))


p <- ggplot(plot.df, aes(x = date)) +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "skyblue",alpha = 0.4) +
  geom_line(aes(y = mid),color = "#0072B5",linewidth = 0.9) +
  geom_point(aes(y = obs),color = "black",size = 1.1,alpha = 0.8) +
  facet_wrap(~ District, ncol = 2, scales = "free_y") +
  labs(y = "Daily cases",x = NULL,title = "") +
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
  )+
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "4 days");p

ggsave("3.result/plot fig/meta_fit_district_params.png", p, 
       width = 7, height = 5, dpi = 400)

# 8. Evaluation metrics --------------------------------------------------------------
pred.mat <- cbind(med)  
obs.mat  <- as.matrix(cases_obs[, -1])

cat("R2 =", R2_Score(pred.mat, obs.mat), "\n")
# cat("R2 (Shunde) =", R2_Score(pred.mat[,1], obs.mat[,1]), "\n")
# cat("R2 (Chancheng) =", R2_Score(pred.mat[,2], obs.mat[,2]), "\n")
# cat("R2 (Nanhai) =", R2_Score(pred.mat[,3], obs.mat[,3]), "\n")
# cat("R2 (Gaoming) =", R2_Score(pred.mat[,4], obs.mat[,4]), "\n")
# cat("R2 (Sanshui) =", R2_Score(pred.mat[,5], obs.mat[,5]), "\n")
cat("RMSE =", RMSE(pred.mat, obs.mat), "\n")


# 9. save ----------------------------------------------------------
saveRDS(sim.array, "3.result/sim_array_meta.rds")
saveRDS(list(times = times,
             ci.low = ci.low,
             ci.high = ci.high,
             median = med,
             obs = cases_obs,
             fit_params = fit.res$par,
             posterior = posterior), 
        "3.result/fit_summary_meta.rds")



