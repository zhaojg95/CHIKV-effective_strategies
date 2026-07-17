rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes","patchwork","metR")
pacman::p_load(pkgs,character.only = T)
# 1.loading results -------------------------------------------------------
source("2.code/Metapopulation_model.R")
source("2.code/init_meta_parameters.R")   

cases_obs <- read.xlsx("1.data/Foshan_cases_byDistrict.xlsx", detectDates = T) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>% 
  subset(date <= max(date) )

set.seed(2025)
posterior_samples <- readRDS("3.result/posterior_meta.rds") %>% 
  .[sample(nrow(.), 100), ]

n_day <- 180
times   <- 1:n_day

# 2. Baidu migration index  ----------------------------------------------
baidu_mig <- read.xlsx("1.data/Foshan-baidu migration index.xlsx", detectDates = T) 
func_BD_in  <- approxfun(x = 1:n_day,
                         y = baidu_mig$value_move_in[1:n_day], rule = 2)
func_BD_out <- approxfun(x = 1:n_day,
                         y = baidu_mig$value_move_out[1:n_day], rule = 2)

# 3. meta_population model --baseline scenario -------------------------

set.seed(2025)
pos.sample <- posterior_samples

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
             func = Foshan_Meta_Model, parms = parms.run)
  
  xp_start <- 52
  xp_end <- 56
  cumulative <- data.frame(out[, xp_start:xp_end])
  cumulative$Foshan <- apply(cumulative[,1:5],1,sum)
  
  sim <- matrix(0, nrow = n_day, ncol = 6)
  sim[1, ] <- as.numeric(cumulative[1, ])
  for(i in 2:n_day) {
    sim[i, ] <- as.numeric(cumulative[i, ] - cumulative[i-1, ])
  }

  return(sim)
})

sim.array <- array(unlist(sim_lst), dim = c(n_day, 6, 100))  # day × district × sample


ci.low  <- apply(sim.array, c(1, 2), quantile, probs = 0.025)
ci.high <- apply(sim.array, c(1, 2), quantile, probs = 0.975)
med     <- apply(sim.array, c(1, 2), median);med


Districts1 <- c(Districts,"Foshan")
cat("\n figures...\n")
plot.df <- bind_rows(
  lapply(1:6, function(i){
    data.frame(
      date = 
        seq(as.Date("2025-6-16"),as.Date("2025-6-16")+180-1,"day"),
      District = Districts1[i],
      low  = ci.low[, i],
      high = ci.high[, i],
      mid  = med[, i]
    )
  })
) 

plot.df$District <- factor(plot.df$District, levels = c("Foshan","Shunde","Chancheng","Nanhai","Gaoming" ,"Sanshui"))
p <- ggplot(plot.df, aes(x = date)) +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "grey70",alpha = 0.4) +
  geom_line(aes(y = mid),color = "#0072B5",linewidth = 0.9) +
  facet_wrap(~ District, ncol = 2, scales = "free_y") +
  labs(y = "Daily new cases",x = NULL,title = " ") +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(size = 10),
    plot.title = element_text(size = 12)
  );p

sim_cases_Foshan <- subset(plot.df,District == "Foshan")

ggsave("3.result/plot fig/meta_district_sim.png", p,
       width = 8, height = 5, dpi = 400)

save.image("3.result/sim_result_baseline.RData")
