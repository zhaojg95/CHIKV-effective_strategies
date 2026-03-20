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

# 2. Baidu migration index ----------------------------------------------
baidu_mig <- read.xlsx("1.data/Foshan-baidu migration index.xlsx", detectDates = T) 
func_BD_in  <- approxfun(x = 1:n_day,
                         y = baidu_mig$value_move_in[1:n_day], rule = 2)
func_BD_out <- approxfun(x = 1:n_day,
                         y = baidu_mig$value_move_out[1:n_day], rule = 2)

# 3. meta_population model --with intervention -------------------------

set.seed(2025)
pos.sample <- posterior_samples
control_time <- c(31)

#LC:----
# ua_c <- seq(0,0.9,0.1)
um_c <- 0
R_bite <- 0
transmission_time <- 7


result_ac <- c()
for (ua_c in seq(0,0.9,0.1)) {
  
  R_gamma1  <-  R_gamma <- 1/transmission_time
  
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
               func = Foshan_Meta_Model_intervention, parms = parms.run)
    
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
  
  saveRDS(sim.array,file = paste0("3.result/single intervention/sim.array(ua_c=",ua_c,").rds"))
  
  ci.low  <- apply(sim.array, c(1, 2), quantile, probs = 0.025)
  ci.high <- apply(sim.array, c(1, 2), quantile, probs = 0.975)
  med     <- apply(sim.array, c(1, 2), median);med
  

  region_sums <- apply(sim.array, c(2, 3), sum)  # 6×100
  region_median <-   matrixStats::rowMedians(region_sums)
  region_ci.high <- apply(region_sums, 1, quantile, probs = 0.975)
  region_ci.low <- apply(region_sums, 1, quantile, probs = 0.025)

  
  
  Districts1 <- c(Districts,"Foshan")
  cat("\nfigure...\n")
  plot.df <- bind_rows(
    lapply(1:6, function(i){
      data.frame(
        date = 
          seq(as.Date("2025-6-16"),as.Date("2025-6-16")+180-1,"day"),
        District = Districts1[i],
        low  = ci.low[, i],
        high = ci.high[, i],
        mid  = med[, i],
        total_median = region_median[i],
        total_ci.high = region_ci.high[i],
        total_ci.low = region_ci.low[i]
      )
    })
  ) 
  
  plot.df$District <- factor(plot.df$District, levels = c("Foshan","Shunde","Chancheng","Nanhai","Gaoming" ,"Sanshui"))
  plot.df$group <- ua_c
  result_ac <- rbind(result_ac,plot.df)
}


#AMC:----
ua_c <- 0
um_c <- 0
R_bite <- 0
transmission_time <- 7

result_mc <- c()
for (um_c in seq(0,0.9,0.1)) {
  
  R_gamma1  <-  R_gamma <- 1/transmission_time
  
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
               func = Foshan_Meta_Model_intervention, parms = parms.run)
    
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
  
  sim.array <- array(unlist(sim_lst), dim = c(n_day, 6, 100))  # day ×district × sample

  
  saveRDS(sim.array,file = paste0("3.result/single intervention/sim.array(um_c=",um_c,").rds"))
  ci.low  <- apply(sim.array, c(1, 2), quantile, probs = 0.025)
  ci.high <- apply(sim.array, c(1, 2), quantile, probs = 0.975)
  med     <- apply(sim.array, c(1, 2), median);med
  
  region_sums <- apply(sim.array, c(2, 3), sum)  
  region_median <-   matrixStats::rowMedians(region_sums)
  region_ci.high <- apply(region_sums, 1, quantile, probs = 0.975)
  region_ci.low <- apply(region_sums, 1, quantile, probs = 0.025)
  
  
  Districts1 <- c(Districts,"Foshan")
  cat("\nfigure...\n")
  plot.df <- bind_rows(
    lapply(1:6, function(i){
      data.frame(
        date = 
          seq(as.Date("2025-6-16"),as.Date("2025-6-16")+180-1,"day"),
        District = Districts1[i],
        # obs  = cases_obs[[i+1]],
        low  = ci.low[, i],
        high = ci.high[, i],
        mid  = med[, i],
        total_median = region_median[i],
        total_ci.high = region_ci.high[i],
        total_ci.low = region_ci.low[i]
      )
    })
  ) 
  
  plot.df$District <- factor(plot.df$District, levels = c("Foshan","Shunde","Chancheng","Nanhai","Gaoming" ,"Sanshui"))
  plot.df$group <- um_c
  result_mc <- rbind(result_mc,plot.df)
}

#BRR:----
##r_bite ####
ua_c <- 0
um_c <- 0
R_bite <- 0
transmission_time <- 7

result_rbite <- c()
for (R_bite in seq(0,0.9,0.1)) {
  
  R_gamma1  <-  R_gamma <- 1/transmission_time
  
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
               func = Foshan_Meta_Model_intervention, parms = parms.run)
    
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
  
  saveRDS(sim.array,file = paste0("3.result/single intervention/sim.array(R_bite=",R_bite,").rds"))
  
  ci.low  <- apply(sim.array, c(1, 2), quantile, probs = 0.025)
  ci.high <- apply(sim.array, c(1, 2), quantile, probs = 0.975)
  med     <- apply(sim.array, c(1, 2), median);med
  
  
  region_sums <- apply(sim.array, c(2, 3), sum)  # 6×100
  region_median <-   matrixStats::rowMedians(region_sums)
  region_ci.high <- apply(region_sums, 1, quantile, probs = 0.975)
  region_ci.low <- apply(region_sums, 1, quantile, probs = 0.025)
  
  
  
  Districts1 <- c(Districts,"Foshan")
  cat("\nfigure...\n")
  plot.df <- bind_rows(
    lapply(1:6, function(i){
      data.frame(
        date = 
          seq(as.Date("2025-6-16"),as.Date("2025-6-16")+180-1,"day"),
        District = Districts1[i],
        # obs  = cases_obs[[i+1]],
        low  = ci.low[, i],
        high = ci.high[, i],
        mid  = med[, i],
        total_median = region_median[i],
        total_ci.high = region_ci.high[i],
        total_ci.low = region_ci.low[i]
      )
    })
  ) 
  
  plot.df$District <- factor(plot.df$District, levels = c("Foshan","Shunde","Chancheng","Nanhai","Gaoming" ,"Sanshui"))
  plot.df$group <- R_bite
  result_rbite <- rbind(result_rbite,plot.df)
}


#IPS:----
ua_c <- 0
um_c <- 0
R_bite <- 0
transmission_time <- 7

result_trans <- c()
for (transmission_time in seq(1,7,1)) {
  
  R_gamma1  <-  R_gamma <- 1/transmission_time
  
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
               func = Foshan_Meta_Model_intervention, parms = parms.run)
    
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
  
  sim.array <- array(unlist(sim_lst), dim = c(n_day, 6, 100))  # day×district×sample
  
  saveRDS(sim.array,file = paste0("3.result/single intervention/sim.array(transmission_time=",transmission_time,").rds"))
  
  ci.low  <- apply(sim.array, c(1, 2), quantile, probs = 0.025)
  ci.high <- apply(sim.array, c(1, 2), quantile, probs = 0.975)
  med     <- apply(sim.array, c(1, 2), median);med
  
  region_sums <- apply(sim.array, c(2, 3), sum)  # 6×100
  region_median <-   matrixStats::rowMedians(region_sums)
  region_ci.high <- apply(region_sums, 1, quantile, probs = 0.975)
  region_ci.low <- apply(region_sums, 1, quantile, probs = 0.025)
  

  Districts1 <- c(Districts,"Foshan")
  cat("\nfigure...\n")
  plot.df <- bind_rows(
    lapply(1:6, function(i){
      data.frame(
        date = 
          seq(as.Date("2025-6-16"),as.Date("2025-6-16")+180-1,"day"),
        District = Districts1[i],
        # obs  = cases_obs[[i+1]],
        low  = ci.low[, i],
        high = ci.high[, i],
        mid  = med[, i],
        total_median = region_median[i],
        total_ci.high = region_ci.high[i],
        total_ci.low = region_ci.low[i]
      )
    })
  ) 
  
  plot.df$District <- factor(plot.df$District, levels = c("Foshan","Shunde","Chancheng","Nanhai","Gaoming" ,"Sanshui"))
  plot.df$group <- transmission_time
  result_trans <- rbind(result_trans,plot.df)
}


theme <- theme(
  strip.background = element_rect(
    fill = "white",      
    colour = "white",        
    size = 0.3              
  ),
  axis.text.x = element_text(size = 12, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
  axis.text.y = element_text(size = 12, colour = "black",family = "sans"),
  plot.title = element_text(size = 16, face = "bold"),  
  axis.title.x = element_text(size = 12),  
  axis.title.y = element_text(size = 12), 
  legend.title = element_text(size = 12), 
  legend.text = element_text(size = 12,family = "sans"),  
  legend.position = "right",  
  legend.background = element_blank(),   
  legend.key        = element_blank(),   
  strip.text = element_text(size = 14, face = "bold"), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),     
  axis.line.x = element_line(colour = "black", linewidth = .5),
  axis.line.y = element_line(colour = "black", linewidth = .5)
)

# plot  trend ------------------------------------------------------------

plot.df  <- result_ac
plot.df$group <- as.factor(plot.df$group)
p_ac <- ggplot(plot.df, aes(x = date,group = group, col = group,fill = group )) +
  geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.3,color = NA) +
  scale_color_npg()+
  geom_line(aes(y = mid/1000),linewidth = 0.9) +
  # geom_point(aes(y = obs),color = "black",size = 1.1,alpha = 0.8) +
  facet_wrap(~ District, ncol = 2, scales = "free_y") +
  labs(y = "Daily cases",x = NULL,title = " ") +
  theme_bw() +
  guides(guides(fill = guide_legend(ncol = 1, byrow = F)))  +
  theme+
  labs(title = "a",
       colour = "LC intensity",
       fill   = "LC intensity")+
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "1 month")+
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_ac

###mc####
plot.df  <- result_mc
plot.df$group <- as.factor(plot.df$group)
p_mc <- ggplot(plot.df, aes(x = date,group = group, col = group,fill = group )) +
  geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.3,color = NA) +
  scale_color_npg()+
  geom_line(aes(y = mid/1000),linewidth = 0.9) +
  # geom_point(aes(y = obs),color = "black",size = 1.1,alpha = 0.8) +
  facet_wrap(~ District, ncol = 2, scales = "free_y") +
  labs(y = "Daily cases",x = NULL,title = " ") +
  theme_bw() +
  guides(guides(fill = guide_legend(ncol = 1, byrow = F)))  +
  theme+
  labs(title = "b",
       colour = "AMC intensity",
       fill   = "AMC intensity")+
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "1 month")+
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_mc

####rbite####
plot.df  <- result_rbite
plot.df$group <- as.factor(plot.df$group)
p_bite <- ggplot(plot.df, aes(x = date,group = group, col = group,fill = group )) +
  geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.3,color = NA) +
  scale_color_npg()+
  geom_line(aes(y = mid/1000),linewidth = 0.9) +
  # geom_point(aes(y = obs),color = "black",size = 1.1,alpha = 0.8) +
  facet_wrap(~ District, ncol = 2, scales = "free_y") +
  labs(y = "Daily cases",x = NULL,title = " ") +
  theme_bw() +
  guides(guides(fill = guide_legend(ncol = 1, byrow = F)))  +
  theme+
  labs(title = "c",
       colour = "BRR intensity",
       fill   = "BRR intensity")+
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "1 month")+
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_bite 

####trans####
plot.df  <- result_trans
plot.df$group <- factor(plot.df$group,levels = c("7","6","5","4","3","2","1"))
p_trans <- ggplot(plot.df, aes(x = date,group = group, col = group,fill = group )) +
  geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.3,color = NA) +
  scale_color_npg()+
  geom_line(aes(y = mid/1000),linewidth = 0.9) +
  # geom_point(aes(y = obs),color = "black",size = 1.1,alpha = 0.8) +
  facet_wrap(~ District, ncol = 2, scales = "free_y") +
  labs(y = "Daily cases",x = NULL,title = " ") +
  theme_bw() +
  guides(guides(fill = guide_legend(ncol = 1, byrow = F)))  +
  theme+
  labs(title = "d",
       colour = "IPS intensity",
       fill   = "IPS intensity")+
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "1 month")+
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_trans 


meta_district_trend <- p_ac + p_mc + p_bite + p_trans

# sim_cases_Foshan <- subset(plot.df,District == "Foshan")

ggsave("3.result/plot fig/meta_district_trend.png",meta_district_trend,
       width = 20, height = 11, dpi = 800)


# total plot --------------------------------------------------------------

theme1 <- theme(
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 8, colour = "black",family = "sans"),
  plot.title = element_text(size = 12, face = "bold"), 
  axis.title.x = element_blank(), 
  axis.title.y = element_text(size = 8),  
  legend.title = element_text(size = 8), 
  legend.text = element_text(size = 8,family = "sans"),
  legend.position = "right", 
  legend.background = element_blank(),   
  legend.key        = element_blank(),   
  legend.key.size = unit(0.25, "cm"),  
  legend.spacing.x = unit(0.25, 'cm'),  
  strip.text = element_text(size = 8, face = "bold"),  
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()  
) 
# plot.df  <- result_ac %>% 
#   subset(District =="Foshan")
plot.df  <- result_ac 
plot.df$group <- as.factor(plot.df$group)


p_ac_total <- ggplot(plot.df, aes(x = group,y = total_median / 1000,fill = group)) +
  geom_col(position = position_dodge(width = 0.8),width = 0.75) +
  scale_fill_npg()+
  facet_wrap(~District,scales = "free_y",nrow = 1)+
  geom_errorbar(aes(ymin = total_ci.low / 1000,ymax = total_ci.high / 1000,group = group),
                position = position_dodge(width = 0.8),width = 0.3,colour = "black",linewidth = 0.2)+
  labs(title = "a",x = " ",y = "Cumulative cases",
       colour = "LC intensity",
       fill   = "LC intensity") +
  theme_bw()+
  theme1+
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_ac_total

plot.df  <- result_mc 
plot.df$group <- as.factor(plot.df$group)
p_mc_total <- ggplot(plot.df, aes(x = group,y = total_median / 1000,fill = group)) +
  geom_col(position = position_dodge(width = 0.8),width = 0.75) +
  scale_fill_npg()+
  facet_wrap(~District,scales = "free_y",nrow = 1)+
  geom_errorbar(aes(ymin = total_ci.low / 1000,ymax = total_ci.high / 1000,group = group),
                position = position_dodge(width = 0.8),width = 0.3,colour = "black",linewidth = 0.2)+
  labs(title = "b",x = " ",y = "Cumulative cases",
       colour = "AMC intensity",
       fill   = "AMC intensity") +
  theme_bw()+
  theme1 +
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_mc_total

plot.df  <- result_rbite
plot.df$group <- as.factor(plot.df$group)
p_rbite_total <- ggplot(plot.df, aes(x = group,y = total_median / 1000,fill = group)) +
  geom_col(position = position_dodge(width = 0.8),width = 0.75) +
  scale_fill_npg()+
  facet_wrap(~District,scales = "free_y",nrow = 1)+
  geom_errorbar(aes(ymin = total_ci.low / 1000,ymax = total_ci.high / 1000,group = group),
                position = position_dodge(width = 0.8),width = 0.3,colour = "black",linewidth = 0.2)+
  labs(title = "c",x = " ",y = "Cumulative cases",
       colour = "BRR intensity",
       fill   = "BRR intensity") +
  theme_bw()+
  theme1 +
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_rbite_total


plot.df  <- result_trans
plot.df$group <- factor(plot.df$group, levels = c("7","6","5","4","3","2","1"))
p_trans_total <- ggplot(plot.df, aes(x = group,y = total_median / 1000,fill = group)) +
  geom_col(position = position_dodge(width = 0.8),width = 0.75) +
  scale_fill_npg()+
  facet_wrap(~District,scales = "free_y",nrow = 1)+
  geom_errorbar(aes(ymin = total_ci.low / 1000,ymax = total_ci.high / 1000,group = group),
                position = position_dodge(width = 0.8),width = 0.3,colour = "black",linewidth = 0.2)+
  labs(title = "d",x = " ",y = "Cumulative cases",
       colour = "IPS intensity",
       fill   = "IPS intensity") +
  theme_bw()+
  theme1+
  scale_y_continuous(labels = function(x) paste0(x, "k"));p_trans_total


meta_district_trend  <- p_ac_total/p_mc_total/ p_rbite_total /p_trans_total; meta_district_trend 
ggsave("3.result/plot fig/meta_district_total.png",meta_district_trend,
       width = 15, height = 8, dpi = 400)


save.image("3.result/sim_result_single_intervention.RData")
