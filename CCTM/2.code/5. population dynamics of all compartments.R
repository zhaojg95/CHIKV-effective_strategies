options(scipen = 200)
# 0. install & library packages -------------------------------------------
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes","patchwork","metR")
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

#simulating the trend of disease based on the fitted parameters 
sim_cases_compartment <- lapply(seq_len(nrow(posterior_samples)), function(i) {
  
  parms <- c(parms.fixed, as.list(posterior_samples[1,]))
  k <- parms["k"]
  Np <- 9698900
  Na <- Np*as.numeric(k)
  Nm <- Na*0.859  #e_p
  parms <- c(parms.fixed,Nm = Nm, as.list(posterior_samples[1,]))
  init_value <- c(Sa = Na, Ia = 0, Sm = Nm, Em = 0, Im = 0, Sp = Np - 1, Ep = 0, Ip = 1, Ap = 0, Rp = 0, X = 1)
  ode(
    y     = init_value,
    times = times,
    func  = CHIKV_model,
    parms = parms
  ) %>% as.data.frame() %>%
    mutate(Na = Sa + Ia,
           Nm = Sm + Em + Im)
})

# merged result
posterior_summary <- function(sim_list, drop_vars = c("time", "X"),
                              probs = c(0.025, 0.975)) {
  
  vars <- setdiff(colnames(sim_list[[1]]), drop_vars)
  
  summary <- lapply(vars, function(var) {
    
    mat <- do.call(cbind, lapply(sim_list, `[[`, var))
    
    data.frame(
      median = apply(mat, 1, median),
      q025   = apply(mat, 1, quantile, probs[1]),
      q975   = apply(mat, 1, quantile, probs[2])
    )
  })
  
  names(summary) <- vars
  summary
}

summary_list <- posterior_summary(sim_cases_compartment)

#plot
start_date <- as.Date("2025-06-16")

plot_df <- bind_rows(
  lapply(names(summary_list), function(state) {
    
    df <- summary_list[[state]]
    df$date  <- seq.Date(start_date, by = "day", length.out = nrow(df))
    df$state <- state
    df
  })
)


plot_df$state <- recode(
  plot_df$state,
  Sa = "S[a]", Ia = "I[a]",
  Sm = "S[m]", Em = "E[m]", Im = "I[m]",
  Sp = "S[p]", Ep = "E[p]", Ip = "I[p]",
  Ap = "A[p]", Rp = "R[p]",
  Na = "N[a]", Nm = "N[m]"
)

plot_df$state <- factor(plot_df$state, levels = c(
  "S[a]", "I[a]",
  "S[m]", "E[m]", "I[m]",
  "S[p]", "E[p]", "I[p]", "A[p]", "R[p]",
  "N[a]", "N[m]"
))

# 绘图
p <- ggplot(plot_df, aes(x = date)) +
  geom_ribbon(aes(ymin = q025/1000, ymax = q975/1000), fill = "skyblue", alpha = 0.5) +
  geom_line(aes(y = median/1000), linewidth = 0.3) +
  labs(x = "Date", y = "Population") +
  facet_wrap(~ state, nrow = 4, ncol = 3, scales = "free_y",labeller =  label_parsed) +
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "1 month") +
  scale_y_continuous(labels = function(x) paste0(x, "k")) +
  theme_bw() +
  theme(
    axis.text.x      = element_text(size = 6, colour = "black", angle = 0, hjust = 0, vjust = 1),
    axis.text.y      = element_text(size = 6, colour = "black"),
    axis.title.x     = element_text(size = 6),
    axis.title.y     = element_text(size = 6),
    plot.title       = element_text(size = 6, face = "bold"),
    strip.background = element_blank(),  
    strip.text.x     = element_text(size = 6, face = "bold", hjust = 0), 
    legend.position  = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



p
ggsave(filename = "3.result/plot fig/summary_list.png",p,width = 18,height = 12,dpi = 800,units = "cm")

write.xlsx(plot_df,"3.result/different_stage_population.xlsx")

