rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes","patchwork","metR","ggpubr")
pacman::p_load(pkgs,character.only = T)

# 1.loading results -------------------------------------------------------
source("2.code/result_sim.R")
result_baseline <- readRDS("3.result/result_baseline.rds") 
result_combind_interventions <- readRDS(paste0("3.result/result_combind_measures_control_time(",31,").rds")) 
result_combind_interventions$transmission_time <- as.numeric(result_combind_interventions$transmission_time)
result_combind_interventions_summary <- result_sim3(result_combind_interventions)


Foshan_cases <- read.xlsx("1.data/Foshan_cases.xlsx", detectDates = T,sheet = 2)

times <- 1:180

result_combind_interventions <- result_combind_interventions %>% 
  filter(control_time == 31,day <= 62,ua_c > 0,um_c > 0,R_bite > 0 ,transmission_time<7) %>% 
  .[1:9] %>% 
  mutate(type = paste0('A=',ua_c,', B=',um_c,', C=',R_bite,', D=',transmission_time)) 
R2 <- result_combind_interventions %>% 
  group_by(type) %>% 
  filter(day > 31) %>% 
  summarise(R2 = round(R2_Score(mid,Foshan_cases$Cases[-c(1:31)]),2))

sum(R2$R2 >=0)
sum(R2$R2 >=0)/4374

R2_0.8 <-  R2%>% 
  filter(R2 >= 0.8) %>% 
  arrange(desc(R2))
# 2. plot fig -------------------------------------------------------------

data <- subset(result_combind_interventions, type %in% R2_0.8$type) %>% 
  mutate(cases = rep(Foshan_cases$Cases,nrow(R2_0.8))) %>% 
  left_join(R2_0.8,by = "type") %>% 
  arrange(desc(R2))

data$type <- factor(data$type,levels = R2_0.8$type)
data$type1 <- rep(letters[1:14],each = 62)  

data$day <- as.Date("2025-6-16")+data$day-1


fitted <- ggplot(data, aes(x = day)) +
  geom_point(aes(y = cases, color = ifelse(day > as.Date("2025-07-16"),"Blue cases", "Reported cases")),size = 1) +
  geom_vline(xintercept = as.Date("2025-07-16"),linetype = "dashed",color = "grey70"
  ) +
  geom_ribbon(
    aes(ymin = low, ymax = high),
    alpha = 0.6,
    fill = "pink"
  ) +
  geom_line(
    aes(y = mid),
    linewidth = 0.6,
    color = "black"
  ) +
  scale_color_manual(
    values = c(
      "Reported cases" = "#E64B35FF",
      "Blue cases" = "#3C5488FF"
    )
  ) +
  labs(y = "Daily cases", x = NULL) +
  facet_wrap(~type1, ncol = 4) +
  scale_x_date(
    limits = c(as.Date("2025-06-15"), as.Date("2025-8-20")),
    breaks = seq(as.Date("2025-06-15"),
                 as.Date("2025-12-31"),
                 by = "15 days"),
    date_labels = "%b %d\n2025",
    expand = c(0.1, 0)
  )+
  theme_bw() +
  theme(
    axis.text.x = element_text(
      size = 5,
      colour = "black",
      hjust = 0.5,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.title = element_text(size = 6),
    legend.position = "none",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_blank(),
    strip.background = element_rect(fill = "lightblue"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fitted


ggsave(filename = "3.result/plot fig/Fig.7.png",fitted,width = 18,height = 10,dpi = 800,units = "cm")
ggsave(filename = "3.result/plot fig/Fig.7.pdf",fitted,width = 18,height = 10,dpi = 800,units = "cm")


