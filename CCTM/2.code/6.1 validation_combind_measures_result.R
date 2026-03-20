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


# 
# result_combind_measures_control_time <- readRDS("3.result/result_combind_measures_control_time.rds")
# result_combind_measures_control_time_summary <- result_sim3(result_combind_measures_control_time)
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
data$type1 <- rep(letters[1:15],each = 62)  #0.75 27

data$day <- as.Date("2025-6-16")+data$day-1
fitted <- ggplot(data, aes(x = day)) +
  geom_col(aes(y = cases, fill = ifelse(day > as.Date("2025-7-16"), 
                                        "Blue cases", "Reported cases")),alpha = 1) +
  geom_vline(xintercept = as.Date("2025-7-16"), linetype = "dashed", color = "grey90")+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.6, fill = "pink") +
  scale_fill_manual(values = c("Reported cases" = "#E64B35FF", "Blue cases" = "#3C5488FF")) +
  geom_line(aes(y = mid), linewidth = 0.6,alpha = 0.8, color = "black") +
  labs(y = "Daily cases", x = " ") +
  theme_bw() +
  facet_wrap(~ type1, ncol = 4) + 
  theme(
    axis.text.x = element_text(size = 6, colour = "black", vjust = 0.5, hjust = 0.5, angle = 0),
    axis.text.y = element_text(size = 6, colour = "black"),
    plot.title = element_text(size = 6, face = "bold"),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.position = "",
    legend.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_blank(),  
    # strip.title = element_text(size = 6, face = "bold"),
    strip.background = element_rect(fill = "lightblue"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(date_labels = "%b %d\n2025", date_breaks = "15 days")+
  geom_text(aes(label = paste("R²:", round(R2, 3)),  x = as.Date("2025-6-16")+65, y = 480),
            # fontface = "italic",family = "sans",
            hjust = 1.1, vjust = 1.1, size = 2.0, color = "black", inherit.aes = FALSE)  
fitted

ggsave(filename = "3.result/plot fig/Fig.6—fit.png",fitted,width = 17,height = 10,dpi = 800,units = "cm")


