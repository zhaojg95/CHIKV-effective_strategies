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
posterior_samples1 <- readRDS("3.result/posterior_meta.rds") %>% 
  pivot_longer(
    cols = everything(),      
    names_to = "parameter",   
    values_to = "value"     
  ) %>% 
separate(parameter,
           into = c("param", "region"),
           sep = "_(?=[^_]+$)")

posterior_samples1$region <- factor(posterior_samples1$region, levels = c("Shunde","Chancheng","Nanhai","Gaoming","Sanshui"))
ggplot(posterior_samples1, aes(x = region, y = value, fill = region)) +
  geom_boxplot(width = 0.6, outlier.size = 0.6) +
  facet_wrap(~ param, scales = "free_y") +
  scale_fill_npg()+
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "none"
  )

