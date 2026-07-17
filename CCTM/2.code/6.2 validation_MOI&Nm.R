rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
pkgs <- c("openxlsx","tidyverse","dplyr","deSolve","MLmetrics",
          "FME","scales","patchwork","ggsci","rlang","ggthemes","patchwork","metR","lubridate")
pacman::p_load(pkgs,character.only = T)
# 1. MOI & Nm -- plot ----------------------------------------------------------
Nm_population <- read.xlsx("3.result/different_stage_population.xlsx",detectDates = T) %>% 
  subset(state == "N[m]") %>% 
  mutate(
    date = as.Date(date),
    month_day = format(date, "%m-%d"),
    median = median/1000
  ) %>% 
  rename(Nm = median,
         Nm_025 = q025,
         Nm_975 = q975) %>% 
  dplyr::select(month_day,Nm) %>% 
  pivot_longer(
    cols= -month_day, names_to  = "type", values_to = "value"          
  )

moi <- read.xlsx("1.data/GD_MOI_2018-2023.xlsx") %>% 
  subset(Month>=6) %>% 
  mutate(md_date = paste0("2025-", Month,"-15")) %>% 
  mutate(date = as.Date(md_date),
         type = as.factor(Year))

Nm_population <- Nm_population %>%
  mutate(
    md_date = as.Date(paste0("2025-", month_day))
  )

moi_range <- range(moi$MOI, na.rm = TRUE)
nm_range  <- range(Nm_population$value, na.rm = TRUE)

scale_factor <- diff(nm_range) / diff(moi_range)

shape_vec <- c("2018" = 16,  
               "2019" = 17,  
               "2020" = 15, 
               "2021" = 18,  
               "2022" = 8,   
               "2023" = 3)  

p_MOI_Nm <- ggplot() +
  
  ## MOI
  
  geom_line(data = moi,aes(x = date,y = MOI,color = type,group = type),
            linetype = "dashed",
            linewidth = 1,
            alpha = 0.7
  ) +
  geom_point(data = moi,aes(x = date,y = MOI,color = type,shape = type),
             size = 2,
             alpha = 1
  ) +
  ## Nm
  geom_line(
    data = Nm_population,
    aes(
      x = md_date,
      y = (value - nm_range[1]) / scale_factor + moi_range[1],
      linetype = "Nm"
    ),
    linewidth = 1,
    color = "grey50"
  ) +
  
  scale_x_date(
    name = "Date",
    date_labels = "%b %d",
    breaks = seq(
      as.Date("2025-06-15"),
      as.Date("2025-12-31"),
      by = "1 month"
    )
  ) +
  
  scale_y_continuous(
    name = "Mosquito Oviposition Index (MOI)",
    sec.axis = sec_axis(
      ~ (. - moi_range[1]) * scale_factor + nm_range[1],
      name = expression(N[m]),
      labels = function(x) paste0(x, "k")
    )
  ) +
  labs(title = "a") +
  scale_color_npg(name = "MOI (year)") +
  scale_shape_manual(name = "MOI (year)", values = shape_vec) +
  
  scale_linetype_manual(
    name   = NULL,
    values = c("Nm" = "solid"),
    labels = expression(N[m])
  ) +
  
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8, colour = "black", vjust = 0.5, hjust = 0.5, angle = 0),
    axis.text.y = element_text(size = 8, colour = "black"),
    plot.title = element_text(size = 10, face = "bold"),  
    axis.title.x = element_text(size = 8),  
    axis.title.y = element_text(size = 8),  
    # legend.title = element_blank(),  
    legend.text = element_text(size = 8),  
    legend.title = element_text(size = 8),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.background = element_blank(),   
    legend.key        = element_blank(),   
    strip.text = element_text(size = 8, face = "bold"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    # plot.margin (b = 0),
    legend.box.margin = margin(t = -5),
    legend.margin = margin(0,0,0,0)
  )+
  
  guides(
    color    = guide_legend(nrow = 1, order = 1),
    shape    = guide_legend(nrow = 1, order = 1),
    linetype = guide_legend(nrow = 1, order = 2)
  )

p_MOI_Nm
ggsave(filename = "3.result/plot fig/p_MOI_Nm.png",p_MOI_Nm,width = 16,height = 8,dpi = 400,units = "cm")


# 2. correlation ----------------------------------------------------------

Nm_month <- Nm_population %>% 
  mutate(month = month(.$md_date)) %>% 
  group_by(month) %>% 
  summarise(Nm = mean(value))

Nm_moi <- data.frame(Nm = Nm_month$Nm,
                     moi_2018 = moi[moi$Year == 2018,]$MOI,
                     moi_2019 = moi[moi$Year == 2019,]$MOI,
                     moi_2020 = moi[moi$Year == 2020,]$MOI,
                     moi_2021 = moi[moi$Year == 2021,]$MOI,
                     moi_2022 = moi[moi$Year == 2022,]$MOI,
                     moi_2023 = moi[moi$Year == 2023,]$MOI)
cor(Nm_month$Nm, moi[moi$Year == 2018,]$MOI)
cor(Nm_month$Nm, moi[moi$Year == 2019,]$MOI)
cor(Nm_month$Nm, moi[moi$Year == 2020,]$MOI)
cor(Nm_month$Nm, moi[moi$Year == 2021,]$MOI)
cor(Nm_month$Nm, moi[moi$Year == 2022,]$MOI)
cor(Nm_month$Nm, moi[moi$Year == 2023,]$MOI)



library(reshape2)
Nm_moi_long <- melt(Nm_moi, id.vars = "Nm",
                    variable.name = "Year", value.name = "MOI")

moi_cols <- setdiff(names(Nm_moi), "Nm")
cor_vals <- sapply(moi_cols, function(col){
  cor(Nm_moi$Nm, Nm_moi[[col]], use = "complete.obs")
})

cor_df <- do.call(rbind, lapply(moi_cols, function(col){
  test <- cor.test(Nm_moi$Nm, Nm_moi[[col]])
  data.frame(
    Year = col,
    Cor  = round(test$estimate, 3),
    p    = test$p.value
  )
}))

cor_df$Year <- factor(
  cor_df$Year,
  levels = c("moi_2018","moi_2019","moi_2020","moi_2021","moi_2022","moi_2023"),
  labels = c("2018", "2019", "2020", "2021", "2022", "2023")
)

cor_df$label <- ifelse(
  cor_df$p < 0.001,
  paste0(
    "italic(r)==", round(cor_df$Cor,2),
    "*','~~italic(p)*''<0.001"
  ),
  paste0(
    "italic(r)==", round(cor_df$Cor,2),
    "*','~~italic(p)*''==", signif(cor_df$p, 2)
  )
)

Nm_moi_long$Year <- factor(
  Nm_moi_long$Year,
  levels = c("moi_2018","moi_2019","moi_2020","moi_2021", "moi_2022", "moi_2023"),
  labels = c("2018", "2019", "2020", "2021", "2022", "2023")
)

p <- ggplot(Nm_moi_long, aes(x = Nm/1000, y = MOI,colour = Year)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linewidth = 1) +  
  scale_color_npg() +
  facet_wrap(.~Year, scales = "free_y")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
    axis.text.y = element_text(size = 8, colour = "black",family = "sans"),
    plot.title = element_text(size = 10, face = "bold"),  
    axis.title.x = element_text(size = 8),  
    axis.title.y = element_text(size = 8),  
    # legend.title = element_blank(),  
    legend.text = element_text(size = 8),  
    legend.position  = " ",
    legend.direction = "horizontal",
    legend.background = element_blank(),   
    legend.key        = element_blank(),   
    strip.text = element_text(size = 8, face = "bold"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()  ,
    strip.background = element_rect(
      fill = "grey95",
      colour = "grey90"
    )
  )+
  labs(x = expression(N[m]~" (Million)"), y = "MOI", title = "b",color = "Year") +
  scale_x_continuous(labels = function(x) paste0(x, "m"))+
  geom_text(data = cor_df,aes(x = -Inf, y = Inf, label = label),hjust = -0.1,
            vjust = 1.2,size = 3,inherit.aes = FALSE,parse = TRUE)
print(p)

P <- p_MOI_Nm / p +  plot_layout(
  heights = c(1, 1)
)

ggsave(filename = "3.result/plot fig/p_MOI_Nm_COR.png",P,width = 15,height = 16,dpi = 400,units = "cm")
ggsave(filename = "3.result/plot fig/Fig. 6.pdf",P,width = 15,height = 16,dpi = 400,units = "cm")


