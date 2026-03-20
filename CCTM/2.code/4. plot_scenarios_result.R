rm(list = ls())
options(scipen = 200)
Sys.setlocale("LC_TIME", "en_US.UTF-8")
# 0. install & library packages -------------------------------------------

# install.packages("pacman")
pkgs <- c("openxlsx","tidyverse","deSolve","MLmetrics","FME","scales","patchwork","ggsci","rlang","ggthemes","patchwork","metR")
pacman::p_load(pkgs,character.only = T)

source("2.code/result_sim.R")
source("2.code/plot function.R")
times <- 1:180
start_date <- as.Date("2025-06-16")


# Baseline scenario -------------------------------------------------------

result_baseline <- readRDS("3.result/result_baseline.rds") 

result_baseline <- data.frame(day = result_baseline$day,
                                          ua_c = 0,um_c =0,R_bite = 0,transmission_time= 7,control_time =31) %>% 
  cbind(result_baseline[,c("low","high","mid","total_low","total_high","total_mid")])

result_baseline_summary <- result_sim3(result_baseline)

Np <- 9698900
result_baseline_summary$total_cases_mid / Np*100
result_baseline_summary$total_cases_low / Np*100
result_baseline_summary$total_cases_high / Np*100

p_baseline <- p_baseline_plot(data = result_baseline)
p_baseline

ggsave(filename = "3.result/plot fig/Fig. S2 p_baseline.png",p_baseline,width = 18,height = 8,dpi = 400,units = "cm")

# Scenario I --------------------------------------------------------------
#single intervention
control_time = 31
result_combind_interventions <- readRDS(paste0("3.result/result_combind_measures_control_time(",control_time,").rds")) 
result_combind_interventions_each <- result_sim2(result_combind_interventions)
result_combind_interventions_summary <- result_sim3(result_combind_interventions)

#larval control, LC
result_single_measure_aquatic_stage <- subset(result_combind_interventions, um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==control_time)
result_single_measure_aquatic_stage_each <- subset(result_combind_interventions_each$total_cases_each,um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==control_time) 
result_single_measure_aquatic_stage_summary <- subset(result_combind_interventions_summary,um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==control_time) 
#larval control, LC --sub
result_single_measure_aquatic_stage_sub <- result_single_measure_aquatic_stage %>% 
  .[!(.$ua_c %in% c("0", "0.1", "0.2")),]


#adult mosquito control, AMC
result_single_measure_adult_stage <- subset(result_combind_interventions,ua_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==control_time)
result_single_measure_adult_stage_each <- subset(result_combind_interventions_each$total_cases_each,ua_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==control_time)
result_single_measure_adult_stage_summary <- subset(result_combind_interventions_summary,ua_c ==0 & R_bite == 0 & transmission_time == 7 & control_time ==control_time)
#adult mosquito control, AME -- sub
result_single_measure_adult_stage_sub <- result_single_measure_adult_stage %>% 
  .[!(.$um_c %in% c("0", "0.1", "0.2")),]

# mosquito bite rate reduction, BRR
result_single_measure_Rbite <- subset(result_combind_interventions,ua_c ==0 & um_c ==0 &transmission_time == 7 & control_time ==control_time) 
result_single_measure_Rbite_each <- subset(result_combind_interventions_each$total_cases_each,ua_c ==0 & um_c ==0 & transmission_time == 7 & control_time ==control_time) 
result_single_measure_Rbite_summary <- subset(result_combind_interventions_summary,ua_c ==0 & um_c ==0 & transmission_time == 7 & control_time ==control_time) 

#infectious period shortening, IPS
result_single_measure_shorten_time <- subset(result_combind_interventions,ua_c ==0 & um_c ==0 & R_bite == 0 &  control_time ==control_time)
result_single_measure_shorten_time$transmission_time <- factor(result_single_measure_shorten_time$transmission_time,levels = c("7","6","5","4","3","2","1"))
result_single_measure_shorten_time_each <- subset(result_combind_interventions_each$total_cases_each,ua_c ==0 & um_c ==0 & R_bite == 0 & control_time ==control_time) 
result_single_measure_shorten_time_each$transmission_time <- factor(result_single_measure_shorten_time_each$transmission_time,levels = c("7","6","5","4","3","2","1"))
result_single_measure_shorten_time_summary <- subset(result_combind_interventions_summary,ua_c ==0 & um_c ==0 & R_bite == 0 & control_time ==control_time) 
# result_single_measure_shorten_time_summary$transmission_time <- factor(result_single_measure_shorten_time_summary$transmission_time,levels = c("7","6","5","4","3","2","1"))


#Supplementary Table
result_single_measure_summary <- rbind(result_single_measure_aquatic_stage_summary %>% 
                                         mutate(Interventions = "LC")%>% 
                                         .[,c("Interventions","ua_c","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>% 
                                         rename(Intensity = "ua_c"),
                                       result_single_measure_adult_stage_summary%>% 
                                         mutate(Interventions = "AMC")%>%
                                         .[,c("Interventions","um_c","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>%
                                         rename(Intensity = "um_c"),
                                       result_single_measure_Rbite_summary%>% 
                                         mutate(Interventions = "BRR")%>%
                                         .[,c("Interventions","R_bite","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>%
                                         rename(Intensity = "R_bite"),
                                       result_single_measure_shorten_time_summary%>% 
                                         mutate(Interventions = "IPS")%>%
                                         .[,c("Interventions","transmission_time","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>%
                                         rename(Intensity = "transmission_time")) %>% 
  mutate(total_cases_mid = round(total_cases_mid,0),
         totalcases_reduce_proportion = round(totalcases_reduce_proportion,1),
         peak_value= round( peak_value,0),
         peakvalue_reduce_proportion = round(peakvalue_reduce_proportion,1)) %>% 
  mutate(total_cases_mid = formatC(total_cases_mid, format = "f", big.mark = ",", digits = 0),
         peak_value = formatC(peak_value, format = "f", big.mark = ",", digits = 0))

write.xlsx(result_single_measure_summary,"3.result/result_single_measure_summary.xlsx")
#line plot
p_aquatic_stage <- p_line_func(result_single_measure_aquatic_stage,title = "a",legend_title = "LC intensity");p_aquatic_stage

p_adult_stage <- p_line_func(result_single_measure_adult_stage[,-c(2)],title = "b",legend_title = "AMC intensity");p_adult_stage

p_Rbite <- p_line_func(result_single_measure_Rbite[,-c(2,3)],title = "c",legend_title = "BRR intensity");p_Rbite

result_single_measure_shorten_time_summary$transmission_time <- factor(result_single_measure_shorten_time_summary$transmission_time,levels = c("7","6","5","4","3","2","1"))
p_shorten_time <- p_line_func(result_single_measure_shorten_time[,-c(2,3,4)],title = "d",legend_title = "IPS intensity");p_shorten_time

#line plot--sub
names(result_single_measure_aquatic_stage_sub)[2] <- "group"
result_single_measure_aquatic_stage_sub$group <- as.factor(result_single_measure_aquatic_stage_sub$group)
p_aquatic_stage_sub <- p_stage_sub_plot(result_single_measure_aquatic_stage_sub)
p_aquatic_stage_sub
ggsave(filename = "3.result/plot fig/single_intervention_aquatic_stage_sub.png",p_aquatic_stage_sub,width = 12,height = 8,dpi = 400,units = "cm")



names(result_single_measure_adult_stage_sub)[3] <- "group"
result_single_measure_adult_stage_sub$group <- as.factor(result_single_measure_adult_stage_sub$group)
p_adult_stage_sub <- p_stage_sub_plot(result_single_measure_adult_stage_sub)
p_adult_stage_sub
ggsave(filename = "3.result/plot fig/single_intervention_adult_stage_sub.png",p_adult_stage_sub,width = 12,height = 8,dpi = 400,units = "cm")


#boxplot

p_aquatic_stage_box <- single_boxplot_func(data = result_single_measure_aquatic_stage_each, col = 1,xlab = "LC intensity",title = "a")
p_adult_stage_box <-  single_boxplot_func(data = result_single_measure_adult_stage_each, col = 2,xlab = "AMC intensity",title = "b")
p_Rbite_box <-  single_boxplot_func(data = result_single_measure_Rbite_each, col = 3,xlab = "BRR intensity",title = "c")
p_shorten_time_box <-  single_boxplot_func(data = result_single_measure_shorten_time_each, col = 4,xlab = "IPS intensity",title = "d")


#bar plot
names(result_single_measure_aquatic_stage_summary)[1] <- "group"
result_single_measure_aquatic_stage_summary$group <- as.factor(result_single_measure_aquatic_stage_summary$group)
p_aquatic_stage_bar <- p_bar_func_total(data = result_single_measure_aquatic_stage_summary,x_lab = "LC intensity")

names(result_single_measure_adult_stage_summary)[2] <- "group"
result_single_measure_adult_stage_summary$group <- as.factor(result_single_measure_adult_stage_summary$group)
p_adult_stage_bar <-  p_bar_func_total(data = result_single_measure_adult_stage_summary,x_lab = "AMC intensity")

names(result_single_measure_Rbite_summary)[3] <- "group"
result_single_measure_Rbite_summary$group <- as.factor(result_single_measure_Rbite_summary$group)
p_Rbite_bar <-  p_bar_func_total(data = result_single_measure_Rbite_summary,x_lab = "BRR intensity")

names(result_single_measure_shorten_time_summary)[4] <- "group"
result_single_measure_shorten_time_summary$group <- as.factor(result_single_measure_shorten_time_summary$group)
p_shorten_time_bar <-  p_bar_func_total(data = result_single_measure_shorten_time_summary,x_lab = "IPS intensity")

box_bind <- (p_aquatic_stage_box+p_adult_stage_box)/(p_Rbite_box+p_shorten_time_box)
ggsave(filename = "3.result/plot fig/Fig.2 single_intervention--box_bind.png",box_bind ,width = 15,height = 12,dpi = 400,units = "cm")

combined <- (p_aquatic_stage + p_aquatic_stage_bar + p_adult_stage+ p_adult_stage_bar + p_Rbite+ p_Rbite_bar + p_shorten_time +p_shorten_time_bar) + 
  plot_layout(ncol = 2,widths = c(2.8, 1.2))
combined
ggsave(filename = "3.result/plot fig/Fig.2 single_intervention.png",combined,width = 18,height = 22,dpi = 400,units = "cm")

# Scenario II --------------------------------------------------------------
#single intervention & intervention timing

result_combind_measures_control_time_week <- rbind(readRDS("3.result/result_combind_measures_control_time(7).rds"),
                                              readRDS("3.result/result_combind_measures_control_time(14).rds"),
                                              readRDS("3.result/result_combind_measures_control_time(21).rds"),
                                              readRDS("3.result/result_combind_measures_control_time(28).rds"))
result_combind_measures_control_time_each <- result_sim2(result_combind_measures_control_time_week)
saveRDS(result_combind_measures_control_time_each, file = "3.result/result_combind_measures_control_time_each.rds")
result_combind_measures_control_time_each <- readRDS("3.result/result_combind_measures_control_time_each.rds")

#total cases
total_aquatic_stage_control_time <- subset(result_combind_measures_control_time_each$total_cases_each,ua_c %in%c(0.2,0.5,0.8) & um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time %in% c(7,14,21,28) ) 
peak_aquatic_stage_control_time <- subset(result_combind_measures_control_time_each$peak_each,ua_c %in%c(0.2,0.5,0.8) & um_c ==0 & R_bite == 0 & transmission_time == 7 & control_time %in% c(7,14,21,28) ) 
names(total_aquatic_stage_control_time)[1] <- "group"
names(peak_aquatic_stage_control_time)[1] <- "group"


total_aquatic_plot<- plot_func_total(total_aquatic_stage_control_time )
peak_aquatic_plot<- plot_func_peak(peak_aquatic_stage_control_time)

aquatic_plot <- total_aquatic_plot + peak_aquatic_plot;aquatic_plot
ggsave(filename = "3.result/plot fig/Fig. 3 aquatic_plot.png",aquatic_plot,width = 18,height = 4.3,dpi = 1200,units = "cm")

#adult mosquito control & intervention timing
#total cases
total_adult_stage_control_time <- subset(result_combind_measures_control_time_each$total_cases_each,ua_c ==0  & um_c %in%c(0.2,0.5,0.8)& R_bite == 0 & transmission_time == 7 & control_time %in% c(7,14,21,28) ) 
peak_adult_stage_control_time <- subset(result_combind_measures_control_time_each$peak_each,ua_c ==0  & um_c %in% c(0.2,0.5,0.8) & R_bite == 0 & transmission_time == 7 & control_time %in% c(7,14,21,28) ) 
names(total_adult_stage_control_time)[2] <- "group"
names(peak_adult_stage_control_time)[2] <- "group"


total_adult_plot<- plot_func_total(total_adult_stage_control_time,title = "b" )
peak_adult_plot<- plot_func_peak(peak_adult_stage_control_time)

adult_plot <- total_adult_plot + peak_adult_plot;adult_plot

ggsave(filename = "3.result/plot fig/Fig. 3 adult_plot.png",adult_plot,width = 18,height = 4.3,dpi = 1200,units = "cm")


# mosquito bite rate reduction & intervention timing
#total cases
total_BRR_control_time <- subset(result_combind_measures_control_time_each$total_cases_each,ua_c ==0  & um_c ==0 & R_bite %in%c(0.2,0.5,0.8) & transmission_time == 7 & control_time %in% c(7,14,21,28) ) 
peak_BRR_control_time <- subset(result_combind_measures_control_time_each$peak_each,ua_c ==0  & um_c ==0  & R_bite %in% c(0.2,0.5,0.8) & transmission_time == 7 & control_time %in% c(7,14,21,28) ) 
names(total_BRR_control_time)[3] <- "group"
names(peak_BRR_control_time)[3] <- "group"

total_BRR_plot<- plot_func_total(total_BRR_control_time,title = "c" )

peak_BRR_plot<- plot_func_peak(peak_BRR_control_time)

BRR_plot <- total_BRR_plot + peak_BRR_plot;BRR_plot

ggsave(filename = "3.result/plot fig/Fig. 3 BRR_plot.png",BRR_plot,width = 18,height = 4.3,dpi = 1200,units = "cm")

# Infectious period shortening & intervention timing
#total cases
total_IPS_control_time <- subset(result_combind_measures_control_time_each$total_cases_each,ua_c ==0  & um_c ==0 & R_bite==0 & transmission_time  %in%c(1,3,5) & control_time %in% c(7,14,21,28) ) 
peak_IPS_control_time <- subset(result_combind_measures_control_time_each$peak_each,ua_c ==0  & um_c ==0  & R_bite ==0  & transmission_time %in% c(1,3,5) & control_time %in% c(7,14,21,28) ) 
names(total_IPS_control_time)[4] <- "group"
names(peak_IPS_control_time)[4] <- "group"

total_IPS_plot<- plot_func_total(total_IPS_control_time,title = "d", x_lab = "Intervention timing",group0 = c(1,3,5) )
peak_IPS_plot<- plot_func_peak(peak_IPS_control_time,group0 = c(1,3,5), x_lab = "Intervention timing")

IPS_plot <- total_IPS_plot + peak_IPS_plot+
  plot_layout(axes = "collect");IPS_plot

ggsave(filename = "3.result/plot fig/Fig. 3 IPS_plot.png",IPS_plot,width = 18,height = 4.3,dpi = 1200,units = "cm")

combined_plot <- aquatic_plot / adult_plot / BRR_plot / IPS_plot +
  plot_layout(axes = "collect_y");combined_plot


ggsave(filename = "3.result/plot fig/Fig. 3 single interention $ timing.png",combined_plot ,width = 18,height = 3.5*4,dpi = 800,units = "cm")



####Scenario II : Supplementary Table
timing <- c(3,5,7,9,11,13,14,15,17,19,21,23,25,27,28,29)
result_combind_interventions_control_time_summary0 <- c()
for (cont in 1:length(timing)) {
  control_time = timing[cont]
  result_combind_interventions_control_time_summary <- result_sim3(readRDS(paste0("3.result/result_combind_measures_control_time(",control_time,").rds"))) %>% 
    .[,c("ua_c","um_c", "R_bite", "transmission_time", "control_time","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>% 
    mutate(total_cases_mid = round(total_cases_mid,0),
           totalcases_reduce_proportion = round(totalcases_reduce_proportion,1),
           peak_value= round( peak_value,0),
           peakvalue_reduce_proportion = round(peakvalue_reduce_proportion,1)) %>% 
    mutate(total_cases_mid = formatC(total_cases_mid, format = "f", big.mark = ",", digits = 0),
           peak_value = formatC(peak_value, format = "f", big.mark = ",", digits = 0)) %>% 
    subset((um_c ==0 & R_bite == 0 & transmission_time == 7)|
             (ua_c ==0 & R_bite == 0 & transmission_time == 7)|
             (ua_c ==0 & um_c ==0 & transmission_time == 7)|
             (ua_c ==0 & um_c ==0 & R_bite == 0)) %>% 
    filter(!(ua_c ==0 & um_c ==0 & R_bite == 0 & transmission_time == 7))
  
  result_combind_interventions_control_time_summary0 <- rbind(result_combind_interventions_control_time_summary0,result_combind_interventions_control_time_summary)
    
  }

LC_timing <- subset(result_combind_interventions_control_time_summary0,ua_c != 0) %>% 
  arrange(ua_c,desc(control_time)) %>% 
  mutate(Interventions = "LC")%>% 
  .[,c("Interventions","ua_c","control_time","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>% 
  rename(Intensity = "ua_c")
  

AMC_timing <- subset(result_combind_interventions_control_time_summary0,um_c != 0) %>% 
  arrange(um_c,desc(control_time)) %>% 
  mutate(Interventions = "AMC")%>%
  .[,c("Interventions","um_c","control_time","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>%
  rename(Intensity = "um_c")

BRR_timing <- subset(result_combind_interventions_control_time_summary0,R_bite != 0) %>% 
  arrange(R_bite,desc(control_time)) %>% 
  mutate(Interventions = "BRR")%>%
  .[,c("Interventions","R_bite","control_time","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>%
  rename(Intensity = "R_bite")

IPS_timing <- subset(result_combind_interventions_control_time_summary0,transmission_time != 7) %>% 
  arrange(desc(transmission_time),desc(control_time)) %>% 
  mutate(Interventions = "IPS")%>%
  .[,c("Interventions","transmission_time","control_time","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>%
  rename(Intensity = "transmission_time")

result_single_interventions_control_time_summary1 <- rbind(LC_timing,
                                                            AMC_timing,
                                                            BRR_timing,
                                                            IPS_timing)

write.xlsx(result_single_interventions_control_time_summary1,"3.result/result_single_interventions_control_time_summary1.xlsx")
# Scenario III --------------------------------------------------------------
#pairwise intervention combinations

control_time
result_data_aquatic_adult <- subset(result_combind_interventions_summary,R_bite==0 & transmission_time ==7 &control_time == control_time)
result_data_aquatic_Rbite <- subset(result_combind_interventions_summary,um_c==0 & transmission_time ==7&control_time == control_time)
result_data_aquatic_transmissiontime <- subset(result_combind_interventions_summary,um_c==0 & R_bite ==0 &control_time == control_time)

result_data_adult_Rbite <- subset(result_combind_interventions_summary,ua_c==0 & transmission_time ==7 & control_time == control_time)
result_data_adult_transmissiontime <- subset(result_combind_interventions_summary,ua_c==0 & R_bite ==0 & control_time == control_time)

result_data_Rbite_transmissiontime <- subset(result_combind_interventions_summary,ua_c==0 & um_c==0 & control_time == control_time)

result_data_Pairwise_combination_summary <- rbind(result_data_aquatic_adult,
                                                  result_data_aquatic_Rbite,
                                                  result_data_aquatic_transmissiontime,
                                                  result_data_adult_Rbite,
                                                  result_data_adult_transmissiontime,
                                                  result_data_Rbite_transmissiontime,
                                                  result_data_Rbite_transmissiontime)
#Supplementary Table
result_data_Pairwise_combination_summary1 <- result_data_Pairwise_combination_summary[,c("ua_c","um_c","R_bite","transmission_time","total_cases_mid","totalcases_reduce_proportion","peak_value","peakvalue_reduce_proportion")] %>% 
  subset(!(ua_c!=0&um_c==0&R_bite==0&transmission_time==7|
           ua_c==0&um_c!=0&R_bite==0&transmission_time==7|
           ua_c==0&um_c==0&R_bite!=0&transmission_time==7|
           ua_c==0&um_c==0&R_bite==0&transmission_time!=7|
           ua_c==0&um_c==0&R_bite==0&transmission_time==7)) %>% 
  .[!duplicated(.), ] %>% 
  mutate(total_cases_mid = round(total_cases_mid,0),
         totalcases_reduce_proportion = round(totalcases_reduce_proportion,1),
         peak_value= round( peak_value,0),
         peakvalue_reduce_proportion = round(peakvalue_reduce_proportion,1)) %>% 
  mutate(total_cases_mid = formatC(total_cases_mid, format = "f", big.mark = ",", digits = 0),
         peak_value = formatC(peak_value, format = "f", big.mark = ",", digits = 0)) 
  
write.xlsx(result_data_Pairwise_combination_summary1,"3.result/result_data_Pairwise_combination_summary1.xlsx")


write.xlsx(result_data_Pairwise_combination_summary,"3.result/result_data_Pairwise_combination_summary.xlsx")
#### scenario III : Pairwise combination measures  plot#
p_Contour_aquatic_adult <- p_Contour_func(data = result_data_aquatic_adult , x = ua_c, y = um_c, title1 = "a",title2 = "g", 
                                          xlab = "LC intensity", ylab = "AMC intensity",
                                          xlabel = seq(0,0.9,0.1),ylabel = seq(0,0.9,0.1))
p_Contour_aquatic_Rbite <- p_Contour_func(data = result_data_aquatic_Rbite , x = ua_c, y = R_bite, title1 = "b",title2 = "h",xlab = "LC intensity", 
                                          ylab = "BRR intensity",xlabel = seq(0,0.9,0.1),ylabel = seq(0,0.9,0.1) )
p_Contour_aquatic_transmissiontime <- p_Contour_func(data = result_data_aquatic_transmissiontime , x = ua_c, y = transmission_time, xlab = "LC intensity", 
                                                     ylab = "IPS intensity", title1 = "c",title2 = "i",
                                                     xlabel = seq(0,0.9,0.1),ylabel = seq(0,7,1))

p_Contour_adult_transmissiontime <- p_Contour_func(data = result_data_adult_transmissiontime , x = um_c, y = transmission_time, 
                                                   xlab = "AMC intensity", ylab = "IPS intensity",
                                                   title1 = "d",title2 = "j",
                                                   xlabel = seq(0,0.9,0.1),ylabel = seq(0,7,1))
p_Contour_adult_Rbite <- p_Contour_func(data = result_data_adult_Rbite , x = um_c, y = R_bite, xlab = "AMC intensity", 
                                        ylab = "BRR intensity",title1 = "e",title2 = "k",
                                        xlabel = seq(0,0.9,0.1),ylabel = seq(0,0.9,0.1))

p_Contour_Rbite_transmission_time <- p_Contour_func(data = result_data_Rbite_transmissiontime , x = R_bite, y = transmission_time,
                                                    xlab = "BRR intensity", ylab = "IPS intensity",title1 = "f",title2 = "l",
                                                    xlabel = seq(0,0.9,0.1),ylabel = seq(0,7,1))

#total
p_Contour_total <- (p_Contour_aquatic_adult[[1]]+p_Contour_aquatic_Rbite[[1]])/(p_Contour_aquatic_transmissiontime[[1]]+p_Contour_adult_transmissiontime[[1]])/(p_Contour_adult_Rbite[[1]] + p_Contour_Rbite_transmission_time[[1]])

ggsave(filename = "3.result/plot fig/Fig. 4(a–f) p_Contour_total.png",p_Contour_total,width = 18,height = 18,dpi = 400,units = "cm")

#peak
p_Contour_peak <- (p_Contour_aquatic_adult[[2]]+p_Contour_aquatic_Rbite[[2]])/(p_Contour_aquatic_transmissiontime[[2]]+p_Contour_adult_transmissiontime[[2]])/(p_Contour_adult_Rbite[[2]] + p_Contour_Rbite_transmission_time[[2]])
ggsave(filename = "3.result/plot fig/Fig. 4(g–l) p_Contour_peak.png",p_Contour_peak,width = 18,height = 18,dpi = 400,units = "cm")

p_Contour <- p_Contour_total |p_Contour_peak
ggsave(filename = "3.result/plot fig/Fig. 4 p_Contour.png",p_Contour,width = 35,height = 17,dpi = 800,units = "cm")

#pairwise intervention combinations --results
##LC -- AMC
#total_cases
set.seed(2025)
data_aquatic_adult <- result_data_aquatic_adult[,c("ua_c","um_c","total_cases_mid")]
names(data_aquatic_adult)[c(1,2)] <- c("x","y")
data <- pivot_wider(data_aquatic_adult,names_from = "y",values_from = "total_cases_mid") %>%
  .[,2:ncol(.)] %>%
  as.matrix()

contour_data <- contourLines(x=seq(0,0.9,0.1),y=seq(0,0.9,0.1), z= matrix(data,nrow = 10), nlevels = 30)

LC_AMC_equivalent_total <- do.call(rbind, lapply(contour_data, function(line) {
  data.frame(x = line$x, y = line$y, level = line$level)
}))

#peak_value
set.seed(2025)
data_aquatic_adult <- result_data_aquatic_adult[,c("ua_c","um_c","peak_value")]
names(data_aquatic_adult)[c(1,2)] <- c("x","y")
data <- pivot_wider(data_aquatic_adult,names_from = "y",values_from = "peak_value") %>%
  .[,2:ncol(.)] %>%
  as.matrix()

contour_data <- contourLines(x=seq(0,0.9,0.1),y=seq(0,0.9,0.1), z= matrix(data,nrow = 10), nlevels = 20)

LC_AMC_equivalent_peak <- do.call(rbind, lapply(contour_data, function(line) {
  data.frame(x = line$x, y = line$y, level = line$level)
}))



# Scenario IV --------------------------------------------------------------
#pairwise intervention combinations + intervention timing

timing <- c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31)

result_combind_interventions_control_time_summary <- c()
for (cont in 1:length(timing)){
  control_time = timing[cont]
  result_combind_interventions <- readRDS(paste0("3.result/result_combind_measures_control_time(",control_time,").rds")) 
  result_combind_interventions_control_time_summary0 <- result_sim3(result_combind_interventions)

  result_combind_interventions_control_time_summary <- rbind(result_combind_interventions_control_time_summary,
                                                             result_combind_interventions_control_time_summary0)
  
  rm(result_combind_interventions)
  
}

result_data_aquatic_adult_control_time_summary <- subset(result_combind_interventions_control_time_summary,R_bite==0 & transmission_time ==7) %>% 
  mutate(facet_label = factor(paste("intervention timing =",control_time),levels = paste("intervention timing =",control_time)))
result_data_aquatic_Rbite_control_time_summary <- subset(result_combind_interventions_control_time_summary,um_c==0 & transmission_time ==7) %>% 
  mutate(facet_label = factor(paste("intervention timing =",control_time),levels = paste("intervention timing =",control_time)))
result_data_aquatic_transmissiontime_control_time_summary <- subset(result_combind_interventions_control_time_summary,um_c==0 & R_bite ==0)%>% 
  mutate(facet_label = factor(paste("intervention timing =",control_time),levels = paste("intervention timing =",control_time)))

result_data_adult_Rbite_control_time_summary <- subset(result_combind_interventions_control_time_summary,ua_c==0 & transmission_time ==7 )%>% 
  mutate(facet_label = factor(paste("intervention timing =",control_time),levels = paste("intervention timing =",control_time)))
result_data_adult_transmissiontime_control_time_summary <- subset(result_combind_interventions_control_time_summary,ua_c==0 & R_bite ==0 )%>% 
  mutate(facet_label = factor(paste("intervention timing =",control_time),levels = paste("intervention timing =",control_time)))

result_data_Rbite_transmissiontime_control_time_summary <- subset(result_combind_interventions_control_time_summary,ua_c==0 & um_c==0 )%>% 
  mutate(facet_label = factor(paste("intervention timing =",control_time),levels = paste("intervention timing =",control_time)))

#
result_data_two_measures_control_time_summary <- rbind(result_data_aquatic_adult_control_time_summary,
                                                       result_data_aquatic_Rbite_control_time_summary,
                                                       result_data_aquatic_transmissiontime_control_time_summary,
                                                       result_data_adult_Rbite_control_time_summary,
                                                       result_data_adult_transmissiontime_control_time_summary,
                                                       result_data_Rbite_transmissiontime_control_time_summary)
write.xlsx(result_data_two_measures_control_time_summary,"3.result/result_data_two_measures_control_time_summary.xlsx")


#pairwise intervention combinations + intervention timing  plot
dir.create("3.result/plot fig/Fig. S two_measures_intervention_time")
p_Contour_aquatic_adult_grid <- p_Contour_func_grid(data = result_data_aquatic_adult_control_time_summary , x = ua_c, y = um_c, 
                                                    xlab = "LC intensity", ylab = "AMC intensity",
                                                    xlabel = seq(0,0.8,0.1),ylabel = seq(0,0.8,0.1))
p_Contour_aquatic_adult_grid_bind <- p_Contour_aquatic_adult_grid[[1]]/p_Contour_aquatic_adult_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S two_measures_intervention_time/Fig. S3 p_Contour_aquatic_adult_grid_bind.png",p_Contour_aquatic_adult_grid_bind,width = 30,height = 25,dpi = 400,units = "in")

p_Contour_aquatic_Rbite_grid <- p_Contour_func_grid(data = result_data_aquatic_Rbite_control_time_summary , xlabel = seq(0,0.8,0.1),ylabel = seq(0,0.8,0.1),x = ua_c, y = R_bite, xlab = "LC intensity", ylab = "BRR intensity" )
p_Contour_aquatic_Rbite_grid_bind <- p_Contour_aquatic_Rbite_grid[[1]]/p_Contour_aquatic_Rbite_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S two_measures_intervention_time/Fig. S4 p_Contour_aquatic_Rbite_grid_bind.png",p_Contour_aquatic_Rbite_grid_bind,width = 30,height = 25,dpi = 400,units = "in")

p_Contour_aquatic_transmissiontime_grid <- p_Contour_func_grid(data = result_data_aquatic_transmissiontime_control_time_summary , 
                                                               x = ua_c, y = transmission_time, xlabel = seq(0,0.8,0.1),ylabel = seq(0,7,1),
                                                               xlab = "LC intensity", ylab = "IPS intensity" )
p_Contour_aquatic_transmissiontime_grid_bind <- p_Contour_aquatic_transmissiontime_grid[[1]]/p_Contour_aquatic_transmissiontime_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S two_measures_intervention_time/Fig. S5 p_Contour_aquatic_transmissiontime_grid_bind.png",p_Contour_aquatic_transmissiontime_grid_bind,width = 30,height = 25,dpi = 400,units = "in")


p_Contour_adult_transmissiontime_grid <- p_Contour_func_grid(data = result_data_adult_transmissiontime_control_time_summary ,xlabel = seq(0,0.8,0.1),ylabel = seq(0,7,1),
                                                             x = um_c, y = transmission_time, xlab = "AMC intensity", ylab = "IPS intensity" )
p_Contour_adult_transmissiontime_grid_bind <- p_Contour_adult_transmissiontime_grid[[1]]/p_Contour_adult_transmissiontime_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S two_measures_intervention_time/Fig. S6 p_Contour_adult_transmissiontime_grid_bind.png",p_Contour_adult_transmissiontime_grid_bind,width = 30,height = 25,dpi = 400,units = "in")

p_Contour_adult_Rbite_grid <- p_Contour_func_grid(data = result_data_adult_Rbite_control_time_summary , xlabel = seq(0,0.8,0.1),ylabel = seq(0,0.8,0.1),
                                                  x = um_c, y = R_bite, xlab = "AMC intensity", ylab = "BRR intensity" )
p_Contour_adult_Rbite_grid_bind <- p_Contour_adult_Rbite_grid[[1]]/p_Contour_adult_Rbite_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S two_measures_intervention_time/Fig. S7 p_Contour_adult_Rbite_grid_bind.png",p_Contour_adult_Rbite_grid_bind,width = 30,height = 25,dpi = 400,units = "in")

p_Contour_Rbite_transmissiontime_grid <- p_Contour_func_grid(data = result_data_Rbite_transmissiontime_control_time_summary , xlabel = seq(0,0.8,0.1),ylabel = seq(0,7,1),
                                                             x = R_bite, y = transmission_time, xlab = "BRR intensity", ylab = "IPS intensity" )
p_Contour_Rbite_transmissiontime_grid_bind <- p_Contour_Rbite_transmissiontime_grid[[1]]/p_Contour_Rbite_transmissiontime_grid[[2]]
ggsave(filename = "3.result/plot fig/Fig. S two_measures_intervention_time/Fig. S8 p_Contour_Rbite_transmissiontime_grid_bind.png",p_Contour_Rbite_transmissiontime_grid_bind,width = 30,height = 25,dpi = 400,units = "in")


# Scenario V --------------------------------------------------------------
control_time = 31
# LC & AMC & BRR
result_data_aquatic_adult_R_bite_summary <- subset(result_combind_interventions_summary, 
                                                   ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&R_bite %in% c(0.2,0.5,0.8) &transmission_time == 7 & control_time == control_time)

# LC & AMC & IPS
result_data_aquatic_adult_transmission_time_summary <- subset(result_combind_interventions_summary,
                                                              ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)& R_bite == 0 & control_time == control_time)

# LC & BRR & IPS
result_data_aquatic_R_bite_transmission_time_summary <- subset(result_combind_interventions_summary,ua_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& um_c == 0 & control_time == control_time)

# AMC & BRR & IPS
result_data_adult_R_bite_transmission_time_summary <- subset(result_combind_interventions_summary,um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& ua_c == 0 & control_time == control_time)

write.xlsx(result_data_aquatic_adult_R_bite_summary,"3.result/result_data_aquatic_adult_R_bite_summary.xlsx")
write.xlsx(result_data_aquatic_adult_transmission_time_summary,"3.result/result_data_aquatic_adult_transmission_time_summary.xlsx")
write.xlsx(result_data_aquatic_R_bite_transmission_time_summary,"3.result/result_data_aquatic_R_bite_transmission_time_summary.xlsx")
write.xlsx(result_data_adult_R_bite_transmission_time_summary,"3.result/result_data_adult_R_bite_transmission_time_summary.xlsx")




# Scenario VI --------------------------------------------------------------
timing <- c(7,14,21,28)

result_combind_interventions_control_time_summary <- c()
for (cont in 1:length(timing)){
  control_time = timing[cont]
  result_combind_interventions <- readRDS(paste0("3.result/result_combind_measures_control_time(",control_time,").rds")) 
  result_combind_interventions_control_time_summary0 <- result_sim3(result_combind_interventions)
  
  result_combind_interventions_control_time_summary <- rbind(result_combind_interventions_control_time_summary,
                                                             result_combind_interventions_control_time_summary0)
  
  rm(result_combind_interventions)
  
}


# LC & AMC & BRR
result_data_aquatic_adult_R_bite_control_time_summary <- subset(result_combind_interventions_control_time_summary, 
                                                                ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&R_bite %in% c(0.2,0.5,0.8) &transmission_time == 7 & control_time %in% c(7,14,21,28)) %>% 
  mutate(x = paste0("(",paste(ua_c,um_c,R_bite,sep = ", "),")"))

# LC & AMC & IPS
result_data_aquatic_adult_transmission_time_control_time_summary <- subset(result_combind_interventions_control_time_summary,
                                                                           ua_c %in% c(0.2,0.5,0.8)&um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)& R_bite == 0 & control_time %in% c(7,14,21,28))%>% 
  mutate(x = paste0("(",paste(ua_c,um_c,transmission_time,sep = ", "),")"))

# LC & BRR & IPS
result_data_aquatic_R_bite_transmission_time_control_time_summary <- subset(result_combind_interventions_control_time_summary,ua_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& um_c == 0 & control_time %in% c(7,14,21,28))%>% 
  mutate(x = paste0("(",paste(ua_c,R_bite,transmission_time,sep = ", "),")"))

# AMC & BRR & IPS
result_data_adult_R_bite_transmission_time_control_time_summary <- subset(result_combind_interventions_control_time_summary,um_c %in% c(0.2,0.5,0.8)&transmission_time %in% c(1,3,5)&R_bite %in% c(0.2,0.5,0.8)& ua_c == 0 & control_time %in% c(7,14,21,28))%>% 
  mutate(x = paste0("(",paste(um_c,R_bite,transmission_time,sep = ", "),")"))


write.xlsx(result_data_aquatic_adult_R_bite_control_time_summary,"3.result/result_data_aquatic_adult_R_bite_control_time_summary.xlsx")
write.xlsx(result_data_aquatic_adult_transmission_time_control_time_summary,"3.result/result_data_aquatic_adult_transmission_time_control_time_summary.xlsx")
write.xlsx(result_data_aquatic_R_bite_transmission_time_control_time_summary,"3.result/result_data_aquatic_R_bite_transmission_time_control_time_summary.xlsx")
write.xlsx(result_data_adult_R_bite_transmission_time_control_time_summary,"3.result/result_data_adult_R_bite_transmission_time_control_time_summary.xlsx")

##plot# 

#cumulative cases
p_data_aquatic_adult_R_bite_control_time_summary<- p_bar_func_total_three(result_data_aquatic_adult_R_bite_control_time_summary,title= "a")
p_data_aquatic_adult_transmission_time_control_time_summary<- p_bar_func_total_three(result_data_aquatic_adult_transmission_time_control_time_summary,title= "b")
p_data_aquatic_R_bite_transmission_time_control_time_summary<- p_bar_func_total_three(result_data_aquatic_R_bite_transmission_time_control_time_summary,title= "c")
p_data_adult_R_bite_transmission_time_control_time_summary<- p_bar_func_total_three(result_data_adult_R_bite_transmission_time_control_time_summary,title= "d",x_lab = "Intervention intensity combination",legend.position = "bottom")

p_three_total_bind <- p_data_aquatic_adult_R_bite_control_time_summary/p_data_aquatic_adult_transmission_time_control_time_summary/p_data_aquatic_R_bite_transmission_time_control_time_summary/p_data_adult_R_bite_transmission_time_control_time_summary

ggsave(filename = "3.result/plot fig/Fig. S p_three_total_bind.png",p_three_total_bind,width = 20,height = 20,dpi = 400,units = "in")

#peak
p_data_aquatic_adult_R_bite_control_time_summary<- p_bar_func_peak_three(result_data_aquatic_adult_R_bite_control_time_summary,title= "a ")
p_data_aquatic_adult_transmission_time_control_time_summary<- p_bar_func_peak_three(result_data_aquatic_adult_transmission_time_control_time_summary,title= "b ")
p_data_aquatic_R_bite_transmission_time_control_time_summary<- p_bar_func_peak_three(result_data_aquatic_R_bite_transmission_time_control_time_summary,title= "c ")
p_data_adult_R_bite_transmission_time_control_time_summary<- p_bar_func_peak_three(result_data_adult_R_bite_transmission_time_control_time_summary,title= "d ",x_lab = "Intervention intensity combination",legend.position = "bottom")

p_three_peak_bind <- p_data_aquatic_adult_R_bite_control_time_summary/p_data_aquatic_adult_transmission_time_control_time_summary/p_data_aquatic_R_bite_transmission_time_control_time_summary/p_data_adult_R_bite_transmission_time_control_time_summary

ggsave(filename = "3.result/plot fig/Fig. S p_three_peak_bind.png",p_three_peak_bind,width = 20,height = 20,dpi = 400,units = "in")



