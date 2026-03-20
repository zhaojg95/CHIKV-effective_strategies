
# Baseline scenario -------------------------------------------------------


#### baseline scenario plot ####
p_baseline_plot <- function(data){
  p <- ggplot(data, aes(x = start_date + day-1)) +
    geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.5, fill = "skyblue") +
    scale_color_flatui()+
    geom_line(aes(y = mid/1000),  linewidth = 0.8) +
    labs(y = "Daily cases",
         x = " ") +
    theme_bw()+
    theme(
      axis.text.x = element_text(size = 8, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
      axis.text.y = element_text(size = 8, colour = "black",family = "sans"),
      plot.title = element_text(size = 10, face = "bold"),  
      axis.title.x = element_text(size = 8),  
      axis.title.y = element_text(size = 8),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 8,family = "sans"),  
      legend.position = "", 
      legend.background = element_blank(),   
      legend.key        = element_blank(),   
      strip.text = element_text(size = 8, face = "bold"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank()  
    )+
    scale_x_date(date_labels = "%b %d\n2025", date_breaks = "15 days")+
    scale_y_continuous(labels = function(x) paste0(x, "k"),limits = c(0,600))
  return(p)
}


# Intervention scenarios --------------------------------------------------

#### scenario 1 : single measure plot####
p_line_func <- function(data,title = NULL,x_lab = NULL,legend_title = NULL){
  data$group = as.factor(data[,2])
  p <- ggplot(data, aes(x = start_date+day-1,group = group,colour = group,fill = group)) +
    geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.3,color = NA) +
    scale_color_npg()+
    geom_line(aes(y = mid/1000),  linewidth = 1) +
    labs(title = title,
         y = "Daily cases",
         x = x_lab,
         colour = legend_title,
         fill   = legend_title) +
    theme_bw()+
    theme(
      axis.text.x = element_text(size = 6, colour = "black",family = "sans", vjust = 0.5, hjust = 0.5, angle = 0),
      axis.text.y = element_text(size = 7, colour = "black",family = "sans"),
      plot.title = element_text(size = 8, face = "bold"),  
      axis.title.x = element_text(size = 8),  
      axis.title.y = element_text(size = 8), 
      legend.title = element_text(size = 6), 
      legend.text = element_text(size = 6,family = "sans"),  
      legend.position = c(0.1,0.55),  
      legend.background = element_blank(),   
      legend.key        = element_blank(),   
      legend.key.size = unit(0.25, "cm"),  
      legend.spacing.x = unit(0.1, 'cm'),  
      strip.text = element_text(size = 6, face = "bold"), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()  
    )+
    scale_x_date(date_labels = "%b %d\n2025", date_breaks = "15 days")+
    scale_y_continuous(limits = c(0,620),breaks = c(0,200,400,600),labels = function(x) paste0(x, "k"))
  return(p)
}

p_bar_func_total <- function(data,title = NULL,x_lab = NULL){
  p <- ggplot(data, aes(x = group,y = total_cases_mid / 1000,fill = group)) +
    geom_col(position = position_dodge(width = 0.8),width = 0.75) +
    scale_fill_npg()+
    geom_errorbar(aes(ymin = total_cases_low / 1000,ymax = total_cases_high / 1000),
                  position = position_dodge(width = 0.8),width = 0.3,colour = "black",linewidth = 0.2)+
    labs(title = title,x = x_lab,y = "Cumulative cases") +
    theme_bw()+
    theme(
      axis.ticks.x = element_line(colour = "black", linewidth = 0.2),
      # axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8, colour = "black",family = "sans"),
      plot.title = element_text(size = 8, face = "bold"), 
      axis.text.x  = element_text(size = 6, colour = "black"),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),  
      legend.title = element_text(size = 8), 
      legend.text = element_text(size = 8,family = "sans"),
      legend.position = " ", 
      legend.background = element_blank(),   
      legend.key        = element_blank(),   
      legend.key.size = unit(0.25, "cm"),  
      legend.spacing.x = unit(0.25, 'cm'),  
      strip.text = element_text(size = 8, face = "bold"),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()  
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "k"))
  return(p)
}

# line plot
p_stage_sub_plot <- function(data){
  p <- ggplot(data, aes(x = start_date+day-1 ,group = group,colour = group,fill = group)) +
    geom_ribbon(aes(ymin = low/1000, ymax = high/1000), alpha = 0.2,color = NA) +
    scale_color_manual(values = pal_npg("nrc")(10)[4:10]) +
    scale_fill_manual(values = pal_npg("nrc")(10)[4:10])+
    geom_line(aes(y = mid/1000),  linewidth = 1.5) +
    labs(title = "",
         y = "",
         x = "") +
    theme_bw()+
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 12, colour = "black", vjust = 0.5, hjust = 0.5, angle = 0),
      axis.text.y = element_text(size = 15, colour = "black"),
      plot.title = element_text(size = 18, face = "bold"), 
      axis.title.x = element_text(size = 15), 
      axis.title.y = element_text(size = 15), 
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 15),  
      legend.position = "",
      legend.background = element_blank(),  
      legend.key        = element_blank(), 
      strip.text = element_text(size = 18, face = "bold"), 
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black"),
      plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm")
    )+
    scale_x_date(date_labels = "%b %d\n2025", date_breaks = "30 days")+
    scale_y_continuous(labels = function(x) paste0(x, "k"))
  return(p)
}

#bar plot
p_adult_stage_sub_bar_plot <- function(data){
  p <- ggplot(result_foshan_data_single_measure_adult_stage_sub_summary, aes(x = factor(control_time),y = total_cases_mid / 1000,fill = group)) +
    geom_col(position = position_dodge(width = 0.8),width = 0.75) +
    scale_color_manual(values = pal_npg("nrc")(10)[4:10]) +
    scale_fill_manual(values = pal_npg("nrc")(10)[4:10])+
    geom_errorbar(aes(ymin = total_cases_low / 1000,ymax = total_cases_high / 1000),
                  position = position_dodge(width = 0.8),width = 0.3,colour = "black",linewidth = 0.2)+
    labs(title = "",x = "",y = "") +
    theme_bw()+
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 15, colour = "black"),
      plot.title = element_text(size = 18, face = "bold"),  
      axis.title.x = element_text(size = 15),  
      axis.title.y = element_text(size = 15),  
      legend.title = element_text(size = 16),  
      legend.text = element_text(size = 15), 
      legend.position = "",  
      legend.background = element_blank(),  
      legend.key        = element_blank(),  
      strip.text = element_text(size = 18, face = "bold"),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()  
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "k"))
  return(p)
}


#boxplot
single_boxplot_func <- function(data,col = 1,xlab= NULL,title = NULL){
  
  names(data)[col] <- "group"
  pp <- data %>% 
    pivot_longer(cols = !1:5, names_to = "each_run", values_to = "total_cases") %>% 
    mutate(group = as.factor(group)) %>% 
    ggplot(aes(x = group, y = total_cases/1000,group = group,fill = group)) +
    
    # geom_jitter(
    #   position = position_jitterdodge(jitter.width = 1, dodge.width = 0.8),
    #   size = 0.001,alpha = 0.2)+
    # facet_wrap(.~group, scales = "free_y",
    #            labeller = labeller(group = function(x) paste0("group = ", x))
    # )+
    geom_boxplot(position = position_dodge(0.8), linewidth = 0.3,width = 0.8, alpha = 0.8,outlier.size = 0.3) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.3,      
      linewidth = 0.25
    )+
    scale_fill_npg()+
    theme_bw() +
    theme(
      strip.text = element_text(size = 8, face = "bold", colour = "black"), 
      axis.ticks.x = element_line(colour = "black", linewidth = 0.2),
      axis.ticks.y = element_line(colour = "black", linewidth = 0.2),
      axis.text.x  = element_text(size = 6, colour = "black"),
      axis.text.y  = element_text(size = 8, colour = "black"),
      plot.title   = element_text(size = 8, face = "bold"),
      axis.title.x = element_text(size = 8),
      axis.title.y = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 8),
      legend.position = "none",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.25, "cm"),
      legend.spacing.x = unit(0.25, 'cm'),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),  
      # panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )  +
    # scale_y_continuous(labels = function(x) paste0(x, "k")) + 
    scale_y_continuous(limits = c(0,8500),labels = function(x) paste0(round(x,0), "k")) + 
    labs(title = title,x = xlab,y = "Cumulative cases")
  
}

## scenario 2 : single measure & initial intervention time plot####
#cumulative cases
# p_bar_func_total <- function(data,title = NULL,x_lab = NULL, legend.position = "right"){
#   p <- ggplot(data, aes(x = factor(control_time),y = total_cases_mid / 1000,fill = group)) +
#     geom_col(position = position_dodge(width = 0.8),width = 0.7) +
#     scale_fill_npg()+
#     geom_errorbar(aes(ymin = total_cases_low / 1000,ymax = total_cases_high / 1000),
#                   position = position_dodge(width = 0.8),width = 0.1,colour = "black",linewidth = 0.1)+
#     labs(title = title,x = x_lab,y = "Cumulative cases") +
#     theme_bw()+
#     theme(
#       axis.ticks.x = element_line(colour = "black", linewidth = 0.2),  
#       axis.ticks.y = element_line(colour = "black", linewidth = 0.2),   
#       axis.text.x = element_text(size = 6, colour = "black", vjust = 0.5, hjust = 0.5, angle = 0),
#       axis.text.y = element_text(size = 6, colour = "black"),
#       plot.title = element_text(size = 6, face = "bold"),  
#       axis.title.x = element_text(size = 6),  
#       axis.title.y = element_text(size = 6),  
#       legend.title = element_text(size = 6),  
#       legend.text = element_text(size = 5),  
#       legend.position = legend.position,  
#       legend.background = element_blank(),  
#       legend.key        = element_blank(),  
#       legend.key.size = unit(0.15, "cm"), 
#       legend.spacing.x = unit(0, 'cm'), 
#       strip.text = element_text(size = 4, face = "bold"), 
#       panel.border = element_rect(colour = "black", fill=NA, size=0.2),
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(),  
#       plot.margin = unit(c(0.01, 0, 0, 0), "cm")  
#     ) +
#     scale_y_continuous(labels = function(x) paste0(x, "k"))+
#     guides(fill = guide_legend(ncol = 1))
#   return(p)
# }

# #peak value
# p_bar_func_peak <- function(data,title = NULL,x_lab = NULL, legend.position = "right"){
#   p <- ggplot(data, aes(x = factor(control_time),y = peak_value / 1000,fill = group)) +
#     geom_col(position = position_dodge(width = 0.8),width = 0.75) +
#     scale_fill_npg()+
#     geom_errorbar(aes(ymin = peak_value_low / 1000,ymax = peak_value_high / 1000),
#                   position = position_dodge(width = 0.8),width = 0.1,colour = "black",linewidth = 0.1)+
#     labs(title = title,x = x_lab,y = "Peak value") +
#     theme_bw()+
#     theme(
#       axis.ticks.x = element_line(colour = "black", linewidth = 0.2), 
#       axis.ticks.y = element_line(colour = "black", linewidth = 0.2),   
#       axis.text.x = element_text(size = 6, colour = "black", vjust = 0.5, hjust = 0.5, angle = 0),
#       axis.text.y = element_text(size = 6, colour = "black"),
#       plot.title = element_text(size = 6, face = "bold"),  
#       axis.title.x = element_text(size = 6),  
#       axis.title.y = element_text(size = 6),  
#       legend.title = element_text(size = 6), 
#       legend.text = element_text(size = 5),  
#       legend.position = legend.position,  
#       legend.background = element_blank(),  
#       legend.key        = element_blank(), 
#       legend.key.size = unit(0.15, "cm"),  
#       legend.spacing.x = unit(0, 'cm'),  
#       strip.text = element_text(size = 4, face = "bold"),  
#       panel.border = element_rect(colour = "black", fill=NA, size=0.2),
#       panel.grid.major = element_blank(),  
#       panel.grid.minor = element_blank(),  
#       plot.margin = unit(c(0.01, 0, 0, 0), "cm")  
#     ) +
#     scale_y_continuous(labels = function(x) paste0(x, "k"))
#   
#   return(p)
# }

#
plot_func_total <- function(data, x_lab = "",title = "a",group0 = c(0.2,0.5,0.8)){
  
  pp <- data %>% 
    subset(group %in%group0 & control_time%in% c(7,14,21,28) ) %>% 
    pivot_longer(cols = !1:5, names_to = "each_run", values_to = "total_cases") %>% 
    ggplot(aes(x = factor(control_time), y = total_cases/1000, fill = factor(control_time))) +
    
    geom_jitter(
      position = position_jitterdodge(jitter.width = 1, dodge.width = 0.8),
      size = 0.001,alpha = 0.2)+
    facet_wrap(.~group, scales = "free_y",
               labeller = labeller(group = function(x) paste0("Intensity = ", x))
    )+
    geom_boxplot(position = position_dodge(0.8), linewidth = 0.3,width = 0.8, alpha = 0.8,outlier.size = 0.3) +
    stat_boxplot(
      geom = "errorbar",
      width = 0.3,      
      linewidth = 0.25
    )+
    scale_fill_npg()+
    theme_bw() +
    theme(
      strip.background = element_rect(
        fill = "#F4D1C7",      
        colour = "black",        
        size = 0.3               
      ),
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
      legend.position = "none",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.15, "cm"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0.01, 0.01, 0.1, 0.1), "cm")
    )  +
    scale_y_continuous(labels = function(x) paste0(round(x,0), "k")) + 
    labs(title = title,x = x_lab,y = "Cumulative cases") 
  return(pp)
}


plot_func_peak <- function(data, x_lab = "",title = "",group0 = c(0.2,0.5,0.8)){
  
  pp <- data %>%
    subset(group %in%group0 & control_time%in% c(7,14,21,28) ) %>%
    pivot_longer(cols = !1:5, names_to = "each_run", values_to = "peak_cases") %>%
    ggplot(aes(x = factor(control_time), y = round(peak_cases/1000,1), fill = factor(control_time))) +
    
    geom_jitter(
      position = position_jitterdodge(jitter.width = 1, dodge.width = 0.8),
      size = 0.001,alpha = 0.2)+
    facet_wrap(.~group, scales = "free_y",
               labeller = labeller(group = function(x) paste0("Intensity = ", x))
    )+
    geom_violin(position = position_dodge(0.8), width = 0.5, alpha = 0.8) +
    stat_summary(
      fun.data = function(x){
        data.frame(y = median(x),
                   ymin = quantile(x, 0.25),
                   ymax = quantile(x, 0.75))
      },
      geom = "pointrange",
      position = position_dodge(0.8),
      size = 0.1,
      colour = "black",
      alpha = 0.5
    )+
    scale_fill_npg()+
    theme_bw() +
    theme(
      strip.background = element_rect(
        fill = "#C5D5EA",     
        colour = "black",      
        size = 0.3              
      ),
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
      legend.position = "none",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.15, "cm"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(0.01, 0.01, 0.1, 0.1), "cm")
    )  +
    scale_y_continuous(labels = function(x) paste0(x, "k")) +
    labs(title = title,x = x_lab,y = "Peak value")
  return(pp)
}


#### scenario 3 : Pairwise combination measures  plot####

p_Contour_func <- function(data, x, y ,xlab, ylab,title1 = NULL, title2 = NULL,xlabel, ylabel){
  
  Contour_p_data_total <- 
    ggplot(data,aes(x = {{x}},y = {{y}},z = total_cases_mid/1000))+
    geom_contour_fill(bins = 30) + 
    geom_contour(colour = "black", alpha = 0.3, bins = 12) +
    scale_fill_gradientn(colours = c("#3C5488FF", "#8491B4FF", "grey100", "#F39B7FFF", "#E64B35FF"),
                         na.value = "grey90",
                         name = "Cumulative cases",
                         labels = function(x) paste0(x, "k")) +
    theme_bw() +
    theme(
      axis.ticks      = element_line(colour = "black", linewidth = 0.2), 
      axis.ticks.length = unit(1, "mm"), 
      axis.text.x = element_text(size = 8, colour = "black"),
      axis.text.y = element_text(size = 8, colour = "black"),
      plot.title  = element_text(size = 9, face = "bold"),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 7),
      legend.position = "right",        
      legend.key.size = unit(0.8, "cm"),
      legend.key.width = unit(0.3, "cm"),
      legend.spacing.x = unit(0, "cm"),
      strip.text = element_text(size = 6, face = "bold"),
      panel.border   = element_blank(),            
      plot.background = element_blank(),         
      plot.margin = unit(c(0.01, 0, 0, 0), "cm")  
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),breaks = xlabel, labels = xlabel) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)),breaks = ylabel, labels = ylabel) +
    labs(x = xlab, y = ylab,title = title1)
  
  Contour_p_data_peak <- 
    ggplot(data,aes(x = {{x}},y = {{y}},z = peak_value/1000))+
    geom_contour_fill(bins = 30) + 
    geom_contour(colour = "black", alpha = 0.3, bins = 12) +
    scale_fill_gradientn(colours = c("#3C5488FF", "#8491B4FF", "grey100", "#F39B7FFF", "#E64B35FF"),
                         na.value = "grey90",
                         name = "Peak value",
                         labels = function(x) paste0(x, "k")) +
    theme_bw() +
    theme(
      axis.ticks      = element_line(colour = "black", linewidth = 0.2),  
      axis.ticks.length = unit(1, "mm"), 
      axis.text.x = element_text(size = 8, colour = "black"),
      axis.text.y = element_text(size = 8, colour = "black"),
      plot.title  = element_text(size = 9, face = "bold"),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text  = element_text(size = 7),
      legend.position = "right",        
      legend.key.size = unit(0.8, "cm"),
      legend.key.width = unit(0.3, "cm"),
      legend.spacing.x = unit(0, "cm"),
      strip.text = element_text(size = 6, face = "bold"),
      panel.border   = element_blank(),            
      plot.background = element_blank(),         
      plot.margin = unit(c(0.01, 0, 0, 0), "cm")  
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),breaks = xlabel, labels = xlabel) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)),breaks = ylabel, labels = ylabel) +
    labs(x = xlab, y = ylab,title = title2)
  
  list(Contour_p_data_total,Contour_p_data_peak)
}

#### scenario 4 : Pairwise combination measures & initial intervention time plot####
p_Contour_func_grid <- function(data, x, y ,xlab, ylab,xlabel, ylabel){
  
  Contour_p_data_total <- 
    ggplot(data,aes(x = {{x}},y = {{y}},z = total_cases_mid/1000))+
    geom_contour_fill(bins = 20) + 
    geom_contour(colour = "black", alpha = 0.4, bins = 12) +
    scale_fill_gradientn(colours = c("#3C5488FF", "#8491B4FF", "grey100", "#F39B7FFF", "#E64B35FF"),
                         na.value = "grey90",
                         name = "Cumulative cases",
                         labels = function(x) paste0(x, "k")) +
    facet_wrap(~ facet_label, ncol = 5) +
    theme_bw() +
    theme(
      axis.ticks      = element_line(colour = "black", linewidth = 1),  
      axis.ticks.length = unit(13, "pt"), 
      axis.text.x = element_text(size = 25, colour = "black"),
      axis.text.y = element_text(size = 25, colour = "black"),
      plot.title  = element_text(size = 36, face = "bold"),
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.text  = element_text(size = 25),
      legend.position = "right",        
      legend.key.size = unit(3, "cm"),
      legend.key.width = unit(1.1, "cm"),
      legend.spacing.x = unit(1.1, "cm"),
      strip.text = element_text(size = 20, face = "bold"),
      panel.border   = element_blank(),            
      plot.background = element_blank(),         
      plot.margin = unit(rep(0, 4), "cm")        
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),breaks = xlabel, labels = xlabel) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)),breaks = ylabel, labels = ylabel) +
    labs(title = "a",x = xlab, y = ylab)
  
  Contour_p_data_peak <- 
    ggplot(data,aes(x = {{x}},y = {{y}},z = peak_value/1000))+
    geom_contour_fill(bins = 20) + 
    geom_contour(colour = "black", alpha = 0.4, bins = 12) +
    scale_fill_gradientn(colours = c("#3C5488FF", "#8491B4FF", "grey100", "#F39B7FFF", "#E64B35FF"),
                         na.value = "grey90",
                         name = "Peak value",
                         labels = function(x) paste0(x, "k")) +
    facet_wrap(~ facet_label, ncol = 5) +
    theme_bw() +
    theme(
      axis.ticks      = element_line(colour = "black", linewidth = 1), 
      axis.ticks.length = unit(13, "pt"), 
      axis.text.x = element_text(size = 25, colour = "black"),
      axis.text.y = element_text(size = 25, colour = "black"),
      plot.title  = element_text(size = 36, face = "bold"),
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.text  = element_text(size = 25),
      legend.position = "right",        
      legend.key.size = unit(3, "cm"),
      legend.key.width = unit(1.1, "cm"),
      legend.spacing.x = unit(1.1, "cm"),
      strip.text = element_text(size = 20, face = "bold"),
      panel.border   = element_blank(),            
      plot.background = element_blank(),         
      plot.margin = unit(rep(0, 4), "cm")         
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),breaks = xlabel, labels = xlabel) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)),breaks = ylabel, labels = ylabel) +
    labs(title = "b",x = xlab, y = ylab)
  
  list(Contour_p_data_total,Contour_p_data_peak)
}

#### scenario 6 : Combination of three intervention measures & initial intervention time####
#cumulative cases
p_bar_func_total_three <- function(data,title = NULL,x_lab = NULL,legend.position = "none"){
  p <- ggplot(data, aes(x = factor(x),y = total_cases_mid / 1000,fill = factor(control_time))) +
    geom_col(position = position_dodge(width = 0.8),width = 0.75) +
    scale_fill_manual(values = pal_npg("nrc")(10)[c(5,2,3,4)])+
    geom_errorbar(aes(ymin = total_cases_low / 1000,ymax = total_cases_high / 1000),
                  position = position_dodge(width = 0.8),width = 0.3,colour = "black")+
    labs(title = title,x = x_lab,y = "Cumulative cases",fill = "Intervention timings") +
    theme_bw()+
    theme(
      axis.text.x = element_text(size = 15, colour = "black", vjust = 0.5, hjust = 0.5, angle = 45),
      axis.text.y = element_text(size = 20, colour = "black"),
      plot.title = element_text(size = 25, face = "bold"),  
      axis.title.x = element_text(size = 20),  
      axis.title.y = element_text(size = 20),  
      legend.title = element_text(size = 20),  
      legend.text = element_text(size = 20), 
      legend.position = legend.position, 
      legend.background = element_blank(),  
      legend.key        = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 18, face = "bold")  
    )+geom_hline(yintercept = 9699/1000, linetype = "dashed", color = "red")+
    scale_y_continuous(labels = function(x) paste0(x, "k"))
  return(p)
}

#peak 
p_bar_func_peak_three <- function(data,title = NULL,x_lab = NULL,legend.position = "none"){
  p <- ggplot(data, aes(x = factor(x),y = peak_value ,fill = factor(control_time))) +
    geom_col(position = position_dodge(width = 0.8),width = 0.75) +
    scale_fill_manual(values = pal_npg("nrc")(10)[c(6,7,8,9)])+
    geom_errorbar(aes(ymin = peak_value_low,ymax = peak_value_high),
                  position = position_dodge(width = 0.8),width = 0.3,colour = "black")+
    labs(title = title,x = x_lab,y = "Peak value",fill = "Intervention timings") +
    theme_bw()+
    theme(
      axis.text.x = element_text(size = 15, colour = "black", vjust = 0.5, hjust = 0.5, angle = 45),
      axis.text.y = element_text(size = 20, colour = "black"),
      plot.title = element_text(size = 25, face = "bold"),
      axis.title.x = element_text(size = 20),  
      axis.title.y = element_text(size = 20), 
      legend.title = element_text(size = 20),  
      legend.text = element_text(size = 20), 
      legend.position = legend.position,  
      legend.background = element_blank(),   
      legend.key        = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 18, face = "bold") 
    )+
    geom_hline(yintercept = 969, linetype = "dashed", color = "blue")
  
  return(p)
}





