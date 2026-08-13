library('dplyr')
library('tidyr')
library('forcats')
library('ggplot2')
library('viridis')
library('readxl')
library('ggnewscale')
source("C:\\Users\\olikc\\OneDrive\\Documents\\R_Projects\\PhD_Research\\Useful_Functions.R")


Data_For_Plot <- Raw_Data %>%
  group_by(Drug_ID) %>%
  mutate(Pan_Average = mean(Mean))

Data_For_Plot_Average <- Data_For_Plot %>%
  select(Drug_ID, Drug, Pan_Average) %>%
  unique()

count(Data_For_Plot, Drug_ID)

Raw_colour_vector <- c("#005493", '#ff2f92','#07d92d') ##Select colours 
Darkened_Colours <- darken(Raw_colour_vector,factor = 1.8)

ggplot(data = Data_For_Plot, aes(x = Drug, y = Mean, color = Drug)) +
  scale_color_manual(values = Raw_colour_vector) +
  geom_violin() + 
  #geom_boxplot(show.legend = F) +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+
  scale_y_continuous(limits=c(12,115),breaks = c(20,40,60,80,100)) +
  geom_jitter(width = 0.20, aes(color = Drug, alpha = 0.3)) +
  #geom_point(aes(color = Drug, alpha = 0.3)) +
  theme(
    axis.text.x = element_text(size = "14"), 
    axis.text.y = element_text(size = "14"),
    axis.title.x = element_blank(),
    legend.key = element_blank(),
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor =element_line(color = "light grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    #axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "16"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
  ) + 
  ylab("Islet Fluorescence Intensity /AU")

My_model <- lm(data = Data_For_Plot, Mean ~ Drug)
summary(My_model)

TukeyHSD(aov(My_model))
