source("C:/Users/olikc/OneDrive/Documents/R_Projects/PhD_Research/Useful_Functions.R")
##Package_Loading-----------------------
packages <- c("dplyr","stringr","tidyverse","reshape2","minpack.lm","ggprism","ggnewscale","gganimate") ## packages required to get code running

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
####DATA IMPORT AND CLEANUP COD UNIVERSALE----------------------
Raw_Data <- read_single_excel_file()

Raw_Data <- better_read()



Raw_Data_Long <- pivot_longer(Raw_Data,
                              cols = starts_with("Food"),
                              names_to = "Time",
                              names_prefix = "Food_",
                              values_to = "Food_Weight") %>%
  group_by(Mouse_Number) %>%
  mutate(Delta_Food = (Food_Weight - Food_Weight[Time==0])*-1)

Raw_Data_Long <- Raw_Data %>%
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Preinjection_Dirty") %>%
  mutate(Preinjection = gsub(pattern = "\\..*", replacement = "", Preinjection_Dirty),
         Delta_Food = value,
         Time=60,
         Mouse_ID = row_number()
         ) %>%
  select(-c("Preinjection_Dirty","value")) %>%
  na.exclude()

Time_0_Fix <- data.frame(Time = rep(0, nrow(Raw_Data_Long)), Delta_Food =  rep(0,nrow(Raw_Data_Long)), Preinjection = Raw_Data_Long$Preinjection, Drug = Raw_Data_Long$Drug, Mouse_ID = Raw_Data_Long$Mouse_ID)

Raw_Data_Long <- rbind(Raw_Data_Long, Time_0_Fix)


Raw_Data_Long$Time <- as.numeric(Raw_Data_Long$Time) ##Convert first to numeric so R orders according to numeric val
Raw_Data_Long$Time <- as.factor(Raw_Data_Long$Time) ## Then back convert to factor so that each point is automatically discrete in x axis


### GRAPHs ACUTE------------------------

Raw_colour_vector <- c("#075694ff", '#ff2c90ff', 'springgreen1', 'tomato1') ##Select colours for Genotypes here - EXF1_ExD3
Raw_colour_vector <- c('darkgreen', 'Magenta4', 'mediumblue', 'tomato1') ##Select colours for Genotypes here - SNAP_MICE
Darkened_Colours <- darken(Raw_colour_vector,factor = 2)


Raw_Data_Long$Genotype <- factor(Raw_Data_Long$Genotype, c("WT/WT","SNAP/WT","SNAP/SNAP")) ## Make geno a factor

means <- Raw_Data_Long %>% ## Calc group means - doing reorder on raw data will do it row-wise and fuck up
  group_by(Genotype) %>%
  summarize(mean_val = mean(Delta_Food, na.rm = T)) %>%
  arrange(desc(mean_val))

## Raw_Data_Long$Genotype <- factor(Raw_Data_Long$Genotype, levels = means$Genotype) ## reorder based on pre calcd group means - only do this if you mixing data in bar graphs

ggplot(data = Raw_Data_Long, aes(x= Day, y = Delta_Food, color = Preinjection)) + ## ALL ON ONE GRAPH
  scale_fill_manual(values = Raw_colour_vector) +
  scale_color_manual(values = Raw_colour_vector) +
  #geom_line(aes(x= Time, y = Delta_Food, group = Mouse_ID)) + 
  #geom_jitter(aes(x= Time, y = Delta_Food, group = Mouse_ID, shape = Drug), alpha = 0.5, width = 0.2) +
  stat_summary(fun = mean, geom = "line", aes(group = Preinjection), linewidth = 1.2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", aes(color = Preinjection), size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 2) +
  theme(
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
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 19)) +
  scale_x_continuous(breaks = c(0,24,48,72)) + 
  #scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1)) + 
  #coord_cartesian(ylim = c(0, 1))+
  ylab("Cumulative Chow Consumed/g") + 
   xlab("Time/Hr")

    transition_states(
    Genotype,
    transition_length = 2,
    state_length = 2, wrap = F
  ) +
  ease_aes('linear')




 ggplot(data = subset(Raw_Data_Long, Time %in% c(240)), aes(x= Drug, y = Delta_Food)) + ## ALL ON ONE GRAPH
  scale_fill_manual(values = Raw_colour_vector) +
  scale_color_manual(values = Raw_colour_vector, guide = "none") +
  stat_summary(fun = mean, geom = "bar", aes(fill = Genotype),  position = "identity", size = 3, alpha = 0.7) +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+
 # geom_line(aes(x= Time, y = Delta_Food, group = Mouse_ID, color = Genotype), linewidth = 1, alpha = 0.4) + 
#  geom_point(aes(x= Time, y = Delta_Food, group = Mouse_ID, color = Genotype, shape = Genotype), size = 2, alpha = 0.4) +
  stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},  ##Add SEM as error bars - i.e. standard dev/sqrt(n)
               fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
               fun = mean, linewidth = 1,
               geom = "errorbar",
               linetype = "solid",
               width = 0.15,
               aes(group = Genotype, y = Delta_Food, color = Genotype),
               position = position_dodge(width = 0.3)) +
  # stat_summary(fun = mean, geom = "line", aes(group = Genotype, linewidth = 1.2, alpha = 0.5)) +
  #stat_summary(fun = mean, geom = "point", aes(color = Genotype), size = 3) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme(
    panel.grid.major = element_line(color = " light grey"),
    panel.grid.minor =element_line(color = "light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    #axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "12"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 19)) +
  ylab("Chow Consumed/g") +
  geom_jitter(aes(col = Genotype, shape = Drug), width = 0.2, size = 2) +
  facet_wrap(~Genotype)


data_for_food <- Raw_Data_Long %>%
  select(Delta_Food, Drug, Genotype, Sex, Age,Time) %>%
  filter(Time == 240) %>%
  filter(Drug == "Ex4") %>%
  unique()

my_model <- lm(data = data_for_food, Delta_Food ~ Age + Genotype + Sex)

data_for_weight <- Raw_Data_Long %>%
  group_by(Sex, Genotype) %>% 
  mutate(Weight = `Weight _g`) %>%
  select(Weight, Mouse_ID, Sex, Genotype, Age) %>%
  unique()

# Calculate the means
aggregate(Weight ~ Genotype + Sex, data = data_for_weight, FUN = mean)

summarise(data_for_weight)

weight_check_model <- lm(data = data_for_weight, Weight ~ Genotype + Sex + Age)
  
summary(my_model)
summary(weight_check_model)

 # For generating final graph, ben suggests to only include a single timepoint - perhaps 4hr or 8 hr is best
# Linear Model


  





### GRAPHs PRE-INJECT ------------------------


Raw_Data_Long %>%
  mutate(Count_Var = paste(Preinjection, Drug)) %>%
  filter(Time==0) %>%
  ungroup() %>%
  count(Count_Var, name = "count") 


Raw_colour_vector <- c('Slateblue1', 'Magenta1', 'green4') ##Select colours for Genotypes here 
Darkened_Colours <- darken(Raw_colour_vector,factor = 2)

#Raw_Data_Long$Genotype <- factor(Raw_Data_Long$Genotype, c("WT/WT","SNAP/WT","SNAP/SNAP")) ## Make geno a factor - if needed

means <- Raw_Data_Long %>% ## Calc group means - doing reorder on raw data will do it row-wise and fuck up
  group_by(Drug) %>%
  summarize(mean_val = mean(Delta_Food, na.rm = T)) %>%
  arrange(desc(mean_val))

## Raw_Data_Long$Genotype <- factor(Raw_Data_Long$Genotype, levels = means$Genotype) ## reorder based on pre calcd group means - only do this if you mixing data in bar graphs


sub_data <- Raw_Data_Long %>%
  dplyr::filter(Preinjection=="0.05_nm")

ggplot(data = Raw_Data_Long, aes(x= Time, y = Delta_Food, color = Preinjection, linetype = Drug)) + ## ALL ON ONE GRAPH
  scale_fill_manual(values = Raw_colour_vector) +
  scale_color_manual(values = Raw_colour_vector) +
  #geom_line(aes(x= Time, y = Delta_Food, group = Mouse_ID)) + 
  geom_jitter(aes(x= Time, y = Delta_Food, group = Mouse_ID, shape = Drug), alpha = 0.5, width = 0.2) +
  stat_summary(fun = mean, geom = "line", aes(group = interaction(Preinjection,Drug)), linewidth = 1.2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", aes(shape = Drug, color = Preinjection), size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor =element_line(color = "light grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    #axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    #panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "12"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 19)) +
  ylab("Chow Consumed/g") + 
  xlab("Time/min") +
  facet_wrap(~Preinjection)

  
  transition_states(
    Preinjection,
    transition_length = 2,
    state_length = 2, wrap = F
  ) +
  ease_aes('linear')




ggplot(data = subset(Raw_Data_Long, Time %in% c(60)), aes(x= Drug, y = Delta_Food)) + ## ALL ON ONE GRAPH
  scale_fill_manual(values = Raw_colour_vector) +
  scale_color_manual(values = Raw_colour_vector, guide = "none") +
  stat_summary(fun = mean, geom = "bar", aes(fill = Preinjection),  position = "identity", size = 3, alpha = 0.7) +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+
  # geom_line(aes(x= Time, y = Delta_Food, group = Mouse_ID, color = Genotype), linewidth = 1, alpha = 0.4) + 
  #  geom_point(aes(x= Time, y = Delta_Food, group = Mouse_ID, color = Genotype, shape = Genotype), size = 2, alpha = 0.4) +
  stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},  ##Add SEM as error bars - i.e. standard dev/sqrt(n)
               fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
               fun = mean, linewidth = 1,
               geom = "errorbar",
               linetype = "solid",
               width = 0.15,
               aes(group = Preinjection, y = Delta_Food, color = Preinjection),
               position = position_dodge(width = 0.3)) +
  # stat_summary(fun = mean, geom = "line", aes(group = Genotype, linewidth = 1.2, alpha = 0.5)) +
  #stat_summary(fun = mean, geom = "point", aes(color = Genotype), size = 3) +
  # stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor =element_line(color = "light grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    #axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "12"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 19)) +
  ylab("Chow Consumed/g") +
  geom_jitter(aes(col = Preinjection, shape = Drug), width = 0.25, size = 2) +
  facet_wrap(~Preinjection)


data_for_food <- Raw_Data_Long %>%
  select(Delta_Food, Drug, Preinjection, Time) %>%
  filter(Time == 60) %>%
  unique()


data_for_food_drug <- Raw_Data_Long %>%
  select(Delta_Food, Drug, Preinjection, Time) %>%
  filter(Time == 60) %>%
  unique()

Food_Intake_model <- lm(data = data_for_food, Delta_Food ~ Preinjection:Drug)

data_for_weight <- Raw_Data_Long %>%
  mutate(Weight = `Weight_g`) %>%
  select(Weight, Mouse_ID, Preinjection, Drug) %>%
  unique()

# Calculate the means
aggregate(Weight ~ Preinjection + Drug, data = data_for_weight, FUN = mean)

summarise(data_for_weight)

weight_check_model <- lm(data = data_for_weight, Weight ~ Preinjection)

summary(Food_Intake_model)
summary(weight_check_model)

TukeyHSD(Food_Intake_model)

post_hoc <- emmeans(Food_Intake_model, pairwise ~ Preinjection | Drug)

summary(post_hoc)

write.csv(Raw_Data_Long, file = "C:\\Users\\olikc\\Desktop\\PhD_Research\\In_Vivo\\Long_Format_FS.csv")



