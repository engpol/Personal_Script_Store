library('tidyverse')
library('reshape2')
library('xts')
library('zoo')
library('readxl')
library('purrr')
library('qpcR')
library('ggforce')
library('Hmisc')
library('cowplot')
library('lme4')
library('texreg')
library('DescTools')
library('splines')
library('readxl')
library('ggbeeswarm')


# Set the directory containing the CSV files to append - csv_direction = for labelling experiment, parent directory is the directoy containing all experiments you want to analyse

csv_directory <- "C:/Users/olikc/Desktop/PhD_Research/Hypothalamic_Neuron_Isolations/hGLP-1R/Ex_D3_hGLP_1_Plate_14_1/ResultsTables"
csv_directory_parent <- "C:/Users/olikc/Desktop/PhD_Research/Hypothalamic_Neuron_Isolations/hGLP-1R"


c_csv_drug_and_label <- function(druglabel,platenumber) {
 
  
csv_files <- list.files(path = csv_directory, pattern = "*.csv", full.names = TRUE) ## CREATE A LIST OF THE ALL THE CSV FILES IN THE DESIRED DIRECTORY

##
## AS CBIND.NA WONT WORK WHEN BINDING TO AN EMPTY DATAFRAME, THIS IS A MESSY WORKAROUND WHERE READ THE FIRST CSV IN THE LIST TO WHICH WE WILL MERGE ALL SUBSEQUENT CSV FILES
##

combined_data <- read.csv(csv_files[1])
FOV_number <- combined_data$Label[1]
col_names <- colnames(combined_data)
FOV_col_names <- sapply(col_names, function(x) paste(FOV_number, x, sep = "_")) ##ADDING THE FOV_NUMBER_X TO THE BEGGINING OF ALL COLUMN NAMES SO WE WILL BE ABLE TO IDENTIFY WHICH NEURON AND FOV NUMBER A NEURON CAME FROM
colnames(combined_data) <- FOV_col_names
combined_data <- combined_data[,-2] ##REMOVING LABEL COLUMN
combined_data <- combined_data %>% ##RENAMING X AS TIME
  rename("time" = 1)


csv_files_no_first <- csv_files[2:length(csv_files)] ## JUST SO WE DONT ADD THE FIRST CSV IN THE LIST TWICE

# Loop through each file to read and combine the data

for (file in csv_files_no_first) { ##AS ABOVE BUT LOOPING THROUGH ALL REMAINING CSVS
  temp_data <- read.csv(file)
  FOV_number <- temp_data$Label[1]
  col_names <- colnames(temp_data)
  FOV_col_names <- sapply(col_names, function(x) paste(FOV_number, x, sep = "_"))
  colnames(temp_data) <- FOV_col_names
  temp_data <- temp_data[,-c(1:2)]
  combined_data <- qpcR:::cbind.na(combined_data, temp_data)
}

csv_directory_no_last_path <- str_sub(csv_directory, end = -15) ##Remove the /Results_Table section to save in parent directory


##All of this just removes the Area and Mean from the column names so you can merge following pivoting to long format - - - - - - 

data_fluoresence <- combined_data[, -grep("Area", colnames(combined_data))] 
data_area <- combined_data[, -grep("Mean", colnames(combined_data))]
data_fluoresence_colnames <- gsub("Mean.","",colnames(data_fluoresence))
data_area_colnames <- gsub("Area.","",colnames(data_area))
data_fluoresence_colnames <- gsub("\\.", platenumber, data_fluoresence_colnames)
data_area_colnames <- gsub("\\.", platenumber, data_area_colnames)


colnames(data_fluoresence) <- data_fluoresence_colnames
colnames(data_area) <- data_area_colnames

data_fluoresence_long <- pivot_longer(data_fluoresence, 2:last_col(), 
                                      names_to = "Neuron_ID", 
                                      values_to = "Mean_Intensity")

data_area_long <- pivot_longer(data_area, 2:last_col(), 
                                      names_to = "Neuron_ID", 
                                      values_to = "Area")

data_combined_long <- merge(data_fluoresence_long, data_area_long)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

data_combined_long$Drug <- druglabel ##Add a drug label based on the argument specified in function
write.csv(data_combined_long , paste(csv_directory_no_last_path, "/Results_Conc.csv", sep = ""), row.names = FALSE) ##Save to csv file

return(data_combined_long) ##For checking and further analysis

} ##FUNCTION WHICH ADDS TOGETHER ALL CSVS IN THE FOLDER ABOVE INTO ONE CSV, AND SAVES IT AS A CSV IN THE PARENT (EXPERIMENT) DIRECTORY -
                                                              ##NEED TO ENCLOSE ARGS AS STRINGS!!!!!

raw_data <- c_csv_drug_and_label("ExD3", "_Plate_14") #Put your labels here


c_csv_extracted_data <- function() {

list_of_extracted_csv <- list.files(path = csv_directory_parent, pattern ="Results_Conc.csv", full.names = TRUE, recursive = TRUE) #Fetch all "Results_Conc.csv" files from the parent directory of all experiments

combined_data <- read.csv(list_of_extracted_csv[1]) #Read first csv file in empty array to save making empty dataframe of right dimensions

csv_files_no_first <- list_of_extracted_csv[2:length(list_of_extracted_csv)] ## JUST SO WE DONT ADD THE FIRST CSV IN THE LIST TWICE

for (file in csv_files_no_first) { ##AS ABOVE BUT LOOPING THROUGH ALL REMAINING CSVS
  temp_data <- read.csv(file)
  combined_data <- rbind(combined_data, temp_data)
}

combined_data <- combined_data %>%
  mutate(Plate_number = str_extract(Neuron_ID, "Plate_\\d+"))
return(combined_data)

} #Function to loop and append through all "Results_Conc.csv" files in parent directory


FINAL_RAW_DATA <- c_csv_extracted_data()


## Create data-frame for using in graphs
## Group data by Neuron, baseline ( F0) is calculated as average of intensity before first application
## FDelta is difference of mean intensity from baseline
## FDelta0 is alternative to normalised intensity to avoid normalisation to KCL
## Normalisation is performed through the standard (X - X0/Xmax - X0)
##Control fold change is used for filtering
##AUC will be used for plotting

data_FDelta <- FINAL_RAW_DATA %>%
  group_by(Neuron_ID) %>%
  mutate(F0 = mean(Mean_Intensity[time <= 3], na.rm = TRUE)) %>%
  mutate(FDelta = Mean_Intensity - F0) %>%
  mutate(FDelta0 = FDelta/F0) %>%
  mutate(Normalised_Intensity = FDelta/ (max(Mean_Intensity, na.rm = TRUE) - F0)) %>%
  mutate(Control_Fold_Change = mean(Mean_Intensity[time > 3])/(Mean_Intensity[time == 1])) %>%
  mutate(FSK_Fold_Change = mean(Mean_Intensity[time > 9])/(Mean_Intensity[time == 1])) %>%
  mutate(Pre_Application_Range = (max(Mean_Intensity[time <= 3]) - min(Mean_Intensity[time <= 3]))/F0) %>%
  mutate(before_Application  = AUC(x = time, y = FDelta0, from = (x = 1), to = (x = 3))/2) %>%
  mutate(after_Application  = AUC(x = time, y = FDelta0, from = (x = 3), to = (x = 9))/6) %>%
  mutate(delta_Application = after_Application - before_Application)


##DATA FILTERING - - - - - - - - - - - - - - - - - - - - - - - - - 
## add any filters here to keep dataset seperate from previous to save re-running as with larger data this can take a while


data_filter <- data_FDelta %>%
  group_by(Neuron_ID) %>%
  filter(Control_Fold_Change >= 1.25) %>%
  filter(Control_Fold_Change <= 6) %>%
  filter(FSK_Fold_Change >= 3) %>%
  filter(FSK_Fold_Change <= 7) %>%
  filter(Area <= 1000) %>%
  filter(max(Mean_Intensity[time >= 1 & time <= 9]) < max(Mean_Intensity[time >= 9])) %>% #Filtering out any cases where the maximum intensity is before administration of FSK/Control peak
  filter(max(Normalised_Intensity[time >= 1 & time <= 9]) <= 0.8) %>%
  filter(Pre_Application_Range < 0.1) 
  #filter(Plate_number != "Plate_1") #Filter out weird plates here
  
  

##Counting number of neurons for each condition - - - - - -

data_filter %>%
  filter(time == 1) %>%
  group_by(Drug) %>%
  drop_na() %>%
  count(Drug)



# - - - - - - - - - - - - - - - - - - - - - - -
##GRAPH FOR SINGLE NEURON TRACES - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - -

sample_neurons <- function(n_number) {
sample_neurons_data <- data_filter[,2] %>%
  distinct(Neuron_ID) %>%
  slice_sample(n=1)

sample_neurons_fuck <- sample_neurons_data[sample(nrow(sample_neurons_data), n_number), ]

sample_neurons_fuck_merge <- merge(data_filter, sample_neurons_fuck, by = "Neuron_ID")

return(sample_neurons_fuck_merge)
} #func to sample n number of neurons from filtered data

sample_neuron_data <- sample_neurons(49) ##Choose number of neurons


ggplot(data = sample_neuron_data, aes(x = time, y = FDelta0, color = Drug)) +
  geom_line() +
  geom_vline(xintercept = 3, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title = element_text(size = "20"),
    legend.box.background=element_rect(fill="white", color="black"),
    legend.background = element_blank(),
    #legend.position = c(0.6,0),
    legend.position = "none",
    legend.justification = c("left","bottom"),
    legend.key.width = unit(7, "cm"),
    legend.key.height = unit(0.05, "cm"),
    strip.text = element_text(size = 7),
    legend.text = element_text(size = "15")) +
  
  ylab("Normalised Intensity") +
  xlab("Time (s)") +
  facet_wrap_paginate(~Neuron_ID, ncol = 7, nrow = 7,scales = "free")



# For Looking at All neurons of a drug type  - - - - - - - - - - - - - - - - - - - - - - -


ggplot(data = subset(data_filter, Drug == "Veh" ), aes(x = time, y = FDelta0, color = Drug)) +
  geom_line() +
  geom_vline(xintercept = 3, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title = element_text(size = "20"),
    legend.box.background=element_rect(fill="white", color="black"),
    legend.background = element_blank(),
    #legend.position = c(0.6,0),
    legend.position = "none",
    legend.justification = c("left","bottom"),
    legend.key.width = unit(7, "cm"),
    legend.key.height = unit(0.05, "cm"),
    strip.text = element_text(size = 7),
    legend.text = element_text(size = "15")) +
  
  ylab("Normalised Intensity") +
  xlab("Time (s)") +
  facet_wrap_paginate(~Neuron_ID, ncol = 7, nrow = 7,scales = "free")



# - - - - - - - - - - - - - - - - - - - - - - -
##GRAPH FOR AVERAGE TRACES  - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - -


ggplot(data = subset(data_filter, time < 9), aes(x = time, y = FDelta0, color = Drug)) +
  geom_line(data = data_filter, aes(x = time, y = FDelta0, group = Neuron_ID), alpha = 0.2, color = "grey") +
 # stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
  #             alpha = 1, fill = "light blue", linetype = "solid", fun.args=(conf.int=0.683), color = "black") +
  stat_summary(geom = "line", fun.y = mean, size = 1.2) +
  stat_summary(geom = "point", fun.y = mean, size=3, shape=21, fill="black") +
  #geom_smooth(aes(y=FDelta0), alpha=0.5, color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 3, linetype = "solid", size = 1, color = "red", alpha = 0.25) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
  coord_cartesian(xlim = c(1,8)) + 
  scale_y_continuous(limits = c(-0.25,0.5)) +
  #  geom_hline(data = dat_hline, aes(yintercept = intercept), linetype = "dashed", size = 1, color = "red") +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "20"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
  ylab(expression("\u0394F/F"["0"])) +
  xlab("Time (s)") +
  #coord_cartesian(ylim=c(-0.15,0.40)) + 
  facet_wrap(~Drug, nrow = 1, ncol = 4) 



##IGNORE FOR NOW - - - - -  - - - - - - - - - - - - - - - - - - - -
  geom_text(
    size    = 5,
    data    = dat_text,
    mapping = aes(x = 120, y = -0.04, label = label, color = GFP)) +
  geom_text(
    size    = 4,
    data    = dat_hline,
    mapping = aes(x = 100, y = label + 0.025, label = label),
    color = "red") +
  #  geom_text(
  #   size    = 7,
  #  data    = dat_hline,
  # mapping = aes(x = 300, y = 0.3, label = "*"),
  #  color = "red") +
  #scale_y_continuous(limits = c(-0.1,0.4)) +
  #ggtitle(label = "Average Trace Exd3") +
  facet_wrap_paginate(~VEH_OR_CERITINIB+GFP, nrow = 1, ncol = 4) +
  scale_color_manual(breaks = c("No","Yes"),
                     values=c("black", "green")) 

  ## - - - - - - - - - - - - - - - - - - - -



  # - - - - - - - - - - - - - - - - - - - - - - -
  ##GRAPH FOR BOXPLOTS/VIOLIN PLOTS  - - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - -
  
  
  data_for_boxplots <- data_filter %>%
    group_by(Neuron_ID) %>%
    dplyr::select(before_Application, after_Application, Neuron_ID, delta_Application, Drug) %>% 
    filter(row_number() == 1) %>%
    pivot_longer(before_Application:after_Application, names_to = "Before_or_After", values_to = "Response")
  
  
  data_for_boxplots$Before_or_After <- factor(data_for_boxplots$Before_or_After, levels=c("before_Application", "after_Application"))
  
  
  
  ggplot(data = data_for_boxplots, aes(x = Before_or_After, y = Response, color = Drug)) +
    #geom_boxplot(outlier.shape = NA, fill = "white", geom = "errobar") +
    #geom_jitter(position = position_dodge(width = .75), alpha = 0.3) +
    geom_quasirandom( alpha = 0.15, height = 0, width = 0.25, size = 3, pch = 21, cex = 3, bandwidth = 0.7, dodge.width = 0.25) +
    #geom_line(aes(group = Neuron_ID), color = 'blue', alpha = 0.1) +
    stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},  ##Add SEM as error bars - i.e. standard dev/sqrt(x)
                 fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
                 fun = mean, linewidth = 1, color = "black",
                 geom = "errorbar", 
                 width = 0.64) +
    theme_bw() +
    theme( #legend.background = element_rect(fill = "light grey"), 
      #legend.text = element_text(size = "12"),
      legend.key = element_blank(),
      legend.text = element_blank(),
      legend.position = "none",
      #legend.box.background = element_rect(fill = "black", linewidth = 1),
      legend.box.background = element_blank(),
      axis.text = element_text(size = 14),
      axis.title.y = element_text(size = 20),
      strip.text = element_blank(),
      strip.background = element_blank(),
      panel.background = element_blank(),
      #panel.border = element_blank(),
      panel.grid = element_blank()) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 1, alpha = 0.25) +
    ylab("Average AUC") +
    xlab("") +
    scale_fill_manual(name="GFP",
                      labels=c("No","Yes"),
                      values=c("light gray","green")) +
    scale_x_discrete(labels = c("Baseline","After Application")) +
    facet_wrap(~Drug, nrow = 1) +
    scale_y_continuous(limits = c(-0.1, 0.4)) +
    ggtitle(label = "Change in AUC from 100nM of Drug")
  
  
  # - - - - - - - - - - - - - - - - - - - - - - -
  ##STATISTICS- - - - - - - - - - - - - -
  # - - - - - - - - - - - - - - - - - - - - - - -
  
  
  data_for_stats <- data_filter %>%
  mutate(Experiment_Plate = gsub(pattern = ".*Plate_", replacement = "", Neuron_ID)) %>%
    group_by(Experiment_Plate) %>%
    mutate(Average_Experiment_Delta_AUC = mean(delta_Application)) %>%
    dplyr::select('Experiment_Plate','Drug','Average_Experiment_Delta_AUC') %>%
    filter(Drug != "Veh") %>%
  unique()
    
  
  model_AUC <- lm(data = data_for_stats, Average_Experiment_Delta_AUC ~ Drug)
  
  anova(model_AUC)
  
  anova_model <- aov(data = data_for_stats, Average_Experiment_Delta_AUC ~ Drug)
  
  summary(anova_model)
  
  TukeyHSD(anova_model)
  
  anova(model_AUC)
  
  summary(model_AUC)
  
  summary.lm(model_AUC)
  
  
  data_for_model <- data_for_boxplots %>%    #FOR GLP-1 AGONIST DATA
    filter(Before_or_After == "after_Application") 
  
  model_drug_glm <- glm(data = data_for_model, delta_Application ~ Drug)
  
  model_drug_lm <- lm(data = data_for_model, delta_Application ~ Drug)

  summary(model_drug_lm)
  
  anova(model_drug_lm)
  
  post_hoc_model <- aov(model_drug_lm)
  
  TukeyHSD(post_hoc_model)
