library('tidyverse') ##make sure to declare dplyr::select!
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
library('texreg') ##Power analysis
library('DescTools')
library('splines') #Dont think u nead this
library('readxl')
library('ggbeeswarm')


##MASTER CODE FOR ANALYSING ALL 96-WELL PLATE EXPERIMENTS

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, fun.min = min, fun.max = max, colour = "black", geom = geom, width = 0.2, ...)
} ##Function for generating group-based boxplots - should be included in hmisc package, however doesn't work for me for some reason

##CHOOSE FOLDER CONTAINING ALL YOUR EXPERIMENT CSV FILES YOU'd LIKE TO ANALYSE

data_import <- function(){

exfolder <- tcltk::tk_choose.dir(default = "~/")
list_of_extracted_csv <- list.files(path = exfolder, full.names = TRUE, recursive = TRUE) #Fetch all "Results_Conc.csv" files from the parent directory of all experiments

combined_data <- read.csv(list_of_extracted_csv[1]) #Read first csv file in empty array to save making empty dataframe of right dimensions

if(length(list_of_extracted_csv) > 1) {

csv_files_no_first <- list_of_extracted_csv[2:length(list_of_extracted_csv)] ## JUST SO WE DONT ADD THE FIRST CSV IN THE LIST TWICE

for (file in csv_files_no_first) { ##AS ABOVE BUT LOOPING THROUGH ALL REMAINING CSVS
  temp_data <- read.csv(file)
  combined_data <- rbind(combined_data, temp_data)
}
}
return(combined_data)

}

read_data <- data_import()

read_data_filtered <- read_data %>% ##Very simple filtering - in theory can be run on everything
  filter(Channel_Name != "Brightfield",
         Channel_Name != "FRET") %>% ##Remove BF channel - don't care about this one 
  mutate(key = paste0(Well_ID,Channel_Name,Expt_Name, sep = "")) %>% ##This and the 3 lines below check for any duplicate rows - i.e. concat several conditions into 1 string then check if string is original
  distinct(key, .keep_all = TRUE) %>%
  dplyr::select(-(key))

read_data_filtered[is.na(read_data_filtered)] <- 0

read_data_filtered <- read_data_filtered %>% 
  filter(Expt_Name == "20_08_DNA_Lipo_Experiment")  

read_data_filtered2 <- read_data_filtered %>% ##If only correcting one channel
  mutate(Average_Cell_Intensity_Well_Correct = Average_Cell_Intensity_Well - 536)

##Stupid SHit code for stupid shit 25_09 experiment to correct for GFP-------




##Specific code for Optimisation experiments------------------------------------------------------------------------------------------------------------------
ggplot(data = read_data_filtered_GFP_Corrected, aes(x = Plasmid, y = Average_Cell_Intensity_Well, fill = Channel_Name)) +
  stat_sum_df("mean_cl_boot") +
  stat_summary(fun = mean, fun.min = min, geom = "errorbar", width = 0.35, fun.max = max, colour = "black") +
  geom_point(stat ='identity', colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
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
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
  scale_fill_manual(breaks = c("GFP","SNAP","Tetrazine"),
                    values=c("Green", "gold2","slateblue1")) +
  ylab(expression("Mean Fluorescence Intensity (AU)")) +
  xlab("") +
  facet_wrap(~Amino_Acid + Channel_Name, nrow = 1)


facet_wrap_paginate(~Expt_Name + DNA_Conc + AminoAcid + Lipo_Conc, nrow = 1, ncol = 13, page = 2) 

## - - - - - - - - - - - -

read_data_filtered_only_GFP <- read_data_filtered %>%
  filter(Channel_Name == "GFP") %>%
  mutate(Average_Cell_Intensity_Well = Average_Cell_Intensity_Well - 1000)

read_data_filtered_only_GFP[read_data_filtered_only_GFP < 0] <- 0

read_data_filtered_no_GFP <-  read_data_filtered %>%
  filter(Channel_Name != "GFP")

read_data_filtered_GFP_Corrected <- rbind(read_data_filtered_only_GFP,read_data_filtered_no_GFP)


##Specific code for Tetra Labelling experiments------------------------------------------------------------------------------------------------------------------


read_data_filtered$Amino_Acid <- factor(read_data_filtered$Amino_Acid, levels = c("None","BCNK","TCOA"))

plot_for_loop <- ggplot(data = loop_data, aes(x = Channel_Name, y = Mean_Average_Cell_Intensity, fill = Channel_Name)) +
  #stat_sum_df("mean_cl_boot") +
  geom_boxplot() +
  geom_point(stat ='identity', colour = "black") +
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
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text = element_text(size = 17),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
  ylim(0,750) +
  scale_fill_manual(breaks = c("GFP","Tetrazine"),
                    values=c("Green", "Red")) +
  ylab(expression("Mean Fluorescence Intensity")) +
  xlab("") +
  facet_wrap(~Plasmid + Amino_Acid, nrow = 1) 
  
  
  plasmids_vector <- unique(read_data_filtered$Plasmid)
    
  
  
  for (plasmid in plasmids_vector) {
    loop_data <- read_data_filtered %>%
      filter(Plasmid == plasmid)
    
    plot_for_loop <- ggplot(data = loop_data, aes(x = Channel_Name, y = Mean_Average_Cell_Intensity, fill = Channel_Name)) +
      #stat_sum_df("mean_cl_boot") +
      geom_boxplot() +
      geom_point(stat ='identity', colour = "black") +
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
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 17),
        plot.title = element_text(hjust = 0.5, size = 19),
        legend.position = "none") +
      ylim(0,750) +
      scale_fill_manual(breaks = c("GFP","Tetrazine"),
                        values=c("Green", "Red")) +
      ylab(expression("Mean Fluorescence Intensity")) +
      xlab("") +
      facet_wrap(~Plasmid + Amino_Acid, nrow = 1) 
    
    print(plot_for_loop)
  }
  
  

##---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
















#