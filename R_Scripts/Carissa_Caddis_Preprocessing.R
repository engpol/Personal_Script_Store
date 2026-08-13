##Package_Loading-----------------------
packages <- c("dplyr","stringr","tidyverse","ggplot2") ## packages required to get code running

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##Functions-------
better_read <- function(){
  chosen_data <- tcltk::tk_choose.files()
  chosen_data_read <- read.csv(chosen_data)
  return(chosen_data_read)
}
source("C:\\Users\\olikc\\OneDrive\\Documents\\R_Projects\\PhD_Research\\Useful_Functions.R")


##Code----------


read_data <- better_read() ## This will ask you to choose the csv file you want to read


data_fluoresence_long <- pivot_longer(read_data, 6:last_col(), 
                                      names_to = "Neuron_ID", 
                                      values_to = "Mean_Intensity")

data_fluoresence_long$Timepoint <- as.integer(data_fluoresence_long$Timepoint)

cADDIS_Up_Channel_String <- "TRITC" ## PUT THE NAME OF THE CHANNEL HAVING CADDIS UP HERE - Leave blank if not used

cADDIS_Down_Channel_String <- "GFP" ## PUT THE NAME OF THE CHANNEL HAVING CADDIS DOWN HERE - Leave blank if not used

Channel_Fold_Change_Filter <- 1.5  ## Change here if you'd like to use a different filtering number


if(cADDIS_Down_Channel_String != "") {

data_fluorescence_cADDIS_Down <- data_fluoresence_long %>%
  filter(Channel_Name == cADDIS_Down_Channel_String) %>%
  mutate(Grouping_Var = as.factor(paste0(Well_ID,Label,Neuron_ID)))

timepoint_length_cADDIS_Down <- length(unique(data_fluorescence_cADDIS_Down$Timepoint))

data_filtered_cADDIS_Down <- data_fluorescence_cADDIS_Down %>%
  group_by(Grouping_Var) %>%
  mutate(Max_Intensity = Mean_Intensity[Timepoint = timepoint_length_cADDIS_Down],
         Min_Intensity = Mean_Intensity[Timepoint = 1],
         Fold_Change = Min_Intensity/Max_Intensity) %>%
  filter(Fold_Change > Channel_Fold_Change_Filter) %>%
  ungroup() %>%
  select(-c("Min_Intensity","Max_Intensity","Fold_Change","Grouping_Var"))
  

data_filtered_cADDIS_Down_wide <- pivot_wider(data = data_filtered_cADDIS_Down, names_from = Neuron_ID, values_from = Mean_Intensity, names_sort = T)

data_filtered_cADDIS_Down_wide$Timepoint <- as.numeric(as.character(data_filtered_cADDIS_Down_wide$Timepoint))

data_filtered_cADDIS_Down_wide <- data_filtered_cADDIS_Down_wide[order(data_filtered_cADDIS_Down_wide$Timepoint),]
data_filtered_cADDIS_Down_wide <- data_filtered_cADDIS_Down_wide[str_order(data_filtered_cADDIS_Down_wide$Well_ID),]
data_filtered_cADDIS_Down_wide <- data_filtered_cADDIS_Down_wide[str_order(data_filtered_cADDIS_Down_wide$Label),]

}

if(cADDIS_Up_Channel_String != "") {

data_fluorescence_cADDIS_Up <- data_fluoresence_long %>%
  filter(Channel_Name == cADDIS_Up_Channel_String) %>%
  mutate(Grouping_Var = paste0(Well_ID,Label,Neuron_ID))

timepoint_length_cADDIS_Up <- length(unique(data_fluorescence_cADDIS_Up$Timepoint)) ## This might not work on all excel files - will need to check wtf is going on

data_filtered_cADDIS_up <- data_fluorescence_cADDIS_Up %>%
  group_by(Grouping_Var) %>%
  mutate(Max_Intensity = Mean_Intensity[Timepoint = timepoint_length_cADDIS_Up],
         Min_Intensity = Mean_Intensity[Timepoint = 1],
         Fold_Change = Max_Intensity/Min_Intensity) %>%
  filter(Fold_Change > Channel_Fold_Change_Filter) %>%
  ungroup() %>%
  select(-c("Min_Intensity","Max_Intensity","Fold_Change","Grouping_Var"))

data_filtered_cADDIS_up_wide <- pivot_wider(data = data_filtered_cADDIS_up, names_from = Neuron_ID, values_from = Mean_Intensity,  names_sort = T)

data_filtered_cADDIS_up_wide$Timepoint <- as.numeric(as.character(data_filtered_cADDIS_up_wide$Timepoint))

data_filtered_cADDIS_up_wide <- data_filtered_cADDIS_up_wide[order(data_filtered_cADDIS_up_wide$Timepoint),]
data_filtered_cADDIS_up_wide <- data_filtered_cADDIS_up_wide[str_order(data_filtered_cADDIS_up_wide$Well_ID),]
data_filtered_cADDIS_up_wide <- data_filtered_cADDIS_up_wide[str_order(data_filtered_cADDIS_up_wide$Label),]


}


 exfolder <- tcltk::tk_choose.dir(default = "~/")
 
 write.csv(data_filtered_cADDIS_up_wide , paste(exfolder, "/Caddis_Up.csv", sep = ""), row.names = FALSE) ##Save anotated data as .csv file
 
 write.csv(data_filtered_cADDIS_Down_wide , paste(exfolder, "/Caddis_Down.csv", sep = ""), row.names = FALSE) ##Save anotated data as .csv file
 


 ggplot(data = data_filtered_cADDIS, aes(x = Timepoint, y = Mean_Intensity)) +
   geom_line() +
   geom_vline(xintercept = 3, linetype = "dashed") +
   geom_hline(yintercept = 0, linetype = "dashed") +
   theme(
     panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
     panel.background = element_rect(colour = "black", size=2),
     axis.title = element_text(size = "20"),
     #legend.box.background=element_rect(fill="white", color="black"),
     #legend.background = element_blank(),
     #legend.position = c(0.6,0),
     #legend.position = "none",
     legend.justification = c("left","bottom"),
     legend.key.width = unit(7, "cm"),
     legend.key.height = unit(0.05, "cm"),
     strip.text = element_text(size = 7),
     legend.text = element_text(size = "15")) +
   ylab("Intensity") +
   xlab("Time") +
   facet_wrap(~Well_ID, ncol = 8, nrow = 8,scales = "free")

 







