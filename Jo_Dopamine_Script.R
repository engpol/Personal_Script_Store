##Package_Loading-----------------------
packages <- c("dplyr","stringr","tidyverse","reshape2","minpack.lm","ggprism","ggnewscale") ## packages required to get code running

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##Functions-------
read_csv_file <- function(){
  chosen_data <- tcltk::tk_choose.files()
  chosen_data_read <- read.csv(chosen_data)
  return(chosen_data_read)
} ## General function to easily load in csv files

dopamine_data_import <- function(){
  
  exfolder <- tcltk::tk_choose.dir(default = "~/")
  list_of_extracted_csv <- list.files(path = exfolder, pattern =".txt", full.names = TRUE, recursive = TRUE) #Fetch all ".txt" files from the parent directory of all experiments
  
  combined_data <- read.delim(list_of_extracted_csv[1], header = F) %>% #Read first csv file in empty array to save making empty dataframe of right dimensions
    mutate(Mouse_ID_Dirty = gsub(pattern = paste0(exfolder,"/"), replacement = "", list_of_extracted_csv[1])) # get name of txt file from list declared above, remove path from name
   
  if(length(list_of_extracted_csv) > 1) {
    
    csv_files_no_first <- list_of_extracted_csv[2:length(list_of_extracted_csv)] ## JUST SO WE DONT ADD THE FIRST CSV IN THE LIST TWICE
    
    for (file in csv_files_no_first) { ##AS ABOVE BUT LOOPING THROUGH ALL REMAINING CSVS
      temp_data <- read.delim(file, header = F) %>%
        mutate(Mouse_ID_Dirty = gsub(pattern = paste0(exfolder,"/"), replacement = "", file))#Read first csv file in empty array to save making empty dataframe of right dimensions
      combined_data <- rbind(combined_data, temp_data)
    }
  }
  
  combined_data <- combined_data %>%
    mutate(Mouse_ID = gsub(pattern = ".txt", replacement = "", Mouse_ID_Dirty)) %>% ## Remove file extension from name
    select(-c(Mouse_ID_Dirty))
  
  colnames(combined_data) <- c("Time","Dopamine","Mouse_ID")
  
  return(combined_data)
  
} ## Function to clean up and combine all dopamine data, with name of txt file stored in col

## Loading in and reformatting datasets----

dopamine_data <- dopamine_data_import() ## load in dopamine data and set to data frame

Mouse_Food_Data <- read_csv_file() ## load in dataframe containing Mouse IDs and food consumed

pellet_timing <-  read_csv_file() ## Load in data containing pellet consumption timings and mouse ids

Pellet_no = 1:(ncol(pellet_timing) - 1) ## Count number of pellet consumptions

Pellet_loop_func <- function() {
  
for(Pellet in Pellet_no) { ## Loop through all pellets individually
  Pellet_x_only <- pellet_timing[,c(1,(Pellet + 1))] ## Select data from nth pellet 
  
  Pellet_x_string <- colnames(Pellet_x_only)[2] ## Get name of nth pellet, to save to a variable called Pellet_Number
  
  Pellet_x_merge <- merge(dopamine_data, Pellet_x_only) ## Right merge the dopamine data and Pellet timing to get pellet timings for each mouse
  
  Pellet_X <- Pellet_x_merge[,4] ## Get Pellet consumption timings and save to new col
  
  Pellet_x_merge$Mouse_ID <- as.factor(Pellet_x_merge$Mouse_ID) ## Change from char to factor data type 
  
  Pellet_x_merge_data_filtered <- Pellet_x_merge %>% 
    mutate(Pellet_X_Col = Pellet_X) %>%
    group_by(Mouse_ID) %>% ## For each mouse... 
    dplyr::filter(Time <= (Pellet_X_Col + 10),Time >= (Pellet_X_Col - 10)) %>% ## Remove dopamine data from outside desired range - (10s before and after pellet consumption)
    mutate(Time_Corrected = Time - Pellet_X_Col, ## Set time of pellet consumption to 0
           Pellet_Number = Pellet_x_string) %>% # Set pellet number to a variable
    dplyr::select(-c("Time","Pellet_X_Col",Pellet_x_string)) # Remove unneccesary cols
  
  if(Pellet < 2) {
    Pellet_combined <- Pellet_x_merge_data_filtered
  }
  if(Pellet > 1){
    Pellet_combined <- rbind(Pellet_combined, Pellet_x_merge_data_filtered)
  }
  
  
}
  return(Pellet_combined)
} ## function to loop through and annotate the data for each pellet and mouse 

Pellet_data_combined <- Pellet_loop_func() ## call func to dataframe

Pellet_1_merged_data <- merge(dopamine_data, pellet_timing[,c(1,2)]) ## Get timings of first pellet for each mouse
## BASELINE CORRECTION TOTAL----
Dopamine_Baseline_Data <- Pellet_1_merged_data %>% ## For determining what is the Dopamine 'Baseline' i.e. average dopamine over the period prior to consumption
  group_by(Mouse_ID) %>%  ## for each mouse... 
  dplyr::filter(Time <=  (Pellet_1 -10)) %>% ## remove rows 120 s prior to consumption of first pellet
  mutate(Dopamine_add_10 = (Dopamine + 10)) %>%
  mutate(Baseline_Dopamine = mean(Dopamine_add_10)) %>% ## Find average dopamine
  select(c("Baseline_Dopamine","Mouse_ID")) %>%  ## Remove unneccessary columns
  filter(row_number()==1) ## each row for each mouse is identical, only keep 1

##BASELINE CORRECTION BY PELLET------

Dopamine_Baseline_Data <- Pellet_data_combined %>% ## For determining what is the Dopamine 'Baseline' i.e. average dopamine over the period prior to consumption
  group_by(Mouse_ID, Pellet_Number) %>%  ## for each mouse... 
  dplyr::filter(Time_Corrected <= -5) %>% ## remove rows 120 s prior to consumption of first pellet
  mutate(Dopamine_add_10 = (Dopamine + 10)) %>%
  mutate(Baseline_Dopamine = mean(Dopamine_add_10)) %>% ## Find average dopamine
  select(c("Baseline_Dopamine","Mouse_ID","Pellet_Number")) %>%  ## Remove unneccessary columns
  filter(row_number()==1)

Pellet_data_combined_add_10 <- Pellet_data_combined %>%
  mutate(Dopamine_add_10 = (Dopamine + 10)) ## So as not to accidentally add - a lot of data is negative

Pellets_data_combined_baseline <- merge(Pellet_data_combined_add_10, Dopamine_Baseline_Data) %>%
  mutate(Delta_Dopamine = (Dopamine_add_10 + (1-Baseline_Dopamine)))## combine average dopamine levels with the rest of the date
  

Pellets_data_combined_baseline_food <- merge(Pellets_data_combined_baseline, Mouse_Food_Data)

Pellets_data_combined_baseline_1_mouse <- Pellets_data_combined_baseline %>%
  filter(Mouse_ID == "EE")



#Data from 1 pellet only, useful for messing around-----

Pellet_1_only <- pellet_timing[,c(1,2)]

Pellet_1_string <- colnames(Pellet_1_only)[2]

Pellet_1_test_data <- merge(dopamine_data, Pellet_1_only)

Pellet_1_test_data$Mouse_ID <- as.factor(Pellet_1_test_data$Mouse_ID)

Pellet_X <- Pellet_1_test_data[,4]

Pellet_1_test_data_filtered <- Pellet_1_test_data %>%
  mutate(Pellet_X_Col = Pellet_X) %>%
  group_by(Mouse_ID) %>%
  dplyr::filter(Time <= (Pellet_X_Col + 10),Time >= (Pellet_X_Col - 10)) %>%
  mutate(Time_Corrected = Time - Pellet_1,
         Pellet_Number = "Pellet_1") %>%
  dplyr::select(-c("Time","Pellet_X_Col",Pellet_1_string))


###GRAPHS------

ggplot(data = Pellets_data_combined_baseline_food, aes(x = Time_Corrected, y = Delta_Dopamine, color = Food)) +
  #geom_line(data = data_filter, aes(x = time, y = FDelta0, group = Neuron_ID), alpha = 0.2, color = "grey") +
  # stat_summary(geom="ribbon", fun.data=mean_cl_normal, 
   #            alpha = 1, fill = "light blue", linetype = "solid", fun.args=(conf.int=0.683), color = "black") +
  stat_summary_bin(geom = "line", fun = mean, size = 0.1, bins = 20) + 
  #geom_smooth(aes(y=Delta_Dopamine), alpha=0.5) +
  geom_vline(xintercept = 0, linetype = "solid", size = 2, color = "purple", alpha = 0.25) +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "12"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 19)) +
   # legend.position = "none") +
  ylab(paste0("\u0394","Dopamine")) +
  xlab("Time around Giving Pellet (s)") + 
  #coord_cartesian(ylim=c(-0.087,-0.083))+ 
  #scale_y_continuous(breaks = c(-0.087,-0.085,-0.083))
# +
  facet_wrap(~Mouse_Number, scales = "free")




#  coord_cartesian(ylim = c(-4.552, -4.548))
  
  

  #stat_summary(geom = "point", fun.y = mean, size=3, shape=21, fill="black")



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


