##
## THIS CODE WILL COMBINE ALL CSV FILES IN YOUR EXPERIMENT FOLDER AND FORMAT IT INTO A USEFUL DATA TABLE
## SIMPLY PRESS CTRL + SHIFT + ENTER, AND SELECT THE EXPERIMENT FOLDER WHICH YOU RAN THE IMAGEJ MACRO ON
## YOUR RESULTS WILL BE SAVED INTO YOUR EXPERIMENT FOLDER AS A "RESULTS_CONC.CSV" FILE
##

exfolder <- tcltk::tk_choose.dir(default = "~/")

c_csv_extracted_data <- function() {
  
  list_of_extracted_csv <- list.files(path = exfolder, pattern ="Well_Number_\\d+.csv", full.names = TRUE, recursive = TRUE) #Fetch all "Results_Conc.csv" files from the parent directory of all experiments
  
  combined_data <- read.csv(list_of_extracted_csv[1]) #Read first csv file in empty array to save making empty dataframe of right dimensions
  
  csv_files_no_first <- list_of_extracted_csv[2:length(list_of_extracted_csv)] ## JUST SO WE DONT ADD THE FIRST CSV IN THE LIST TWICE
  
  for (file in csv_files_no_first) { ##AS ABOVE BUT LOOPING THROUGH ALL REMAINING CSVS
    temp_data <- read.csv(file)
    combined_data <- rbind(combined_data, temp_data)
  }
  
  combined_data <- transform(combined_data, Well_number = sapply(regmatches(Label, regexec("MMStack_[a-zA-Z]\\d+", Label)), "[", 1))
  
  combined_data <- transform(combined_data, Well_number = sapply(regmatches(Well_number, regexec("[a-zA-Z]\\d+", Well_number)), "[", 1))
  combined_data <- transform(combined_data, Label_channel = paste(Label, Channel_Name))
  
  unique_data <- combined_data[!duplicated(combined_data$Label_channel), ]

  distinct_data <- subset(unique_data, select = c('Well_number', 'Slice','Mean','Channel_Name'))  
  
  pred <- function(subset_df){    
    df <- data.frame(Well_number = subset_df$Well_number[[1]], 
                     Slice = subset_df$Slice[[1]],
                     Channel_Name = subset_df$Channel_Name[[1]],
                     Average_Intensity = mean(subset_df$Mean)
    )                      
    return(df)
  }
  
  averaged_data_list <- by(distinct_data, list(unique_data$Well_number,unique_data$Slice,unique_data$Channel_Name), pred)
  averaged_data <- do.call(rbind, averaged_data_list)
  
  averaged_data <- averaged_data[order(averaged_data$Channel_Name,averaged_data$Well_number), ]
  
  return(averaged_data)
  
} #Function to loop and append through all "Well_Number_.csv" files in parent directory, add well labels, AND  and take averages for each slice/well

c_csv_extracted_data_no_average <- function() {
  
  list_of_extracted_csv <- list.files(path = exfolder, pattern ="Well_Number_\\d+.csv", full.names = TRUE, recursive = TRUE) #Fetch all "Results_Conc.csv" files from the parent directory of all experiments
  
  combined_data <- read.csv(list_of_extracted_csv[1]) #Read first csv file in empty array to save making empty dataframe of right dimensions
  
  csv_files_no_first <- list_of_extracted_csv[2:length(list_of_extracted_csv)] ## JUST SO WE DONT ADD THE FIRST CSV IN THE LIST TWICE
  
  for (file in csv_files_no_first) { ##AS ABOVE BUT LOOPING THROUGH ALL REMAINING CSVS
    temp_data <- read.csv(file)
    combined_data <- rbind(combined_data, temp_data)
  }
  
  combined_data <- transform(combined_data, Well_number = sapply(regmatches(Label, regexec("MMStack_[a-zA-Z]\\d+", Label)), "[", 1))
  
  combined_data <- transform(combined_data, Well_number = sapply(regmatches(Well_number, regexec("[a-zA-Z]\\d+", Well_number)), "[", 1))
  
  combined_data <- transform(combined_data, Label_channel = paste(Label, Channel_Name))
  
  unique_data <- combined_data[!duplicated(combined_data$Label_channel), ]
  
  distinct_data <- subset(unique_data, select = c('Well_number', 'Slice','Mean', 'Channel_Name'))  
  
  return(distinct_data)
  
} #Function to loop and append through all "Well_Number_.csv" files in parent directory, add well labels, AND  and take averages for each slice/well

my_data <- c_csv_extracted_data() ##save output of function into dataframe

my_data_no_average <- c_csv_extracted_data_no_average() ##Same but for un-averaged data

meta_data_list <- list.files(path = exfolder, pattern ="Meta_Data.csv", full.names = TRUE, recursive = FALSE) #Lazy - just to find metadata file
meta_data <- read.csv(meta_data_list[1]) #Read Neta data file

my_data_annotated <- merge(my_data, meta_data, by = "Well_number") ##Merge meta data file with concatenated data to annotate with experimental variables

my_data_no_average_annotated <- merge(my_data_no_average, meta_data, by = "Well_number") ##same but for unaveraged

write.csv(my_data_annotated , paste(exfolder, "/Results_Conc.csv", sep = ""), row.names = FALSE) ##Save anotated data as .csv file

write.csv(my_data_no_average_annotated , paste(exfolder, "/Results_Conc_No_Average.csv", sep = ""), row.names = FALSE) ##Same but for unaveraged


##These are for fucking around with experiments which are spread around different folders - - - -- -






data_split_2 <- my_data_annotated

write.csv(my_data , paste(exfolder, "/Results_Conc.csv", sep = ""), row.names = FALSE)
write.csv(my_data_no_average , paste(exfolder, "/Results_Conc_No_Average.csv", sep = ""), row.names = FALSE)

data_conc_average <- rbind(data_split_1,data_split_2)

write.csv(data_conc_average , paste(exfolder, "/Results_Conc_Average.csv", sep = ""), row.names = FALSE)
