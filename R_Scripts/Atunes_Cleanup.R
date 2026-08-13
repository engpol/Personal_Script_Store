##Package_Loading-----------------------
packages <- c("dplyr","stringr","tidyverse","reshape2","minpack.lm","ggprism","ggnewscale","readxl") ## packages required to get code running

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

## Function Loading-----------------

read_xls_file <- function(){
  chosen_data <- tcltk::tk_choose.files()
  chosen_data_read <- read_excel(chosen_data)
  return(chosen_data_read)
} ## General function to easily load in csv files

atunes_cleanup <- function(mousenames){
  # List all mouse names in string
  mouse_names <- c(mousenames)
  
  # Define a function to convert the file names to camel_case
  convert_to_camel_case <- function(mouse_name) {
    # Replace stupid shit with commas
    new_name <- gsub("//.//..|//..//..|//.//.", "|", mouse_name)
    
    return(new_name)
  }
  return(convert_to_camel_case(mouse_names))
}


## Code ------------------

## First, import the most recent file downloaded from Atunes

Atunes_import <- read_xls_file() %>%
  dplyr::select("Cage-ID","Animal-ID","No. of animals", "S","DoB","Age","Strain","Room","PPL","Sire","Dam","19b protocol") %>%
  rename("Animal_ID" = "Animal-ID") ## These are the parameters I'm keeping, change here if you want more

Running_Genotyping_Data <- read_csv("C:\\Users\\olikc\\Desktop\\PhD_Research\\In_Vivo\\Breeding_Stock_Current.csv") ## Then run this to read in the most recent data with Genotyping information

Genotyping_Info <- Running_Genotyping_Data %>% ## To only keep genotyping info so merge doesnt break
  dplyr::select(c("Animal_ID","Genotype"))

Mating_List <- c(unique(Atunes_import$Sire),unique(Atunes_import$Dam))
Mating_List_Cleaned <- Mating_List[!is.na(Mating_List)]

Mice_List_Mating <- data.frame("Animal_ID" = unique(Atunes_import$"Animal_ID")) %>%
  mutate(Mating = (Animal_ID %in% Mating_List_Cleaned))

Atunes_merged <- merge(Atunes_import,Genotyping_Info, all.x = T, by = "Animal_ID") 
  ## Add genotyping info to entire stock, the ones with no genotyping info will be blank

Atunes_merged_Mate <- merge(Atunes_merged,Mice_List_Mating, all.x = T, by = "Animal_ID")
   
write.csv(Atunes_merged_Mate, file = "C:\\Users\\olikc\\Desktop\\PhD_Research\\In_Vivo\\Breeding_Stock_Current.csv") ## Save output

Atunes_merged_Mate %>%
  group_by(as.factor(S)) %>%
  count(as.factor(Genotype))


##For filtering data-------

Mouse_IDs_to_Filter <- atunes_cleanup(c("263422//R//B
263425//RR//B
263426//R//B
263427//L//B
263428//RL//B
263429//RR//B
274124//L//B
276583//RL//B
277531//L//B
277532//RL//B
277533//RR//B
277534//R//B
277535//L//B
280836//R//B
280837//L//B
280838//RL//B
280839//R//B
280840//L//B
280841//RL//B
282944//R//B
282946//RL//B
282948//L//B
282949//RL//B
282950//RR//B
282951//R//B
283663//R//B
283664"))#Put mouse ids in here. just copy and paste with the //R//B etc., they will be removed

filtered_dataset_df <- Atunes_merged_Mate %>%
  filter(str_detect(Animal_ID, Mouse_IDs_to_Filter))


write.csv(filtered_dataset_df, file = "C:\\Users\\olikc\\Desktop\\PhD_Research\\In_Vivo\\Filtered_Dataset.csv")
