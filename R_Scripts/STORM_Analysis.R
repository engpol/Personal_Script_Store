# Install tidyverse if you haven't already: install.packages("tidyverse")
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tcltk)

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tcltk)
library(ggridges)

source("C:\\Users\\olikc\\OneDrive\\Documents\\R_Projects\\PhD_Research\\Useful_Functions.R")
Snake_Case_Cols = c("Receptor", "Drug", "Time")

process_multimer_folders <- function(metadata_cols = c("Receptor", "Drug", "Time")) {
  
  # 1. Interactively select the main folder
  main_dir <- tcltk::tk_choose.dir(caption = "Select the Main Folder containing your subfolders")
  
  if (is.na(main_dir)) {
    stop("No folder was selected. Function aborted.")
  }
  
  # 2. Get a list of all subfolders inside the main directory
  subfolders <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)
  
  if (length(subfolders) == 0) {
    stop("No subfolders found in the selected directory.")
  }
  
  # 3. Define a helper function to process a single subfolder
  process_single_folder <- function(folder_path) {
    
    # Extract the folder name and split it by "_"
    folder_name <- basename(folder_path)
    parsed_elements <- str_split(folder_name, "_")[[1]]
    
    # List all CSVs in this specific subfolder
    csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
    
    # COUNT THE CSVS: Store how many files are being combined
    num_csvs <- length(csv_files)
    
    if (num_csvs == 0) {
      warning(paste("No CSV files found in", folder_name))
      return(NULL)
    }
    
    # Read, combine, and aggregate the CSV data
    processed_data <- csv_files %>%
      map_dfr(~ read_csv(.x, show_col_types = FALSE)) %>%
      group_by(multimer_size) %>%
      summarise(channel_1 = sum(channel_1, na.rm = TRUE), .groups = "drop")
    
    # Add the parsed folder name elements as new columns
    for (i in seq_along(parsed_elements)) {
      col_name <- if (i <= length(metadata_cols)) metadata_cols[i] else paste0("Var", i)
      processed_data[[col_name]] <- parsed_elements[i]
    }
    
    # Add the count of CSVs to the dataframe
    processed_data$num_files_combined <- num_csvs
    
    return(processed_data)
  }
  
  # 4. Iterate over all subfolders, process them, and bind into a single long-format DataFrame
  final_combined_data <- map_dfr(subfolders, process_single_folder)
  
  # 5. Final calculations with dynamic grouping!
  final_combined_data_cleaned <- final_combined_data %>%
    mutate(
      multimer_size = as.numeric(multimer_size),
      Multimer_product = channel_1 * multimer_size
    ) %>%
    # Use across(all_of()) to pass a character vector of column names to group_by
    group_by(across(all_of(metadata_cols))) %>%
    mutate(Total_Receptor_Number = sum(Multimer_product, na.rm = TRUE)) %>%
    ungroup() # Always good practice to ungroup after mutating!
  
  message("Data processing complete!")
  
  return(final_combined_data_cleaned)
}

Data_Import <- process_multimer_folders()

Data_Import$Time <- factor(Data_Import$Time, levels = c("1min","5min","30min"))

AREA_SIZE <- 6 # IN MICRONS - NORMALLY GIVEN IN nM
Raw_colour_vector <- c("#075694ff", '#ff2c90ff') ##Select colours for Plasmids here
Darkened_Colours <- darken(Raw_colour_vector,factor = 3) ## Darkened Colours for data points - think it looks slightly better maybe


##TOTAL RECEPTOR LOSS-------------------
Total_Receptor_Data <- Data_Import %>%
  select(Receptor,Drug,Time,Total_Receptor_Number,num_files_combined) %>%
  unique() %>%
  mutate(Receptor_Density = Total_Receptor_Number/(AREA_SIZE^2 * num_files_combined))

Total_Receptor_Density_Plot <- ggplot(data = Total_Receptor_Data, aes(y = Receptor_Density, x = Time, fill = Drug)) +
  scale_color_manual(values = Raw_colour_vector) +
  scale_fill_manual(values = Raw_colour_vector) + 
  stat_summary(fun = "mean", geom = "col", alpha = 1, position = position_dodge(width = 0.9)) + 
  ggprism::theme_prism(palette = "candy_bright", base_size = 16) + 
  xlab("") +
  ylab("No of Receptors on Cell Surface / \u03BCm\u00B2 ") +
  ggnewscale::new_scale_color() +
  scale_color_manual(values = Darkened_Colours, guide = "none") +
  # geom_jitter(size = 3, aes(color = Drug), show.legend = FALSE, position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.9)) +
  theme(legend.position = c(0.125, 0.75),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)
        #legend.background = element_rect(colour = "dark grey", fill = "grey"),
        #legend.text = element_text(colour = "white")
  ) +
  facet_wrap(~Receptor)

  #scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5), limits = c(0,3.63))

Total_Receptor_Density_Plot



## MULTIMER ANALYSIS-----------------

# Assuming metadata_cols is still defined, e.g., metadata_cols <- c("Receptor", "Drug", "Time")

Multimer_Data <- Data_Import %>%
  # 1. Create the bin labels
  mutate(
    multimer_bin = case_when(
      multimer_size <= 10 ~ as.character(multimer_size),
      multimer_size >= 11 & multimer_size <= 20 ~ "11-20",
      multimer_size >= 21 & multimer_size <= 30 ~ "21-30",
      multimer_size >= 31 & multimer_size <= 40 ~ "31-40",
      multimer_size >= 41 & multimer_size <= 49 ~ "41-49", # Added to fill the gap!
      multimer_size >= 50 ~ "50+",
      TRUE ~ "Other" # Failsafe
    )
  ) %>%
  # 2. Convert to factor to enforce logical ordering for future plotting
  mutate(
    multimer_bin = factor(multimer_bin, levels = c(
      as.character(1:10), "11-20", "21-30", "31-40", "41-49", "50+"
    ))
  ) %>%
  # 3. Group by your metadata conditions AND the new bin
  group_by(across(all_of(Snake_Case_Cols)), multimer_bin) %>%
  # 4. Sum the metrics per bin
  summarise(
    binned_channel_1 = sum(channel_1, na.rm = TRUE),
    binned_multimer_product = sum(Multimer_product, na.rm = TRUE),
    Total_Receptor_Number = first(Total_Receptor_Number),
    .groups = "drop" # Always drop groups after summarise!
  ) %>%
  mutate(Multimer_Percent = (binned_multimer_product/(Total_Receptor_Number))*100)

Discrete_Ridgeline_Plot <- ggplot(data = Multimer_Data, 
                                  aes(x = multimer_bin, y = Multimer_Percent, fill = Time)) +
  
  geom_col(color = "black", alpha = 0.8) +
  
  # facet_grid(Time ~ ...) is the magic trick here. 
  # It stacks Time on the Y-axis exactly like a ridgeline plot!
  facet_grid(Time ~ Receptor + Drug) + 
  
  ggprism::theme_prism(palette = "candy_bright", base_size = 14) +
  labs(
    title = "Binned Multimer Percentages Over Time",
    x = "Multimer Bin",
    y = "% of Total Receptors"
  ) +
  theme(
    legend.position = "none",
    # Angle the x-axis text so your bin labels ("11-20", "21-30") don't overlap
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = 0) # Makes the Time labels on the right easy to read
  )

print(Discrete_Ridgeline_Plot)


Overlapped_Discrete_Plot <- ggplot(data = Multimer_Data, 
                                   aes(x = multimer_bin, y = Multimer_Percent, fill = Time)) +
  
  # ADDED: position_dodge() forces the bars to sit next to each other
  geom_col(color = "black", alpha = 0.8, position = position_dodge(width = 0.9)) +
  scale_fill_viridis_d(option = "viridis", alpha = 0.9) +
  
  # Removed Time from facet_grid, keeping only Receptor and Drug
  facet_grid(Receptor ~ Drug) + 
  
  ggprism::theme_prism(palette = "candy_bright", base_size = 14) +
  labs(
    title = "Binned Multimer Percentages Over Time",
    x = "Multimer Complex",
    y = "% of Total Receptors in Complex"
  ) +
  theme(
    legend.position = "bottom", # Bring the legend back!
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(Overlapped_Discrete_Plot)


