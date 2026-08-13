##
## PLACE TO STORE USEFUL FUNCTIONS - - - - - -
## Call from this script if necessary

convert_to_camel_global <- function(){
# List all files in the directory
rename_folder <- tcltk::tk_choose.dir(default = "~/")

rename_folder_files <- list.files(rename_folder, full.names = T)

# Define a function to convert the file names to camel_case
convert_to_camel_case <- function(filename) {
  # Replace spaces with underscores
  new_name <- gsub(" ", "_", filename)
  return(new_name)
}

# Rename the files
for (file in rename_folder_files) {
  # Create the new file name by applying the conversion function
  new_name <- convert_to_camel_case(file)
  
  # Rename the file if the name has changed
  if (file != new_name) {
    file.rename(from = file,to = new_name)
    cat("Renamed:", file, "to", new_name, "\n")
  }
}
} ##func for removing whitespace in filenames to camel case. Hello World = Hello_World


better_read <- function(){
  chosen_data <- tcltk::tk_choose.files()
  chosen_data_read <- read.csv(chosen_data)
  return(chosen_data_read)

  }
randomize_conditions <- function(conditions, animal_num) {
  # Create a full vector with each condition repeated 'repeats' number of times
  
  repeats <- round(animal_num/length(conditions))
  
  full_list <- rep(conditions, each = repeats)
  
  # Randomly shuffle the full list
  randomized_list <- sample(full_list)
  
  # Print the randomized list to the console, with each item on a new line.
  # This format is perfect for copying and pasting into a single Excel column.
  cat("Your randomized conditions are ready to be copied below:\n---\n")
  cat(randomized_list, sep = "\n")
  
  # Invisibly return the vector so it doesn't print twice if assigned to a variable
  invisible(randomized_list)
}

randomize_factorial_design <- function(factor1, factor2, animal_num = 1, col_names = c("Factor1", "Factor2")) {
  
  # 1. Create a data frame of all unique combinations (the orthogonal set)
  all_combinations <- expand.grid(factor1, factor2)
  
  repeats <- ceiling(animal_num/(length(factor2)*length(factor1)))
  
  print(repeats)
  
  # 2. Repeat the entire set of combinations the specified number of times
  # We create an index of rows and repeat that index
  row_indices <- rep(1:nrow(all_combinations), times = repeats)
  full_design <- all_combinations[row_indices, ]
  
  # 3. Randomly shuffle the rows of the final data frame
  randomized_design <- full_design[sample(1:nrow(full_design)), ]
  
  # 4. Assign the desired column names
  colnames(randomized_design) <- col_names
  
  # 5. Print the result in a format perfect for pasting into Excel
  cat("Your randomized factorial design is ready to be copied below:\n")
  cat("(It will paste into two columns in Excel)\n---\n")
  write.table(randomized_design, 
              sep = "\t",             # Use a tab separator for Excel columns
              quote = FALSE,          # Don't put quotes around strings
              row.names = FALSE)    # Don't include R row numbers
  
  # Invisibly return the data frame
  invisible(randomized_design)
}

#  Drug_List <- c("Veh","Ex4")
# # 
#  Drug_2_List <- c("ExF1_5nm","ExD3_5nm","Veh")
# # 
#  randomize_factorial_design(factor1 = Drug_List, factor2 = Drug_2_List, animal_num = 39, col_names = c("Pre_Injection","Post_Injection"))

replace_p00_to_p01 <- function(){
  # List all files in the directory
  rename_folder <- tcltk::tk_choose.dir(default = "~/")
  
  rename_folder_files <- list.files(rename_folder, full.names = T)
  
  # Define a function to convert the file names to camel_case
  convert_to_camel_case <- function(filename) {
    # Replace spaces with underscores
    new_name <- gsub("p00", "p01", filename)
    return(new_name)
  }
  
  # Rename the files
  for (file in rename_folder_files) {
    # Create the new file name by applying the conversion function
    new_name <- convert_to_camel_case(file)
    
    # Rename the file if the name has changed
    if (file != new_name) {
      file.rename(from = file,to = new_name)
      cat("Renamed:", file, "to", new_name, "\n")
    }
  }
} ## func to replace p00 in EVOS file names to p01

read_single_excel_file <- function(){
  data_import <- tcltk::tk_choose.files()
  read_data <- readxl::read_excel(data_import, sheet = "Sheet1")
  return(read_data)
}

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

read_csv_file <- function(){
  chosen_data <- tcltk::tk_choose.files()
  chosen_data_read <- read.csv(chosen_data)
  return(chosen_data_read)
}

atunes_cleanup <- function(mousenames){
  # List all mouse names in string
  mouse_names <- c(mousenames)
  
  # Define a function to convert the file names to camel_case
  convert_to_camel_case <- function(mouse_name) {
    # Replace stupid shit with commas
    new_name <- gsub("//.//..|//..//..|//.//.", ",", mouse_name)
    
    return(new_name)
  }
 print(convert_to_camel_case(mouse_names))
} ## func to clean up atunes names


convert_age_to_days <- function(age_strings) {
  
  # We'll use an average of 30.44 days per month (365.25 / 12)
  days_in_month <- 30.44
  
  # 1. EXTRACT numbers for months and days using regex
  # str_match extracts the first number found before "m" or "d"
  # The [, 2] selects the captured group (the digits)
  months <- str_match(age_strings, "(\\d+)\\s*m")[, 2]
  days <- str_match(age_strings, "(\\d+)\\s*d")[, 2]
  
  # 2. CLEAN the extracted values
  # Convert to numbers; non-matches will become NA
  months_num <- as.numeric(months)
  days_num <- as.numeric(days)
  
  # Replace any NAs with 0 (for strings like "10m" or "5d")
  months_num[is.na(months_num)] <- 0
  days_num[is.na(days_num)] <- 0
  
  # 3. CALCULATE the total days and return as a whole number (integer)
  total_days <- (months_num * days_in_month) + days_num
  return(as.integer(round(total_days)))
}

# #test_string <- c("9m 6d")
# 
# #test_output <- convert_age_to_days(test_string)
# 
# #print(test_output)
# 
# atunes_cleanup(
#   "263422//R//B
# 263425//RR//B
# 263426//R//B
# 263427//L//B
# 263428//RL//B
# 263429//RR//B
# 274124//L//B
# 276583//RL//B
# 277531//L//B
# 277532//RL//B
# 277533//RR//B
# 277534//R//B
# 277535//L//B
# 280836//R//Bl
# 280838//RL//Bl
# 280837//L//Bl
# 280839//R//Bl
# 280840//L//B
# 282946//RL//B
# 282948//L//B
# 282949//RL//B
# 282950//RR//B
# 282951//R//B
# 283663//R//B
# 283664//L//B
# 280841//RL//B
# 282944//R//B
# 263422//R//B
# 263425//RR//B
# 263426//R//B
# 263427//L//B
# 263428//RL//B
# 263429//RR//B
# 274124//L//B
# 276583//RL//B
# 277531//L//B
# 277532//RL//B
# 277533//RR//B
# 277534//R//B
# 277535//L//B
# 280836//R//Bl
# 280838//RL//Bl
# 280837//L//Bl
# 280839//R//Bl
# 280840//L//B
# 282946//RL//B
# 282948//L//B
# 282949//RL//B
# 282950//RR//B
# 282951//R//B
# 283663//R//B
# 283664//L//B
# 280841//RL//B
# 282944//R//B
# ")
# 


