packages <- c("dplyr","stringr","tidyverse","reshape2","minpack.lm","ggprism","ggnewscale") ## packages required to get code running

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

long_vals_data <- Raw_Data %>%
  pivot_longer(
    cols = 1:last_col(),
    names_to = "Drug")

Raw_colour_vector <- c('#ff2c90ff', 'skyblue','orange','purple') ##Select colours for Plasmids here 
Darkened_Colours <- darken(Raw_colour_vector,factor = 3) ## Darkened Colours for data points - think it looks slightly better maybe



ggplot(data = long_vals_data, aes(y = value, x = Drug, fill = Drug)) +
  scale_color_manual(values = Raw_colour_vector) +
  scale_fill_manual(values = Raw_colour_vector) + 
  stat_summary(fun = "mean", geom = "col", alpha = 1) +
  ggprism::theme_prism(palette = "candy_bright", base_size = 16) + 
  xlab("GLP-1R Agonist") +
  ylab("AUC") +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+
  geom_jitter(size = 3, aes(color = Drug), show.legend = F, width = 0.05) +
  theme(legend.position = c(0.125, 0.75),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)
        #legend.background = element_rect(colour = "dark grey", fill = "grey"),
        #legend.text = element_text(colour = "white")) +
  ) +
  scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5), limits = c(0,3.63))

