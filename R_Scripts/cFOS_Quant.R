##-----------------------------------------------------------------------------
##  LOADING PACKAGES + USEFUL FUNCTIONS
##  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

packages <- c("dplyr","stringr","readr","tidyverse","readxl") ## packages required to get code running

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
options(scipen = 999) ##Disable scientific notation - dont like how they look - personal preference remove if u want
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
source("C:\\Users\\olikc\\OneDrive\\Documents\\R_Projects\\PhD_Research\\Useful_Functions.R")


##-----------------------------------------------------
## CFOS QUANTIFICATION SCRIPT - DATA IMPORT + WRANGLING
## - - - - - - - - - - - - - - - - - - - - - - -
imported_data <- read_single_excel_file()

filtered_data <- imported_data %>%
  mutate(Neurons_per_mmsqr = (NEURON_NUMBER/TOTAL_REGION_AREA)*1000000,
         Per_Brain_Bregma_Average = paste(BRAIN_ID,Bregma,BRAIN_REGION, sep = "_"),
         Per_Drug_Bregma_Average = paste(DRUG,Bregma,BRAIN_REGION, sep = "_")) %>%
  group_by(Per_Brain_Bregma_Average) %>%
  mutate(Neuron_Average_Bregma_Brain = mean(Neurons_per_mmsqr)) %>%
  ungroup() %>%
  group_by(Per_Drug_Bregma_Average) %>%
  mutate(Neuron_Average_Bregma_Drug = mean(Neurons_per_mmsqr))


per_brain_region_bregma_data <- unique(subset(filtered_data, select = c("BRAIN_ID","BRAIN_REGION","Bregma","Neuron_Average_Bregma_Brain", "DRUG"))) %>%
  mutate(BRAIN_REGION_BREGMA = paste(Bregma,BRAIN_REGION, sep = "_"))

per_drug_region_data <- unique(subset(filtered_data, select = c("DRUG","BRAIN_REGION","Bregma","Neuron_Average_Bregma_Drug")))

ARC_only <- per_brain_region_bregma_data %>%
  filter(BRAIN_REGION == "ARC")

linear_model_per_brain_bregma <- lm(data = per_brain_region_bregma_data, Neuron_Average_Bregma_Brain ~ BRAIN_REGION_BREGMA*DRUG)

lm_ARC_test <- lm(data = ARC_only, Neuron_Average_Bregma_Brain ~ DRUG)

anova(linear_model_per_brain_bregma)

summary(linear_model_per_brain_bregma)

filtered_data$Bregma <- as.factor(filtered_data$Bregma)
filtered_data$DRUG <- as.factor(filtered_data$DRUG)

levels(filtered_data$DRUG)



##Graphs - - - - - - - - - -


data_for_boxplots <- unique(per_brain_region_bregma_data %>%
  group_by(BRAIN_REGION_BREGMA, DRUG) %>%
  reframe(total_value = mean(Neuron_Average_Bregma_Brain), DRUG = DRUG))

ggplot(data = data_for_boxplots, aes(y = total_value, x = BRAIN_REGION_BREGMA, fill = DRUG)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  geom_text(aes(label = BRAIN_REGION_BREGMA), vjust = -1, position = position_dodge2(0.9), check_overlap = T) +
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
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") 

