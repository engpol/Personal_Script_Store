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

## LOADING IN RAW DATA - THIS iS WHAT YOU CHANGE TO LOAD DIFFERENT EXCEL_FILES

raw_data <- read_csv("C:/Users/olikc/Desktop/PhD_Research/test/Carissa_16FOV_1/Results/Results.csv")


##FUNCTION FOR ADDING A COLUMN WITH A NUMBER TO USE AS A FACTOR FOR DIVIDING BETWEEN EACH EXPERIMENT IN RAW DATA FOLDER.
##I.E. IF YOU HAVE 16 FOV IN EACH WELL, SELECT 16 FOR FOV_NUMBER

add_key <- function(mydata,FOV_number) {
  key_repeat <- nrow(mydata)/FOV_number
  well_key_df <- data.frame(x = 1, well_key=c(rep(1:key_repeat, each = FOV_number)))
  data_with_key <- cbind(mydata, well_key_df$well_key) %>%
    rename(Exp_Key = "well_key_df$well_key")
  return(data_with_key)
}

data_with_key <- add_key(raw_data,16)
data_with_key <- data_with_key[c("Label","Mean","Exp_Key")]

data_with_key$Exp_Key <- as.factor(data_with_key$Exp_Key)



ggplot(data = data_with_key, aes(x = Exp_Key, y = Mean)) +
  geom_quasirandom() +
  #geom_boxplot() +
  theme_bw() +
  theme( #legend.background = element_rect(fill = "light grey"), 
    #legend.text = element_text(size = "12"),
    legend.key = element_blank(),
    legend.text = element_blank(),
    legend.position = "none",
    #legend.box.background = element_rect(fill = "black", linewidth = 1),
    #legend.box.background = element_blank(),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 20),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.background = element_blank(),
    #panel.border = element_blank(),
    panel.grid = element_blank()
    )

