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
better_read <- function(){
  chosen_data <- tcltk::tk_choose.files()
  chosen_data_read <- read.csv(chosen_data)
  return(chosen_data_read)
}
source("C:\\Users\\olikc\\OneDrive\\Documents\\R_Projects\\PhD_Research\\Useful_Functions.R")
linefit_width = 0
##STANDARD_DATA_WRANGLING-----------
Standard_cAMP_Curve <- read.csv("C:/Users/olikc/Desktop/PhD_Research/UAA/R_cAMP_Assays/Standard_cAMP_Curve.csv") %>% ##Loading in standard cAMP curve to fit into logistic and predict read values
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Value") %>%
  select(-c("Value")) %>%
  mutate(log_Conc = log10(Conc)) %>%
  group_by(log_Conc) %>%
  mutate(Average_log_val = mean(value)) %>%
  select(-c("Conc","value")) %>%
  unique()


read_data <- better_read() ## Choose and read in cAMP data
long_vals_data_1 <- read_data %>%
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Condition") %>%
  mutate(Condition_Clean = gsub(pattern = "\\..*", replacement = "", Condition)) %>%
  select(-c("Condition")) %>%
  na.exclude()

long_vals_data_1 <- long_vals_data_1 %>%
  group_by(Drug_Conc,Condition_Clean) %>%
  mutate(average_extracellular_intensity = mean(value, na.rm = T),
         Type = "Arrestin 2")

long_vals_data_comb <- rbind(long_vals_data,long_vals_data_1)

# Define the global minimum and maximum of your raw data
global_bottom <- min(long_vals_data$average_extracellular_intensity, na.rm = TRUE)
global_top <-  min(long_vals_data$average_extracellular_intensity, na.rm = TRUE)

### Getting the params of minimum in case wanting to shift y axis to 0

long_vals_data_one_Cond <- long_vals_data %>%
  filter(Condition_Clean == "ExF1")

standard_curve_logistic <- nls(average_extracellular_intensity ~ lower + ((upper - lower) / (1 + 10^(midpoint-Drug_Conc))),
                               data = long_vals_data_one_Cond,
                               start=list(lower = min(long_vals_data_one_Cond$value), upper = max(long_vals_data_one_Cond$value), midpoint = mean(long_vals_data$Drug_Conc)), control = nls.lm.control(maxiter = 200))



antagonist_curve_logistic <- nls(
  average_extracellular_intensity ~ lower + ((upper - lower) / (1 + 10^((logIC50 - Drug_Conc) * hill_slope))),
  data = long_vals_data_one_Cond,
  start = list(
    lower = min(long_vals_data_one_Cond$value), 
    upper = max(long_vals_data_one_Cond$value), 
    logIC50 = mean(long_vals_data_one_Cond$Drug_Conc), 
    hill_slope = -1 # Starts at -1 for a standard inhibitory downward curve
  ), 
  control = nls.control(maxiter = 200) # See note below on nls.lm.control
)


params = coef(standard_curve_logistic)
  

#mutate(Normalised_Response = (value - params[1])/(100 - params[1])*100)  %>% ##Normalisation_function = X(Normalised) = X- Xmin/XMax - Xmin
  

long_vals_data_y_correct <- long_vals_data %>%
  mutate(intensity_corrected = (value - params[1])/(100 - params[1])*100) %>%
  group_by(Drug_Conc,Condition_Clean) %>%
  mutate(average_extracellular_intensity = mean(intensity_corrected, na.rm = T),
         #average_extracellular_intensity = ifelse(average_extracellular_intensity < 0, 0, average_extracellular_intensity),
         value = intensity_corrected) %>%
  select(-c("intensity_corrected"))


####Logistic curve functions---------


## if you want to fit 4 param logistic use obj below

## STIMULATION - - - - - - - -

param_4_logistic <- geom_smooth(method = "nlsLM",
                                formula = y ~ lower + (upper - lower) / (1 + (x / midpoint)^(slope)), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                method.args = list(start=list(lower = min(long_vals_data$value), upper = max(long_vals_data$value), midpoint = mean(long_vals_data$Drug_Conc), slope = 1), control = nls.lm.control(maxiter =2000)),
                                se=FALSE,
                                linewidth=1.5)


## if you want to fit 3 param logistic use obj below

param_3_logistic <- geom_smooth(method = "nlsLM",
                                formula = y ~ global_bottom + ((upper - global_bottom) / (1 + 10^(midpoint-x))), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                method.args = list(start=list(upper = max(long_vals_data$value), midpoint = median(long_vals_data$Drug_Conc)), control = nls.control(maxiter = 2000)),
                                se=FALSE,
                                linewidth=1.5,
                                aes(group = interaction(Condition_Clean, Type)))



param_2_logistic_normalised <- geom_smooth(method = "nlsLM",
                                           formula = y ~ 100/(1+10^((midpoint-x))), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                           method.args = list(start=list(midpoint = median(long_vals_data$Drug_Conc)), control = nls.control(maxiter = 1000)),
                                           se=FALSE,
                                           linewidth=1.5,
                                           aes(group = Condition_Clean))  






## INHIBITION - - - - - - - - -

param_4_logistic_antagonist <- geom_smooth(
  method = "nlsLM",
  formula = y ~ lower + ((upper - lower) / (1 + 10^((midpoint - x) * slope))), 
  method.args = list(
    start = list(
      lower = min(long_vals_data$value), 
      upper = max(long_vals_data$value), 
      midpoint = mean(long_vals_data$Drug_Conc), 
      slope = -1 # Starts at -1 for a downward antagonistic curve
    ), 
    control = minpack.lm::nls.lm.control(maxiter = 200)
  ),
  se = FALSE,
  linewidth = 1.5
)

param_3_logistic_antagonist <- geom_smooth(
  method = "nlsLM",
  formula = y ~ lower + ((upper - lower) / (1 + 10^(x - midpoint))), # Flipped to x - midpoint
  method.args = list(
    start = list(
      lower = min(long_vals_data$value), 
      upper = max(long_vals_data$value), 
      midpoint = median(long_vals_data$Drug_Conc)
    ), 
    control = minpack.lm::nls.lm.control(maxiter = 1000) # Corrected to nls.lm.control
  ),
  se = FALSE,
  linewidth = 1.5,
  aes(group = Condition_Clean)
)


param_2_logistic_normalised_antagonist <- geom_smooth(
  method = "nlsLM",
  formula = y ~ 100 / (1 + 10^(x - midpoint)), # Flipped to x - midpoint
  method.args = list(
    start = list(
      midpoint = median(long_vals_data$Drug_Conc)
    ), 
    control = minpack.lm::nls.lm.control(maxiter = 1000) # Corrected to nls.lm.control
  ),
  se = FALSE,
  linewidth = 1.5,
  aes(group = Condition_Clean)
)

param_4_logistic_antagonist_constrained <- geom_smooth(
  method = "nlsLM",
  # Use the fixed variables instead of parameter names 'lower' and 'upper'
  formula = y ~ global_bottom + ((global_top - global_bottom) / (1 + 10^((midpoint - x) * slope))), 
  method.args = list(
    # Remove lower and upper from this list so they aren't estimated!
    start = list(
      midpoint = mean(long_vals_data$Drug_Conc, na.rm = TRUE), 
      slope = -1
    ), 
    control = minpack.lm::nls.lm.control(maxiter = 200)
  ),
  se = FALSE,
  linewidth = 1.5,
  aes(group = Condition_Clean, color = Condition_Clean) # Grouping dictates the separate curves
)

param_3_logistic_antagonist_constrained <- geom_smooth(
  method = "nlsLM",
  formula = y ~ global_bottom + ((global_top - global_bottom) / (1 + 10^(x - midpoint))), # Flipped to x - midpoint
  method.args = list(
    start = list(
      midpoint = median(long_vals_data$Drug_Conc)
    ), 
    control = minpack.lm::nls.lm.control(maxiter = 1000) # Corrected to nls.lm.control
  ),
  se = FALSE,
  linewidth = 1.5,
  aes(group = Condition_Clean)
)




Raw_colour_vector <- c('#075694ff', '#ff2c90ff') ##Select colours for Plasmids here 
Darkened_Colours <- darken(Raw_colour_vector,factor = 1.8) ## Darkened Colours for data points - think it looks slightly better maybe

long_vals_data_comb$Type <- as.factor(long_vals_data_comb$Type)
## Graph below is for all data collated
ggplot(data = long_vals_data_comb, aes(y = average_extracellular_intensity, x = Drug_Conc, color = Condition_Clean, linetype = Type, group = interaction(Condition_Clean, Type))) +
  scale_color_manual(values = Raw_colour_vector) +
  param_3_logistic + ##Choose your desired parameter model 
  ggprism::theme_prism(palette = "candy_bright", base_size = 16) +
  xlab("[Agonist], M") +
  ylab("AUC Arrestin Shit") +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+
  stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},  ##Add SEM as error bars - i.e. standard dev/sqrt(x)
               fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
               fun = mean, linewidth = 1,
               geom = "errorbar",
               linetype = "solid",
               width = 0.15,
               aes(group = interaction(Condition_Clean, Type), y = value, color = Condition_Clean)) +
  geom_point(size = 3, aes(color = Condition_Clean, shape = Type), show.legend = F) +
  theme(legend.position = c(0.125, 0.75),
        legend.text = element_text(size = 21),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)
        #legend.background = element_rect(colour = "dark grey", fill = "grey"),
        #legend.text = element_text(colour = "white")) +
  ) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100), limits = c(-7,117)) +
  scale_x_continuous(
    limits = c(min(long_vals_data$Drug_Conc), max(long_vals_data$Drug_Conc)), 
    breaks = (max(long_vals_data$Drug_Conc)):(min(long_vals_data$Drug_Conc)),
    guide = "prism_offset_minor",
    minor_breaks = log10(rep(1:9, 7)*(10^rep((max(long_vals_data$Drug_Conc)):(min(long_vals_data$Drug_Conc)-1), each = 9))),
    labels = function(lab) {
      do.call(
        expression,
        lapply(paste(lab), function(x) bquote(bold("10"^.(x))))
      )
    }
  )



r <- range(long_vals_data_one_Cond$Drug_Conc)
xNew <- seq(r[1],r[2],length.out = 200)
yNew <- predict(antagonist_curve_logistic,list(Drug_Conc = xNew))

plot(long_vals_data_one_Cond$Drug_Conc,long_vals_data_one_Cond$average_extracellular_intensity, xlab = "log[cAMP]", ylab = "Response")
lines(xNew,yNew, col = "red", lwd = 1)


