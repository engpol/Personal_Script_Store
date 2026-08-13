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
linefit_width = 0 ## Set variable to any integer just so var exists and doesnt throw an error
## LOADING IN DATA-----------


read_data <- better_read() ## Choose and read in cAMP data
long_vals_data <- read_data %>%
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Condition") %>%
  mutate(Condition_Clean = gsub(pattern = "\\..*", replacement = "", Condition),
         Plasmid = gsub(pattern = "_.*", replacement = "", Condition_Clean),
         Amino_Acid = gsub(pattern = ".*_", replacement = "", Condition_Clean)) %>%
  dplyr::select(-c("Condition")) %>%
  filter(value != 0)


## STANDARD_DATA_WRANGLIN -------------------

Standard_cAMP_Curve <- read.csv("C:/Users/olikc/Desktop/PhD_Research/UAA/R_cAMP_Assays/Standard_cAMP_Curve_NEW.csv") %>% ##Loading in standard cAMP curve to fit into logistic and predict read values
  pivot_longer(
    cols = 2:last_col(),
    names_to = "Value") %>%
  dplyr::select(-c("Value")) %>%
  mutate(log_Conc = log10(Conc)) %>%
  group_by(log_Conc) %>%
  mutate(Average_log_val = mean(value)) %>%
  dplyr::select(-c("Conc","value")) %>%
  unique()


#Standard curve equation Bottom + (Top-Bottom)/(1+10^(X-LogIC50)) = 
#Fit a 3 parameter inverse logistic (equivalent to inhibitor binding) to standard curve - see bottom of code to check model fitting


standard_curve_logistic <- nls(Average_log_val ~ lower + (upper - lower)/(1 + 10^(log_Conc-midpoint)),
                                  data = Standard_cAMP_Curve,
                                  start = list(lower = min(Standard_cAMP_Curve$Average_log_val), upper = max(Standard_cAMP_Curve$Average_log_val), midpoint = mean(Standard_cAMP_Curve$log_Conc)))

params = coef(standard_curve_logistic) ##Extract the params - 1 = bottom, 2 = peak, 3 = IC50/midpoint of curve

logistic_solve_for_x <- function(upper, lower, midpoint, y_values) {  ## Solve algebraic for 3 param logistic for x
  x_value = (log10(((upper-lower)/(y_values-lower) - 1)) + midpoint)    
  return(x_value) }

long_vals_data$log_cAMP <- logistic_solve_for_x(upper = params[2], midpoint = params[3], lower = params[1], y_values = long_vals_data$value) ##Create a new variable with log cAMP concentrations

long_vals_data$nM_cAMP <- 10^long_vals_data$log_cAMP

long_vals_data <- long_vals_data %>%
  group_by(Drug_Conc,Condition_Clean) %>%
  mutate(average_nM_cAMP = mean(nM_cAMP, na.rm = T)) %>%
  na.exclude()





#### IF NO FSK NORMALISATION ------


####Logistic curve functions---------

## if you want to fit 4 param logistic use obj below



param_4_logistic <- geom_smooth(method = "nlsLM",
                                formula = y ~ lower + (upper - lower) / (1 + (x / midpoint)^(slope)), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                method.args = list(start=list(lower = min(long_vals_data$nM_cAMP), upper = max(long_vals_data$nM_cAMP), midpoint = mean(long_vals_data$Drug_Conc), slope = 1), control = nls.lm.control(maxiter = 200)),
                                se=FALSE)


## if you want to fit 3 param logistic use obj below

param_3_logistic <- geom_smooth(method = "nlsLM",
                                formula = y ~ lower + ((upper - lower) / (1 + 10^(midpoint-x))), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                method.args = list(start=list(lower = min(long_vals_data$nM_cAMP), upper = max(long_vals_data$nM_cAMP), midpoint = median(long_vals_data$Drug_Conc)), control = nls.control(maxiter = 1000)),
                                se=FALSE,
                                linewidth=1.5,
                                aes(group = Condition_Clean))



param_3_logistic_normalised <- geom_smooth(method = "nlsLM",
                                formula = y ~ 100/(1+10^(midpoint-x)), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                method.args = list(start=list(midpoint = median(long_vals_data$Drug_Conc)), control = nls.control(maxiter = 1000)),
                                se=FALSE,
                                linewidth=1.5,
                                aes(group = Condition_Clean))

##GRAPHS-----------------

Raw_colour_vector <- c('Slateblue1', 'Magenta1', 'springgreen1', 'tomato1') ##Select colours for Plasmids here 
Darkened_Colours <- darken(Raw_colour_vector,factor = 1.8) ## Darkened Colours for data points - think it looks slightly better maybe
long_vals_data$Amino_Acid <- factor(long_vals_data$Amino_Acid, levels = c("None","TCOA","BCNK")) ## Change order of amino acids - so none comes first

## Graph below is for all data collated
ggplot(data = long_vals_data, aes(y = average_nM_cAMP, x = Drug_Conc, color = Plasmid, linetype = Amino_Acid)) +
  scale_color_manual(values = Raw_colour_vector) +
  param_3_logistic + ##Choose your desired parameter model 
  ggprism::theme_prism(palette = "candy_bright", base_size = 16) +
  xlab("[GLP-1], M") +
  ylab("cAMP (nM)") +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+
  stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},  ##Add SEM as error bars - i.e. standard dev/sqrt(x)
               fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
               fun = mean, linewidth = 1,
               geom = "errorbar",
               linetype = "solid",
               width = 0.15,
               aes(group = Condition_Clean, y = nM_cAMP, color = Plasmid)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted"))+
  guides(linetype = guide_legend(override.aes = list(color = "green"))) +
  geom_point(size = 3, aes(shape = Amino_Acid, color = Plasmid), show.legend = F) +
  theme(legend.position = c(0.125, 0.75),
        legend.text = element_text(size = 21),
       axis.title = element_text(size = 20),
       axis.text = element_text(size = 18)
        #legend.background = element_rect(colour = "dark grey", fill = "grey"),
        #legend.text = element_text(colour = "white")) +
  ) +
  scale_x_continuous(
    limits = c(min(long_vals_data$Drug_Conc), max(long_vals_data$Drug_Conc)), 
    breaks = (min(long_vals_data$Drug_Conc)):(max(long_vals_data$Drug_Conc)),
    guide = "prism_offset_minor",
    minor_breaks = log10(rep(1:9, 7)*(10^rep((min(long_vals_data$Drug_Conc)):(max(long_vals_data$Drug_Conc)-1), each = 9))),
    labels = function(lab) {
      do.call(
        expression,
        lapply(paste(lab), function(x) bquote(bold("10"^.(x))))
      )
    }
  )

#+
#  transition_states(
#    Amino_Acid,
#    transition_length = 2,
#    state_length = 2, wrap = T
#  ) +
#  ease_aes('cubic-in-out')


## Graphs below are for data normalised by plasmid - - - - - -- - - - - - - - - - -


##Normalisation_function used = X(Normalised) = X- Xmin/XMax - Xmin where params are ex


Amino_Acid_to_Normalise = "TCOA" ## Select amino acid or condition


param_extraction_normalising_curve <- long_vals_data %>%
  filter(Amino_Acid == Amino_Acid_to_Normalise,
         Plasmid == "F2") ## Select Curve to which you wish to normalsie data

normalising_curve_params <- coef(nlsLM(average_nM_cAMP ~ lower + ((upper - lower) / (1 + 10^(midpoint-Drug_Conc))),
                   data = param_extraction_normalising_curve,
                   start = list(lower = min(param_extraction_normalising_curve$nM_cAMP), upper = max(param_extraction_normalising_curve$nM_cAMP), midpoint = mean(param_extraction_normalising_curve$Drug_Conc)),
                   control = nls.lm.control(maxiter = 1000))) ## Extract parameters from 3Pl curve fitted to curve being normalised to

data_1_amino_acid <- long_vals_data %>%
  filter(Amino_Acid == Amino_Acid_to_Normalise)  ## For Graphing

data_1_amino_acid  <- data_1_amino_acid %>%
  mutate(Normalised_Response = (nM_cAMP - normalising_curve_params[1])/(normalising_curve_params[2] - normalising_curve_params[1])*100)  %>% ##Normalisation_function = X(Normalised) = X- Xmin/XMax - Xmin
  group_by(Drug_Conc, Plasmid) %>%
  mutate(Average_Normalised_Response = mean(Normalised_Response),
         Average_Normalised_Response = ifelse(Average_Normalised_Response < 0, 0, Average_Normalised_Response)) ## Use normalising min max scaling to  get values also this last bit is a bit of a fudge to force values to a minimum of 0
  


ggplot(data = data_1_amino_acid, aes(y = Average_Normalised_Response, x = Drug_Conc, color = Plasmid)) +
  scale_color_manual(values = Raw_colour_vector) +
  param_3_logistic + ##Choose your desired parameter model 
  ggprism::theme_prism(palette = "candy_bright", base_size = 16) +
  xlab("[GLP-1], M") +
  ylab("cAMP (%WT Max)") +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+ ## Guide none is equivalent to show.legend = F
  stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},  ##Add SEM as error bars - i.e. standard dev/sqrt(x)
               fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
               fun = mean, linewidth = 1,
               geom = "errorbar",
               linetype = "solid",
               width = 0.15,
               aes(group = Plasmid, y = Normalised_Response, color = Plasmid)) +
  geom_point(size = 3, aes(shape = Plasmid, color = Plasmid), show.legend = F) +
  theme(legend.position = c(0.1, 0.85),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)
        #legend.background = element_rect(colour = "dark grey", fill = "grey"),
        #legend.text = element_text(colour = "white")) +
                                        ) +
  scale_x_continuous( ## To add log ticks to x axis
    limits = c(min(long_vals_data$Drug_Conc), max(long_vals_data$Drug_Conc)), 
    breaks = (min(long_vals_data$Drug_Conc)):(max(long_vals_data$Drug_Conc)),
    guide = "prism_offset_minor",
    minor_breaks = log10(rep(1:9, 7)*(10^rep((min(long_vals_data$Drug_Conc)):(max(long_vals_data$Drug_Conc)-1), each = 9))),
    labels = function(lab) {
      do.call(
        expression,
        lapply(paste(lab), function(x) bquote(bold("10"^.(x))))
      )
    }
  ) +
  scale_y_continuous(breaks = c(0,25,50,75,100))






## IF FSK NORMALISATION



## IF FSK NORMALISATION

#### IF FSK NORMALISATION ------
####Logistic curve functions---------

## if you want to fit 4 param logistic use obj below

long_vals_data_no_FSK <- long_vals_data %>%
  filter(Drug_Conc != -6)

param_4_logistic <- geom_smooth(method = "nlsLM",
                                formula = y ~ lower + (upper - lower) / (1 + (x / midpoint)^(slope)), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                method.args = list(start=list(lower = min(long_vals_data_no_FSK$nM_cAMP), upper = max(long_vals_data_no_FSK$nM_cAMP), midpoint = mean(long_vals_data_no_FSK$Drug_Conc), slope = 1), control = nls.lm.control(maxiter = 200)),
                                se=FALSE)


## if you want to fit 3 param logistic use obj below

param_3_logistic <- geom_smooth(method = "nlsLM",
                                formula = y ~ lower + ((upper - lower) / (1 + 10^(midpoint-x))), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                method.args = list(start=list(lower = min(long_vals_data_no_FSK$nM_cAMP), upper = max(long_vals_data_no_FSK$nM_cAMP), midpoint = median(long_vals_data_no_FSK$Drug_Conc)), control = nls.control(maxiter = 1000)),
                                se=FALSE,
                                linewidth=1.5,
                                aes(group = Condition_Clean))



param_3_logistic_normalised <- geom_smooth(method = "nlsLM",
                                           formula = y ~ 100/(1+10^(midpoint-x)), # 4 param logistic equation - make sure y and x are labelled as y and x as they are inherited from aes from ggplot
                                           method.args = list(start=list(midpoint = median(long_vals_data_no_FSK$Drug_Conc)), control = nls.control(maxiter = 1000)),
                                           se=FALSE,
                                           linewidth=1.5,
                                           aes(group = Condition_Clean))



###GRAPHS-------

##Normalisation_function used = X(Normalised) = X- Xmin/XMax - Xmin where params are ex

data_FSK_normalised  <- long_vals_data %>%
  group_by(Plasmid, Amino_Acid) %>%
  mutate(minimum_cAMP = min(nM_cAMP)) %>%
  mutate(Normalised_Response = ((average_nM_cAMP - min(nM_cAMP))/((average_nM_cAMP[Drug_Conc == -6] - min(nM_cAMP))))*100) %>% ##Normalisation_function = X(Normalised) = X- Xmin/XMax - Xmin
  filter(Drug_Conc != -6)

Raw_colour_vector <- c('Slateblue1', 'Magenta1', 'springgreen1', 'tomato1') ##Select colours for Plasmids here 
Darkened_Colours <- darken(Raw_colour_vector,factor = 1.8) ## Darkened Colours for data points - think it looks slightly better maybe
long_vals_data$Amino_Acid <- factor(long_vals_data$Amino_Acid, levels = c("None","TCOA","BCNK")) ## Change order of amino acids - so none comes first


ggplot(data = data_FSK_normalised, aes(y = Normalised_Response, x = Drug_Conc, color = Plasmid, linetype = Amino_Acid)) +
  scale_color_manual(values = Raw_colour_vector) +
  param_3_logistic + ##Choose your desired parameter model 
  ggprism::theme_prism(palette = "candy_bright", base_size = 16) +
  xlab("[GLP-1], M") +
  ylab("cAMP (%WT Max)") +
  ggnewscale::new_scale_color()+
  scale_color_manual(values = Darkened_Colours, guide = "none")+ ## Guide none is equivalent to show.legend = F
  stat_summary(fun.min = function(z) {mean(z) - sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z))},  ##Add SEM as error bars - i.e. standard dev/sqrt(x)
               fun.max = function(z) {mean(z) + (sqrt(sum((z - mean(z)) ^ 2/(length(z) - 1)))/sqrt(length(z)))},
               fun = mean, linewidth = 1,
               geom = "errorbar",
               linetype = "solid",
               width = 0.15,
               aes(group = Plasmid, y = Normalised_Response, color = Plasmid)) +
  scale_linetype_manual(values=c("solid","dashed", "dotted"))+
  geom_point(size = 3, aes(shape = Amino_Acid, color = Plasmid), show.legend = F) +
  theme(legend.position = c(0.1, 0.85),
        legend.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18)
        #legend.background = element_rect(colour = "dark grey", fill = "grey"),
        #legend.text = element_text(colour = "white")) +
  ) +
  scale_x_continuous( ## To add log ticks to x axis
    limits = c(min(long_vals_data$Drug_Conc), max(long_vals_data$Drug_Conc)), 
    breaks = (min(long_vals_data$Drug_Conc)):(max(long_vals_data$Drug_Conc)),
    guide = "prism_offset_minor",
    minor_breaks = log10(rep(1:9, 7)*(10^rep((min(long_vals_data$Drug_Conc)):(max(long_vals_data$Drug_Conc)-1), each = 9))),
    labels = function(lab) {
      do.call(
        expression,
        lapply(paste(lab), function(x) bquote(bold("10"^.(x))))
      )
    }
  ) +
  scale_y_continuous(breaks = c(0,25,50,75,100))



    
    

##OPTIONAL - VISUALISING ANY MODEL FITTING TO CHECK IF WORKS AS INTENDED------------------ 

##Plotting cAMP standard curve

r <- range(Standard_cAMP_Curve$log_Conc)
xNew <- seq(r[1],r[2],length.out = 200)
yNew <- predict(standard_curve_logistic,list(log_Conc = xNew))

plot(Standard_cAMP_Curve$log_Conc,Standard_cAMP_Curve$Average_log_val, xlab = "log[cAMP]", ylab = "Response")
lines(xNew,yNew, col = "red", lwd = 1)


## Plotting individual curves to extract param values

test_data <- long_vals_data_no_FSK %>%
  filter(Condition_Clean == "SNAP_BCNK")


## 4 parameter fitting - WITH STANDARD CURVE - - - - - - - - - - - -  - - - - - - - - - - - 
nls_4pl <- nlsLM(average_nM_cAMP ~ lower + (upper - lower) / (1 + (Drug_Conc / midpoint)^(slope)),
                 data = test_data,
                 start = list(lower = min(test_data$nM_cAMP), upper = max(test_data$nM_cAMP), midpoint = mean(test_data$Drug_Conc), slope = 1))
           # Initial guesses for the paramete)

x_seq <- seq(min(test_data$Drug_Conc), max(test_data$Drug_Conc), length.out = 100)
y_pred <- predict(nls_4pl, newdata = data.frame(Drug_Conc = x_seq))

# Plot the fitted curve
plot(test_data$Drug_Conc, test_data$average_nM_cAMP, main = "4-Parameter Logistic Model", xlab = "x", ylab = "y", pch = 19, col = "blue")
lines(x_seq, y_pred, col = "red", lwd = 2)

##- - - - - - - - - - - - - -  - - - - - - - - - - -  - - - - - - - - - - -  - - - - - - - - - - -
## 4 parameter fitting - NO STANDRAD CURVE
nls_4pl <- nlsLM(value ~ lower + (upper - lower) / (1 + (Drug_Conc / midpoint)^(slope)),
                 data = test_data,
                 start = list(lower = min(test_data$value), upper = max(test_data$value), midpoint = mean(test_data$Drug_Conc), slope = 1))
# Initial guesses for the paramete)

x_seq <- seq(min(test_data$Drug_Conc), max(test_data$Drug_Conc), length.out = 100)
y_pred <- predict(nls_4pl, newdata = data.frame(Drug_Conc = x_seq))

# Plot the fitted curve
plot(test_data$Drug_Conc, test_data$value, main = "4-Parameter Logistic Model", xlab = "x", ylab = "y", pch = 19, col = "blue")
lines(x_seq, y_pred, col = "red", lwd = 2)

coef(nls_4pl)




## 3 parameter fitting 

test_data <- long_vals_data %>%
  dplyr::filter(Plasmid == "ExD3")

nls_3pl <- nlsLM(value ~ lower + ((upper - lower) / (1 + 10^(midpoint-Drug_Conc))),
                 data = test_data,
                 start = list(lower = min(test_data$value), upper = max(test_data$value), midpoint = mean(test_data$Drug_Conc)),
                 control = nls.lm.control(maxiter = 1000))

nls_3pl_antag <- nlsLM(value ~ lower + ((upper - lower) / (1 + 10^(Drug_Conc - midpoint))),
                 data = test_data,
                 start = list(lower = min(test_data$value), upper = max(test_data$value), midpoint = mean(test_data$Drug_Conc)),
                 control = nls.lm.control(maxiter = 1000))

coef(nls_3pl_antag)
# Initial guesses for the paramete)

x_seq <- seq(min(test_data$Drug_Conc), max(test_data$Drug_Conc), length.out = 100)
y_pred <- predict(nls_3pl, newdata = data.frame(Drug_Conc = x_seq))

# Plot the fitted curve
plot(test_data$Drug_Conc, test_data$value, main = "3-Parameter Logistic Model", xlab = "x", ylab = "y", pch = 19, col = "blue")
lines(x_seq, y_pred, col = "red", lwd = 2)




