##CADDIS Photometry Analysis - - - - - - - - - - - -
##Package_Loading-----------------------
packages <- c("dplyr","stringr","tidyverse","reshape2","minpack.lm","ggprism","ggnewscale","readABF", "minpack.lm","signal", "patchwork") ## packages required to get code running

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))




##CODE-------------

abf_folder <- tcltk::tk_choose.dir(default = "~/")

ABF_Import <- function() {
  abf_filepaths <- list.files(path = abf_folder, pattern =".abf", full.names = TRUE, recursive = F) ## list all files ending in .abf
  abf_filelist <- lapply(abf_filepaths, readABF::readABF) ## import in all abf files from file list
  abf_filenames <<- gsub(".abf","", list.files(path = abf_folder, pattern =".abf", full.names = F, recursive = F)) ## get a list of all file names without file ext
  abf_formatted_list <- list() ## create empty list to store formatted dataframes in
  for (file in 1:length(abf_filelist)) {
    temp_abf <- abf_filelist[[file]] ## load in nth abf file from list
    sampling_interval <<- temp_abf$samplingIntervalInSec ## extract the sampling interval from header
    Timeseq <- seq(from = 0, by = sampling_interval, to = (lengths(temp_abf$data)*sampling_interval)-sampling_interval) ## generate a sequence based on sampling interval
    temp_abf_df <- data.frame(Time = Timeseq, Signal = temp_abf$data) ## create a dataframe from the raw data and time sequence
    colnames(temp_abf_df) <- c("Time","Signal") ## Force colnames - something doesnt work for signal
    abf_formatted_list[[abf_filenames[file]]] <- temp_abf_df ## add formatted data frame to list with list name from file name
  }
  return(abf_formatted_list) ## return list
} ## function to load and format all abf files in folder

my_list <- ABF_Import()

LOESS_Baseline_Correction <- function(ABF_List, cutoff_frequency = 25, LOESS_span = 0.4, downsample_factor = 250) {
  
  Exp_Corrected_Dataframe_List <- list() ## to store proceesed dfs
  
  failed_files <- c() ## to store character vector of unprocessed dfs
  
  #plot_list <- vector('list', length = length(ABF_List))
  
  for (df in 1:length(ABF_List)) {
    current_file_name <- abf_filenames[df]
    tryCatch({
      temp_dataframe <- ABF_List[[df]] ## Load in dataframe from list
      
      temp_dataframe$scaled_time <- (temp_dataframe$Time /1000) ## scale so that signal and time are roughly in the same scale 
      temp_dataframe <- temp_dataframe %>%  ## Align to 0
        mutate(Signal = Signal - min(Signal))
      
      samplingFreq <- 1/sampling_interval
      
      Nyquist_Freq <- samplingFreq/2 ## Look up Nyquist sampling -> CANNOT use a cutoff frequency over this value! - the highest frequency detectable in the data is 2x smaller than the rate sampled
      
      Crit_Frequency <- cutoff_frequency/Nyquist_Freq ## Normalising frequency for butterworth filter
      
      lowpass_filt <- butter(n = 4, W = Crit_Frequency, type = "low") 
      
      temp_dataframe$low_pass_signal <- filtfilt(lowpass_filt, temp_dataframe$Signal) ## apply lp filter to data
      
      temp_dataframe_downsampled <- temp_dataframe[seq(1, nrow(temp_dataframe), by = downsample_factor), ] ## Downsample to make LOESS run much faster
      
      loess_fit <- loess(low_pass_signal ~ scaled_time, data = temp_dataframe_downsampled, span = 0.4) ## perform local polynomial regression for baseline correction
      
      temp_dataframe$loess_baseline <- predict(loess_fit, newdata = temp_dataframe) ## generate y predictors based on LOESS polynomial
      
      temp_dataframe$normalised_Signal <- (temp_dataframe$Signal - temp_dataframe$loess_baseline) ## correct baseline 
      
      p1 <- ggplot(data = temp_dataframe, aes(x = scaled_time)) + 
        geom_line(aes(y = loess_baseline), color = "red") + 
        geom_line(aes(y = Signal), color = "green", alpha = 0.4)
      
      p2 <- ggplot(data = temp_dataframe, aes(x = scaled_time, y = normalised_Signal)) +
        geom_line()
      
      plot_layout <- p1 + p2
      
      plot_combined <- plot_layout + plot_annotation(
        title = "LOESS Fit and Corrected Signal",
        caption = paste("File Name:", abf_filenames[df])
      )
      
      print(plot_combined)
  
      Exp_Corrected_Dataframe_List[[abf_filenames[df]]] <- temp_dataframe ## save to list
      message("Successfully processed: ", abf_filenames[df]) ## log success
    }, error = function(e) {
      # --- This is the 'error' handler ---
      # This code ONLY runs if the block above fails.
      
      # 1. Print a helpful message to the console.
      message("-------------------------------------------------")
      message("ERROR processing file: ", current_file_name)
      message("The specific error was: ", e$message)
      message("Skipping to the next file.")
      message("-------------------------------------------------")
      
      # 2. Optionally, store the name of the failed file for later review.
      failed_files <<- c(failed_files, current_file_name)
    })
    
  }
  
  return(Exp_Corrected_Dataframe_List)
  
}

corrected_list <- LOESS_Baseline_Correction(ABF_List =  my_list, LOESS_span = 0.4, downsample_factor = 100)






dbl_exp_Baseline_Correction <- function(ABF_List, start_curve_cutoff) {
  
  Exp_Corrected_Dataframe_List <- list()
  
  failed_files <- c()
  
  for (df in 1:length(ABF_List)) {
    current_file_name <- abf_filenames[df]
    tryCatch({
      temp_dataframe <- ABF_List[[df]] ## Load in dataframe from list
      
      temp_dataframe$scaled_time <- (temp_dataframe$Time /1000) ## scale so that signal and time are roughly in the same scale - helps with nls down the line
      
      temp_dataframe <- temp_dataframe %>%  ## Align to 0
        mutate(Signal = Signal - min(Signal))
      
      temp_dataframe <- temp_dataframe[temp_dataframe$scaled_time > (start_curve_cutoff/1000),] ## Remove data from before n amount of time - required for accurate model fitting of region of data we care about
      
      c_start <- min(temp_dataframe$Signal) # The y offset term - 0 in this case but leave for same reason as below
      
      total_amp_start <- max(temp_dataframe$Signal) - c_start # Total amplitude of signal - should be equivalent to max but leave as this in case decided not to not normalise to 0
      
      log_tail_data <- subset(temp_dataframe, scaled_time > (1 - 0.3) * max(scaled_time))%>% ## look at final 30% of data, and log transform - this should almost always be in the stable portion of trace - change if required
        mutate(log_signal = log(Signal)) %>%
        filter(Signal > 0) ## log of 0 = infinity which breaks line below
      
      fit_slow_comp <- lm(log_signal ~ scaled_time, data = log_tail_data) ## fit linear model on log transformed data
      
      slope_slow <- coef(fit_slow_comp)[2] # extract gradient of linear model
      
      tau2_start <- (-1 / slope_slow) ## negative decay time constant can be estimated by 1/gradient of linear plot - akin to lineweaver burk estimation
      
      tau1_start <- tau2_start / 2 ## Assume fast phase is 10x faster than slow phase
      
      fit <- nlsLM( ## fit model curve to trimmed data - currently double exp decay but can be changed - perhaps a single exp may be better here but alas
        Signal ~ C + (A1*exp(-scaled_time / tau1)) + (-A2*exp(-scaled_time / tau2)), ## equivalent to dblexp_XOffset from IPro9
        data = temp_dataframe, 
        start = list(
          A1 = total_amp_start/2, ## based of aassumption above - fast phase is 1/10 of the amp of slow phase
          tau1 = tau1_start,
          tau2 = tau2_start,
          A2 = total_amp_start/2, ## as above
          C = c_start
        ),
        control = nls.control(maxiter = 300, warnOnly = TRUE) ## consider increasing iterations if inaccurate, but may not give any more accurate results and will baloon proc time
      )
      
      #### Extrapolate data from fitted model and asign to list - - - - -
      temp_dataframe_full <- ABF_List[[df]]
      temp_dataframe_full$scaled_time <- (temp_dataframe_full$Time /1000)
      temp_dataframe_full <- temp_dataframe_full %>%
        mutate(Signal = Signal - min(Signal))
      extrap_data <- predict(fit, newdata = temp_dataframe_full)
      temp_dataframe_full$normalised_Signal <- (temp_dataframe_full$Signal - extrap_data)
      temp_dataframe_full$fitted <- extrap_data
      Exp_Corrected_Dataframe_List[[abf_filenames[df]]] <- temp_dataframe_full
      message("Successfully processed: ", abf_filenames[df])
    }, error = function(e) {
      # --- This is the 'error' handler ---
      # This code ONLY runs if the block above fails.
      
      # 1. Print a helpful message to the console.
      message("-------------------------------------------------")
      message("ERROR processing file: ", current_file_name)
      message("The specific error was: ", e$message)
      message("Skipping to the next file.")
      message("-------------------------------------------------")
      
      # 2. Optionally, store the name of the failed file for later review.
      failed_files <<- c(failed_files, current_file_name)
    })
    
  }
  
  return(Exp_Corrected_Dataframe_List)
  
}

corrected_list <- dbl_exp_Baseline_Correction(my_list, 600)













test <- corrected_list[[4]]

ggplot(data = test, aes(x = scaled_time)) + 
  geom_line(aes(y = loess_baseline), color = "red") + 
  geom_line(aes(y = Signal), color = "green", alpha = 0.4)



## Fucking around with low pass filters and LOESS smoothing--------------------


single_dataframe <- my_list[[1]]
single_dataframe$scaled_time <- (single_dataframe$Time /1000)
single_dataframe <- single_dataframe %>%
  mutate(Signal = Signal - min(Signal))

ggplot(data = single_dataframe, aes(x = Time, y = Signal)) + 
  geom_line()

samplingFreq <- 1/sampling_interval

cutoff_freq <- 25

Nyquist_Freq <- samplingFreq/2

Crit_Frequency <- cutoff_freq/Nyquist_Freq

lowpass_filt <- butter(n = 4, W = Crit_Frequency, type = "low")

single_dataframe$low_pass_signal <- filtfilt(lowpass_filt, single_dataframe$Signal)

ggplot(data = single_dataframe, aes(x = Time, y = low_pass_signal)) + 
  geom_line()

downsample_scaling_fac <- 250

single_dataframe_downsampled <- single_dataframe[seq(1, nrow(single_dataframe), by = downsample_scaling_fac), ]

loess_fit <- loess(low_pass_signal ~ scaled_time, data = single_dataframe_downsampled, span = 0.4)

single_dataframe$loess_baseline <- predict(loess_fit, newdata = single_dataframe)

single_dataframe$normalised_Signal <- (single_dataframe$Signal - single_dataframe$loess_baseline)

ggplot(data = single_dataframe, aes(x = scaled_time)) + 
  geom_line(aes(y = loess_baseline), color = "red") + 
  geom_line(aes(y = Signal), color = "green", alpha = 0.4)

ggplot(data = single_dataframe, aes(x = scaled_time, y = normalised_Signal)) +
  geom_line()













  c_start <- min(single_dataframe$low_pass_signal) # The baseline y-value
total_amp_start <- max(single_dataframe$low_pass_signal) - c_start # Total amplitude
log_tail_data <- subset(single_dataframe, scaled_time > (1 - 0.3) * max(scaled_time))%>%
  mutate(log_signal = log(low_pass_signal)) %>%
  dplyr::filter(low_pass_signal > 0)

fit_slow_comp <- lm(log_signal ~ scaled_time, data = log_tail_data)

slope_slow <- coef(fit_slow_comp)[2]

tau2_start <- (-1 / slope_slow)

tau1_start <- tau2_start / 2

fit <- nlsLM(
  low_pass_signal ~ C + (A1*exp(-scaled_time / tau1)) + (-A2*exp(-scaled_time / tau2)),
  data = single_dataframe,
  start = list(
    A1 = total_amp_start/2,
    tau1 = tau1_start,
    tau2 = tau2_start,
    A2 = total_amp_start/2,
    C = c_start
  ),
  control = nls.control(maxiter = 600, warnOnly = TRUE) 
)



######---------------------




















test <- ABF_Import()

fit_cutoff <- 600/1000

single_dataframe <- test[[2]]
single_dataframe$scaled_time <- (single_dataframe$Time /1000)
single_dataframe <- single_dataframe %>%
  mutate(Signal = Signal - min(Signal))

ggplot(data = single_dataframe, aes(x = Time, y = Signal)) + 
  geom_line()

single_dataframe <- single_dataframe[single_dataframe$scaled_time > fit_cutoff,]

c_start <- min(single_dataframe$Signal) # The baseline y-value
total_amp_start <- max(single_dataframe$Signal) - c_start # Total amplitude
log_tail_data <- subset(single_dataframe, scaled_time > (1 - 0.3) * max(scaled_time))%>%
  mutate(log_signal = log(Signal)) %>%
  filter(Signal > 0)

fit_slow_comp <- lm(log_signal ~ scaled_time, data = log_tail_data)

slope_slow <- coef(fit_slow_comp)[2]

tau2_start <- (-1 / slope_slow)

tau1_start <- tau2_start / 2

coef(fit_slow_comp)

## Single exp-------
y0_start <- coef(fit_slow_comp)[1]
m_start <- coef(fit_slow_comp)[2]
single_dataframe$decay_component <- single_dataframe$Signal - (y0_start + m_start * single_dataframe$scaled_time)
a_start <- single_dataframe$decay_component[1]
decay_only_positive <- subset(single_dataframe, decay_component > 0)
decay_only_positive$log_decay <- log(decay_only_positive$decay_component)
fit_log_decay <- lm(log_decay ~ scaled_time, data = subset(decay_only_positive, scaled_time < 0.5 * max(scaled_time)))
slope_log_decay <- coef(fit_log_decay)[2]
tau_start <- -1 / slope_log_decay
start_params <- list(
  y0 = y0_start,
  m = m_start,
  a = a_start,
  tau = tau_start
)

linear_exp_decay <- function(t, y0, m, a, tau) {
  (y0 + m * t) + a * exp(-t / tau)
}

fit <- nlsLM(
  Signal ~ linear_exp_decay(scaled_time, y0, m, a, tau),
  data = single_dataframe,
  start = start_params,
  control = nls.control(maxiter = 1000)
)

# Fit the model using nls()-------------

fit <- nlsLM(
  Signal ~ C + (A1*exp(-scaled_time / tau1)) + (-A2*exp(-scaled_time / tau2)),
  data = single_dataframe,
  start = list(
    A1 = total_amp_start/2,
    tau1 = tau1_start,
    tau2 = tau2_start,
    A2 = total_amp_start/2,
    C = c_start
  ),
  control = nls.control(maxiter = 600, warnOnly = TRUE) 
)


loess

# See the summary of the fit to check the estimated parameters
summary(fit)

single_dataframe <- test[[6]]
single_dataframe$scaled_time <- (single_dataframe$Time /1000)
single_dataframe <- single_dataframe %>%
  mutate(Signal = Signal - min(Signal))

extrap_data <- predict(fit, newdata = single_dataframe)

single_dataframe$normalised_Signal <- (single_dataframe$Signal - extrap_data)

single_dataframe$fitted <- extrap_data

ggplot(data = single_dataframe, aes(x = scaled_time)) + 
  geom_line(aes(y = fitted), color = "red") + 
  geom_line(aes(y = Signal), color = "green", alpha = 0.4)

ggplot(data = single_dataframe, aes(x = scaled_time, y = normalised_Signal)) +
  geom_line()
