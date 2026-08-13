library('gganimate')
library('tidyverse')


my_model <- lm(data = iris_setosa, Sepal.Length ~ Petal.Length*Petal.Width)

summary(my_model)

# Set the directory where your files are located
dir_path <- tcltk::tk_choose.dir(default = "~/")

# Get a list of all files with a ".TIF" extension in the specified directory
files_to_rename <- list.files(dir_path, pattern = "\\.TIF$", full.names = TRUE)

# Check if there are any files to rename
if (length(files_to_rename) > 0) {
  # Create the new filenames by replacing ".TIF" with ".tiff"
  new_filenames <- gsub("\\.TIF$", ".tiff", files_to_rename)
  
  # Rename the files
  file.rename(files_to_rename, new_filenames)
  
  # Print a confirmation message
  cat("All files with a '.TIF' extension have been successfully renamed to '.tiff'.\n")
} else {
  # Print a message if no files are found
  cat("No files with a '.TIF' extension were found in the specified directory.\n")
}





iris_setosa <- iris %>%
  filter(Species == "virginica") 

func_solve_y <- function(x,m,c) {
  solved <- m*x + c
  return(solved)
}

0.6104680 = intercept
0.7500808 = Slope

number_of_iterations <- 137

mean(iris_setosa$Sepal.Length)

intercept_val_iterate <- seq(mean(iris_setosa$Petal.Length),-5, length.out = number_of_iterations)

intercept_Slope_iterate <- seq(0,1.6017, length.out = number_of_iterations)


iterate_df_values <- data.frame(intercept = intercept_val_iterate, slope = intercept_Slope_iterate)

iteration_num_values <- data.frame(slope = intercept_Slope_iterate, iteration_num = 1:number_of_iterations)


expanded_df <- iris_setosa[rep(1:nrow(iris_setosa), each = length(intercept_val_iterate)), ]
expanded_df$intercept <- rep(intercept_val_iterate, times = nrow(iris_setosa))

iris_setosa_animate <- expanded_df %>%
  select(c("Petal.Length","Sepal.Length","intercept"))


iris_setosa_animate <- merge(iris_setosa_animate, iterate_df_values, all.x = T) %>%
  mutate(y_solve = func_solve_y(x = Sepal.Length,m = slope, c = intercept),
         Square_of_residual = ((Petal.Length - y_solve)^2))

iris_setosa_animate <- merge(iris_setosa_animate, iteration_num_values, all.x = T) %>%
  group_by(iteration_num) %>%
  mutate(Sum_of_RS = round(sum(Square_of_residual), 2)) %>%
  filter(Sum_of_RS < 15)


iris_setosa_RSS_label <- iris_setosa_animate %>%
  select(c(iteration_num,Sum_of_RS))%>%
  unique()
  
iris_setosa_RSS_label$Sum_of_RS <- as.character(iris_setosa_RSS_label$Sum_of_RS)

ggplot(iris_setosa_animate, aes(x = Sepal.Length, y = Petal.Length)) + 
  geom_point() +
  #geom_point(aes(x=mean(Sepal.Length), y=mean(Petal.Length)), colour="red", size = 3) +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "17"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
  ylab("Petal Length (cM)") +
  xlab("Sepal Length (cM)") +
  geom_abline(aes(intercept=intercept,slope=slope), color = "red", linewidth = 1) +
  geom_label(data = iris_setosa_RSS_label, aes(x = 5.5, y = 6, label= paste0("RSS: ", Sum_of_RS)), size = 7) +
  #annotate("text", x = 5, y = 6, label = iris_setosa_RSS_label$Sum_of_RS) + 
  #geom_hline(yintercept = mean(iris_setosa$Petal.Length), color = "red", linewidth = 2, alpha = 0.35) +
  geom_segment(aes(x = Sepal.Length, y = y_solve, xend = Sepal.Length, yend = Petal.Length), color = "blue") +
  transition_states(
    iteration_num,
    transition_length = 2,
    state_length = 2, wrap = F
  ) +
  ease_aes('cubic-in-out')

  
exfolder <- tcltk::tk_choose.dir(default = "~/")

anim_save("Animation_1", animation = last_animation(), path = exfolder)




iris_setosa_animate_parabola <- iris_setosa_animate %>%
  select(c("Sum_of_RS","slope","iteration_num")) %>%
  filter(Sum_of_RS < 15) %>%
  expand(nesting(Sum_of_RS,slope),iris_setosa_animate_parabola$iteration_num) %>%
  mutate(iteration_num = `iris_setosa_animate_parabola$iteration_num`)


iris_setosa_animate_parabola_vline <- iris_setosa_animate %>%
  select(c("slope","iteration_num"))

iris_setosa_animate_parabola_vline <- unique(iris_setosa_animate_parabola_vline)

ggplot(data = iris_setosa_animate, aes(y = Sum_of_RS, x = slope)) +
  geom_point(aes(y=Sum_of_RS, x = slope), colour="red", size = 3) +
  geom_vline(data = iris_setosa_animate_parabola_vline, aes(xintercept = slope), alpha = 0.5, linewidth = 2, colour = "red") +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "17"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
    ylab("RSS") +
    xlab("Slope") +
  geom_label(data = iris_setosa_RSS_label, aes(x = 0.25, y = 4.5, label= paste0("RSS: ", Sum_of_RS)), size = 7) +
  transition_states(
    iteration_num,
    transition_length = 2,
    state_length = 2, wrap = F
  ) +
  ease_aes('cubic-in-out') +
  geom_line(data = iris_setosa_animate_parabola, aes(x = slope, y = Sum_of_RS))
  




ggplot(data = kleibers_law, aes(x = kleibers_law$`log _M`, y = log_RMR)) +
  geom_point(size = 3) + 
  theme(
    panel.grid.major = element_line(color = " light grey"),
    panel.grid.minor = element_line(color = " light grey"),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    #axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "17"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
  ylab("log(Resting Metabolic Rate (W))") +
  xlab("log(Mass (kg))")


ggplot(iris_setosa, aes(x = Sepal.Length, y = Petal.Length)) + 
  geom_point() +
  #geom_point(aes(x=mean(Sepal.Length), y=mean(Petal.Length)), colour="red", size = 3) +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "17"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = "none") +
   ylab("Petal Length (cM)") +
   xlab("Sepal Length (cM)") +
  geom_abline(aes(intercept=0.6104680,slope=0.7500808), color = "red", linewidth = 2, alpha = 0.35) +
  #geom_hline(yintercept = mean(iris_setosa$Petal.Length), color = "red", linewidth = 2, alpha = 0.35) +
  geom_segment(aes(x = Sepal.Length, y = func_solve_y(Sepal.Length,0.7500808,0.6104680), xend = Sepal.Length, yend = Petal.Length), color = "blue")
  

my_lm <- lm(data = iris_setosa, Petal.Length ~ Sepal.Length)


coef(my_lm)


plot(lm(data = iris_setosa, Petal.Length ~ Sepal.Length))

























x_val <- c(1,2,3,4,5,6,7,8,9,10)

y_val <- c(1,2,3,4,5,6,7,8,9,10)


simple_line <- data.frame(y=seq(from = 0.5, to = 5, by = 0.5),x=2:7) 





ggplot(data = simple_line, aes(x= x, y = y)) +
  geom_line() +
  scale_x_continuous(breaks = c(1:10)) +
  scale_y_continuous(breaks = c(1:10))
+
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "17"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 19))

plot(x_val,y_val, type = "o")

plot


