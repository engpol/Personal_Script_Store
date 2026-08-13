data_long <- pivot_longer(data = Photobleaching, cols = 2:3, names_to = "Dye", values_to = "Values")

Raw_colour_vector <- c('royalblue1', 'gold2') ##Select colours for Plasmids here 


ggplot(data = data_long, aes(x = Timepoint, y = Values, color = Dye)) +
  geom_smooth(se = F) +
  scale_color_manual(values = Raw_colour_vector) +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor = element_line(color = " light grey"),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "17"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 19),
    legend.position = c(0.82, 0.85),
    legend.text = element_text(size = 18),
    legend.title = element_blank()) +
    scale_x_continuous(breaks = c(1:15)) +
    #legend.position = "none") +
  ylab("Fluoresence Intensity (AU)") +
  xlab("Timepoint (min)")
