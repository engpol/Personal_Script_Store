library('dplyr')
library('tidyr')
library('forcats')
library('readxl')
library('ggplot2')
library('viridis')
library('tidyverse')
library('broom')
library('svglite')
source("C:\\Users\\olikc\\OneDrive\\Documents\\R_Projects\\PhD_Research\\Useful_Functions.R")
#Data_Import <- read_excel("C:\\Users\\oc124\\Desktop\\Real_Data\\Brains\\ExD3_vs_ExF1_Ex4Cy5\\Quant\\Manual_Quant_F1_D3_Ex_4.xlsx")

Data_Import$Mean_Int <- as.numeric(Data_Import$Mean_Int) #in case of na

Data_Cleaned <- Data_Import %>%
  mutate(Region = fct_recode(Region,
                             "ARH" = "ARC",
                             "OV" = "OVLT/AVPN",
                             "VL" = "Lat_Ven",
                             "V4" = "4th_Ven"
  )) %>%
  mutate(Drug_Group = gsub(pattern = "_\\d.*", replacement = "", Brain_ID)) %>%
  dplyr::filter(!(Brain_ID %in% c("BAD_ExF1_8","ExD3_Cy5_1_BAD"))) %>%
  group_by(Brain_ID,Region) %>%
  mutate(Avg_Intensity = mean(Mean_Int)) %>%
  select(c(Region,Avg_Intensity,Brain_ID,Drug_Group)) %>%
  unique()

Drug_Avg_Data <- Data_Cleaned %>%
  group_by(Drug_Group, Region) %>%
  mutate(Group_Avg_Intensity = mean(Avg_Intensity)) %>%
  select(Group_Avg_Intensity, Drug_Group, Region) %>%
  unique()


Saline_Group_Avg <- Drug_Avg_Data %>%
  dplyr::filter(Drug_Group=="Saline") %>%
  ungroup() %>%
  select(Group_Avg_Intensity, Region) %>%
  rename(Saline_Avg_Intensity = Group_Avg_Intensity)


Drug_Avg_Data_Fold <- merge(Data_Cleaned, Saline_Group_Avg) %>%
  mutate(Fold_Change = log2(Avg_Intensity/Saline_Avg_Intensity)) %>%
  dplyr::filter(Drug_Group!="Saline") %>%
  mutate(Region = fct_relevel(Region, "ME","ARH","OV","NTS","AP","V4","SFO","VL"))


Drug_Avg_Data_Fold$Brain_ID <- factor(
  Drug_Avg_Data_Fold$Brain_ID, 
  levels = sample(unique(Drug_Avg_Data_Fold$Brain_ID))
)

p <- ggplot(data=Drug_Avg_Data_Fold, aes(x = Brain_ID, y = Region, fill = Fold_Change)) +
  geom_tile() +
  geom_text(aes(label = round(Fold_Change,2), color = ifelse(Fold_Change < 3.2, "white", "black")), size = 4) +
  scale_color_identity() +
  scale_fill_viridis_c(breaks = c(1,1.5,2,2.5,3,3.5,4),
                       limits = c(0.9,4.1)) +
  facet_wrap(~Drug_Group, scales = "free_x") +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12)
  ) +
  labs(fill = "log2(fold change)")


Raw_colour_vector <- c("#075694ff", '#ff2c90ff', "green") ##Select colours for F1_D3 here
Darkened_Colours <- darken(Raw_colour_vector,factor = 2) ## Darkened Colours for data points - think it looks slightly better maybe


ggplot(data=Drug_Avg_Data_Fold, aes(x = Region, y = Fold_Change, fill = Drug_Group)) +
  scale_color_manual(values = Raw_colour_vector) +
  scale_fill_manual(values = Raw_colour_vector) + 
  stat_summary(fun = "mean", geom = "col", alpha = 1, position = position_dodge(width = 0.9)) +
  ggnewscale::new_scale_color() +
  stat_summary(fun.data = mean_se, geom = "errorbar",  position = position_dodge(width = 0.9), size = 1, width = 0.6, aes(color = Drug_Group)) +
  scale_color_manual(values = Darkened_Colours, guide = "none") +
  geom_jitter(size = 3, aes(color = Drug_Group, alpha = 0.75), show.legend = FALSE, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9)) +
  theme(
    #panel.grid.major = element_line(color = " light grey"),
    #panel.grid.minor =element_line(color = "light grey"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    #axis.line = element_blank(),
    #axis.line = element_line(color = "black", linewidth = 1),
    panel.background = element_rect(fill = "transparent", size=0.5),
    axis.title = element_text(size = "16"),
    #strip.text = element_blank(),
    strip.background = element_blank(),
    axis.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 19)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  ylab("log2(fold change)") + 
  xlab("Region")



Drug_Avg_Data_Fold_Log <- Drug_Avg_Data_Fold %>%
  group_by(Drug_Group, Region) %>%
  mutate(Group_Avg_Intensity = mean(Fold_Change)) %>%
  select(Group_Avg_Intensity, Drug_Group, Region) %>%
  unique()


ggsave(path = "C:\\Users\\olikc\\Downloads", filename = "heatmap_fixed_paulo.svg", plot = p, width = 11, height = 5,units = "in",device = svglite::svglite)


#### - - - - - - - - - -

## TWO FACTOR LEVELS - T TEST
results <- Drug_Avg_Data_Fold %>%
  group_by(Region) %>%
  do(tidy(t.test(Fold_Change ~ Drug_Group, data = .))) %>%
  ungroup()

final_table <- results %>%
  select(Region, statistic, p.value, estimate, conf.low, conf.high) %>%
  mutate(
    significant = ifelse(p.value < 0.05, "*", "") # Add a flag for significance
  )

print(final_table)

My_model <- lm(data = Data_Cleaned, Avg_Intensity ~ Drug_Group:Region)
TukeyHSD(aov(My_model))


## TWO + - ANOVA 

# 1. Run the ANOVA and Tukey's HSD per Region
posthoc_results <- Drug_Avg_Data_Fold %>%
  group_by(Region) %>%
  # Run the aov(), pass it to TukeyHSD(), and tidy() the results into a dataframe
  do(tidy(TukeyHSD(aov(Fold_Change ~ Drug_Group, data = .)))) %>%
  ungroup()

final_posthoc_table <- posthoc_results %>%
  select(Region, contrast, estimate, conf.low, conf.high, adj.p.value) %>%
  mutate(
    # Force specific columns to standard notation
    adj.p.value = format(adj.p.value, scientific = FALSE),
    estimate = format(estimate, scientific = FALSE),
    
    # Optional: You can also round them at the same time to keep it tidy
    # adj.p.value = format(round(adj.p.value, 4), scientific = FALSE),
    
    significant = ifelse(adj.p.value < 0.05, "*", "")
  )

write.csv(final_table, "C:\\Users\\oc124\\Desktop\\Real_Data\\Brains\\ExD3Cy5_vs_ExF1Cy5\\Statistics.csv")