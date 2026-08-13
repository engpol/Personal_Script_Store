library('dplyr')
library('tidyr')
library('forcats')
library('ggplot2')
library('viridis')
library('tidyverse')
library('broom')

Data_Import <- read_excel("C:\\Users\\olikc\\Downloads\\MET_Comp_Quant.xlsx")


Data_Cleaned <- Data_Import %>%
  mutate(Region = fct_recode(Region, 
                             "ARH" = "ARC",
                             "OV/AVPN" = "OVLT/AVPN",
                             "VL" = "Lat_Ven",
                             "V4" = "4th_Ven"
  )) %>%
  mutate(Drug_Group = gsub(pattern = "_\\d", replacement = "", Brain_ID)) %>%
  group_by(Brain_ID,Region) %>%
  mutate(Avg_Intensity = mean(Mean_Int)) %>%
  select(c(Region,Avg_Intensity,Brain_ID,Drug_Group)) %>%
  dplyr::filter(Brain_ID != "MET097_5") %>%
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
  mutate(Region = fct_relevel(Region, "ME","ARH","OV/AVPN","NTS","AP","V4","SFO","VL"))

ggplot(data=Drug_Avg_Data_Fold, aes(x = Brain_ID, y = Region, fill = Fold_Change)) +
  geom_tile() +
  geom_text(aes(label = round(Fold_Change,2), color = ifelse(Fold_Change < 3.2, "white", "black")), size = 4) +
  scale_color_identity() +
  scale_fill_viridis_c(breaks = c(2,2.5,3,3.5,4,4.5,5)) +
  facet_wrap(~Drug_Group, scales = "free_x") + 
  theme(
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12)
  ) + 
  labs(fill = "log2(fold change)")


Drug_Avg_Data_Fold_Log <- Drug_Avg_Data_Fold %>%
  group_by(Drug_Group, Region) %>%
  mutate(Group_Avg_Intensity = mean(Fold_Change)) %>%
  select(Group_Avg_Intensity, Drug_Group, Region) %>%
  unique()

write.csv(Drug_Avg_Data, "C:\\Users\\oc124\\Desktop\\Real_Data\\Brains\\MET097_vs_H1D3_Man_Quant\\Group_Avg.csv")
write.csv(Drug_Avg_Data_Fold_Log, "C:\\Users\\olikc\\Desktop\\PhD_Research\\Billy_MET097\\Log_Data.csv")

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



cars <- ggplot(mtcars, aes(mpg, factor(cyl)))
cars + geom_point()
cars + stat_bin_2d(aes(fill = after_stat(count)), binwidth = c(3,1))
cars + stat_bin_2d(aes(fill = after_stat(density)), binwidth = c(3,1))

cars +
  stat_density(
    aes(fill = after_stat(density)),
    geom = "raster",
    position = "identity"
  )
cars +
  stat_density(
    aes(fill = after_stat(count)),
    geom = "raster",
    position = "identity"
  )

