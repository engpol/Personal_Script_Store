library('dplyr')

my_data <- For_Teaching_SDH_Plots_Lineweaver_Burk %>%
  mutate(reciprocal_a = 1/Drug_Conc,
         uninhibited = 1/F2_Uninhibited,
         inhibited = 1/F2_Inhibited) %>%
  select(reciprocal_a,uninhibited,inhibited)

my_data_long <- my_data %>% 
  pivot_longer(
  cols = 2:last_col(), 
  names_to = "Inhibition_State"
)


ggplot(data = my_data_long, aes(x = reciprocal_a, y = value, color = Inhibition_State)) +
  geom_line() + 
  theme_bw()

my_data_individual <- my_data_long %>%
  filter(Inhibition_State == "inhibited")

my_model <- lm(data = my_data_individual, value ~ reciprocal_a)

coef(my_model)
