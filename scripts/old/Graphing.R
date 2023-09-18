### Graphing the Data ###

# Graphing data boxplot

# Loading the data fram 

grouped <- read.csv("analysed/230131_Analysed_FHV_DCV.csv")

ggplot(grouped) +
  geom_boxplot(aes(treatment, average_infectivity_ratio))

# Graphing scatter plot
ggplot(grouped) +
  geom_jitter(aes(treatment, average_infectivity_ratio, col = virus))



#### Not relevant but useful for trouble shooing ###
grouped_1 <- grouped %>%
  mutate (average_IU_mL = mean(IU_mL)) %>%
  mutate (average_infectivity_ratio = mean(infectivity_ratio))

# Checking that the order of when you do the averaging doesn't matter

grouped_2 <- grouped_1 %>%
  mutate (infect_using_average_IU_mL = average_IU_mL / 1e+12)
rm(grouped_2)
rm(grouped_1)


#### Not relevant but useful for trouble shooing ###
grouped_1 <- grouped %>%
  mutate (average_IU_mL = mean(IU_mL)) %>%
  mutate (average_infectivity_ratio = mean(infectivity_ratio))

# Checking that the order of when you do the averaging doesn't matter

grouped_2 <- grouped_1 %>%
  mutate (infect_using_average_IU_mL = average_IU_mL / 1e+12)
rm(grouped_2)
rm(grouped_1)
