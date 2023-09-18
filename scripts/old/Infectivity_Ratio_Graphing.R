### Graphing the Data ###
# Loading the library 
install.packages("ggplot2")
library(ggplot2)

# Importing the Data # 

df <-read.csv("analysed/230131_Analysed_FHV_DCV.csv")

# Graphing data boxplot

ggplot(df) +
  geom_boxplot(aes(treatment, average_infectivity_ratio))

# Graphing scatter plot
ggplot(df) +
  geom_jitter(aes(treatment, 
                  average_infectivity_ratio, 
                  col = virus),
                  width = 0.05,
              alpha = 0.6)


