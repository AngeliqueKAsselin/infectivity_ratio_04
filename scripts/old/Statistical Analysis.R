### Fiting the GLM to the data ### 
# Created: 16/02/23

# Approach #

# GLM for a ratio (0,1)
# Logitic regression GLM( y ~x, family = binomial)
# IR ~ Wolbachia + Virus (No interaction)
# IR ~ Wolbachia * Virus 

## Steps ## 
# 1. Transform 
# 2. t-test 
# 3. GLM 
# 4. Compare outputs 
# 5. Choose presentation 


# Reading the data 

data <- read.csv("analysed/230131_Analysed_FHV_DCV.csv")

# Transforming the data 
c <- rep(1e+12, times =24) 

data <- data %>% 
  mutate(total_virus = c) %>% 
  mutate(log_total_virus = log(total_virus)) %>% 
  mutate(log_IU_ml = log(IU_mL)) %>% 
  mutate(log_infectivity_ratio = log_IU_ml/log_total_virus)

data_1 <- data %>%
  group_by(biol_rep) %>%
  mutate(average_log_infectivity_ratio = mean(log_infectivity_ratio)) 

## Check the assumptions of normality of the data ##

# Distribution of ratio variable compared to normal
# Normality of un-transformed data
ggdensity(data_1, x = "average_infectivity_ratio", fill = "grey")+
  stat_overlay_normal_density(color = "red", linetype = "dashed")
skewness(data_1$average_infectivity_ratio, na.rm = TRUE) # 1.15 


# Normality of transformed data 
ggdensity(data_1, x = "average_log_infectivity_ratio", fill = "grey")+
  stat_overlay_normal_density(color = "red", linetype = "dashed")
skewness(data_1$average_log_infectivity_ratio, na.rm = TRUE) # -0.314

## Collapsing the dataframe so there is only one point for each biol rep that is the average of two tech reps 

c <- seq(from = 1, by =2, length.out = 12)
data_2 <- data_1[c,]



## GLM ## 

model_1 <- glm(formula = average_log_infectivity_ratio ~ treatment * virus,
             family = binomial, 
             data = data_2)
summary(model_1)

model_2 <- glm(formula = average_log_infectivity_ratio ~ treatment + virus,
             family = binomial, 
             data = data_2)
summary(model_2)

# Seems to be no impact of virus or treatment on the infectivity ratio


## T-test ## 
#DCV 
result <- t.test(x = data_2$average_log_infectivity_ratio,
       y = data_2$treatment)


#FHV 

        



