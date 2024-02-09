# Any problems let me know and I can sort it! - Tyson
# This analysis utilises Reed-Muench method from 10.1093/oxfordjournals.aje.a118408

# Remember Ctrl + Shift + Enter can be used to run the whole script.
# When running the first time, may need to scroll up in the console past the rm-related errors

# If you ever fuck up the work directory session -> set working directory and do it as project directory

#### Standard Package to install ###
install.packages("dplyr")
install.packages("janitor")
install.packages("moments")
library(dplyr)
library(readxl)
library(tidyr)
library(janitor)
library(ggplot2)
library(moments)
library(ggpubr)




# Creating Labels and Sequences for sample names

# print today's date for file names
today <- Sys.Date()
date <- format(today, format = "%y%m%d")

# Make Creates a dataframe with A1, A2, A3, B1, B2 ...
s <- 1:2
Alpha <- rep(LETTERS[7:12], each = 2)
Samples <- paste(Alpha, s, sep = "")
Samples <- rep(Samples, each = 6)
write.csv(Samples, "Labels.csv", row.names = FALSE)

#### First Step: Import Dataset -> Your Data using template, then proceed ####

# Change this to your Dataset name
Data <- read_excel("230104_TCID50_IFB.xlsx")
View(Data)


#Sets number of replicate wells (default 8)
REPWells = 8

# If you need to delete the dataframes \/ (do at start of each analysis)
rm(Dataset)
rm(Results)
rm(c)
rm(d)
rm(e)
rm(f)
rm(index)

# Run me (housekeeping stuff to sort the data/prepare for analysis)
Dataset <-
  data.frame(
    "Sample",
    "Dilution (a)",
    "Alive (b)",
    "Dead (c)",
    "Cum. Alive (d)",
    "Cum. Dead (e)",
    "Percent Mortality (f)"
  )
Results <- data.frame("Sample", "TCID50", "IU/ml")
index <- data.frame("Index")
a0 = unlist(c(Data[1]))
b0 = unlist(c(Data[2]))
s0 = unlist(c(Data[3]))
c <- data.frame("Dead")
d <- data.frame("Cum. Alive")
e <- data.frame("Cum. Dead")
f <- data.frame("Perc. Mortality")
a = matrix(a0, nrow = 1, ncol = nrow(Data))
b = matrix(b0, nrow = 1, ncol = nrow(Data))
s = matrix(s0, nrow = 1, ncol = nrow(Data))
k = 1
val = 1:nrow(Data)
val2 = 1:(nrow(Data) - 1)
e0 = 0
sample2 = 0
sample = 1
yes = 0


# Generates index for further analysis
index[nrow(index) + 1, ] = c(1)
for (i in val2) {
  if (s[1, i] != s[1, i + 1]) {
    index[nrow(index) + 1, ] = c(i + 1)
    
  }
  
}


# Processes a-f dataset
index[nrow(index) + 1, ] = ncol(b)
z = 2
for (z in 2:(nrow(index) - 1)) {
  d0 = sum(b[1, index[z, 1]:((as.numeric(index[z + 1, 1])) - 1)])
  yes = 1
  c0 = 0
  e0 = 0
  f0 = 0
  for (i in index[z, 1]:((as.numeric(index[z + 1, 1])) - 1)) {
    c0 = REPWells - b[1, i]
    c[nrow(c) + 1, 1] = c(c0)
    
    e0 = e0 + c0
    e[nrow(e) + 1, 1] = c(e0)
    
    f0 = (d0 / (d0 + e0)) * 100
    f[nrow(f) + 1, 1] = c(f0)
    
    Dataset[nrow(Dataset) + 1, ] = c(s[i], a[1, i], b[1, i], c0, d0, e0, round(f0, digits =
                                                                                 2))
    
    d0 = d0 - b[1, k]
    k = k + 1
    
  }
  
  
}
i = i + 1
c0 = REPWells - b[1, i]
c[nrow(c) + 1, 1] = c(c0)

e0 = e0 + c0
e[nrow(e) + 1, 1] = c(e0)

f0 = (d0 / (d0 + e0)) * 100
f[nrow(f) + 1, 1] = c(f0)

Dataset[nrow(Dataset) + 1, ] = c(s[i], a[1, i], b[1, i], c0, d0, e0, round(f0, digits =
                                                                             2))


# Calculated the TCID50 and IU/ml for the dataset

for (z in 2:(nrow(index) - 1)) {
  for (i in (as.numeric(index[z, 1]) + 1):((as.numeric(index[z + 1, 1])))) {
    if (as.numeric(f[i, 1]) >= 50 && as.numeric(f[i + 1, 1]) < 50) {
      g = f[i, 1]
      h = f[i + 1, 1]
      l = i
      concentration = abs(as.numeric(a[1, l - 1]))
      propdist = ((as.numeric(g)) - 50) / (as.numeric(g) - as.numeric(h))
      TCID50 = concentration + propdist
      IUpml = 0.69 * 20 * (10 ^ TCID50)
      Results[nrow(Results) + 1, ] = c(s[i],
                                       round(TCID50, digits = 4),
                                       formatC(IUpml, format = "e", digits = 3))
      
    }
  }
  
}

# Visualize the processed dataset
Dataset

# Visualize TCID50 and IU/ml
Results



# Write to your dataset as an excel file, you will have to edit below file pathway
name <- paste(date, "TCID50", "Data", "DCV", sep = "_")
csv_name <- paste(name, "csv", sep = ".")
write.csv(Dataset, file = csv_name, row.names = FALSE)


# Write censored data to your drive (if you wish)
name <- paste(date, "TCID50", "Results", "DCV", sep = "_")
csv_name <- paste(name, "csv", sep = ".")
write.csv(Results, file = csv_name, row.names = FALSE)

### Data analysis ###

# Tidying the data
Results <- Results[-1, ]

Results_tidy <- Results %>%
  rename(sample = X.Sample.,
         tcid50 = X.TCID50.,
         IU_mL = X.IU.ml.)

# Adds in the treatment details (wol and tet) and virus (DCV or FHV)
treatment <- rep(c("tet", "wol"), each = 6)

Results_tidy <- Results_tidy %>%
  mutate(treatment = treatment) %>%
  mutate (virus = "DCV")

# converts IU_mL into numeric data
Results_tidy$IU_mL <- as.numeric(Results_tidy$IU_mL)

# Adding in the infectivity ratio
Results_tidy_1 <- Results_tidy %>%
  mutate (infectivity_ratio = IU_mL / 1e+12)

# View the tidy data
Results_tidy_1

# Saving the tidy Data
name <- paste(date, "TCID50", "Results", "DCV", "Tidy" sep = "_")
csv_name <- paste(name, "csv", sep = ".")
write.csv(Results_tidy_1, file = csv_name, row.names = FALSE)


### Combining DCV and FHV datasets ###

# Adding the DCV and FHV data together
DCV <- read.csv("Infectivity_DCV.csv")
FHV <- read.csv("Infectivity_FHV.csv")
joined <-
  full_join(FHV,
            DCV,
            by = c(
              "sample",
              "tcid50",
              "IU_mL",
              "treatment",
              "infectivity_ratio",
              "virus"
            ))

# Creating Biol_rep column
labels <- rep(LETTERS[1:12], each = 2)
joined <- joined %>%
  mutate(biol_rep = labels)

# Doing summary

grouped <- joined %>%
  group_by(biol_rep) %>%
  mutate(average_infectivity_ratio = mean(infectivity_ratio)) 


name <- paste(date, "Analysed", "FHV", "DCV", sep = "_")
csv_name <- paste(name, "csv", sep = ".")
write.csv(grouped, file = csv_name, row.names = FALSE)


## Transforming the Data before conducting analysis
# Transforming the data 

data <- read.csv("analysed/230131_Analysed_FHV_DCV.csv")

c <- rep(1e+12, times =24) 

data <- data %>% 
  mutate(total_virus = c) %>% 
  mutate(log_total_virus = log(total_virus)) %>% 
  mutate(log_IU_ml = log(IU_mL)) %>% 
  mutate(log_infectivity_ratio = log_IU_ml/log_total_virus)

data_1 <- data %>%
  group_by(biol_rep) %>%
  mutate(average_log_infectivity_ratio = mean(log_infectivity_ratio)) 

name <- paste(date, "TCID50", "Data", "FHV", "DCV", "Tidy", sep = "_")
csv_name <- paste(name, "csv", sep = ".")
write.csv(data_1, file = csv_name, row.names = FALSE)



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
