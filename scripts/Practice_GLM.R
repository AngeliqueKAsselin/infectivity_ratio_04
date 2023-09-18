## GLM Example ##
# Datacamp
# https://app.datacamp.com/workspace/w/1d6fc927-776a-4ebe-953b-28c7846f4baf/edit

install.packages(c("ISLR", 
                   "Amelia", 
                   "mlbench", 
                   "corrplot", 
                   "caret"))

library(Amelia)
library(mlbench)
library(corrplot)
library(caret)

source("requirements.R")

# Looks at the aspects of the dataset 

df <- Smarket
names(Smarket)
head(df)
summary(Smarket)

# Shows histogram of the data set to get an idea of the spread of data

par(mfrow = c(1, 8))
for (i in 1:8) {
  hist(Smarket[, i], main = names(Smarket)[i])
}

# Check for missing data in data set

missmap(Smarket, col = c("blue", "red"), legend = TRUE)

# Checks the correlation between numeric variables 


correlations <- cor(Smarket[, 1:8])
corrplot(correlations, method = "circle")

pairs(Smarket, col=Smarket$Direction)



x <- Smarket[, 1:8]
y <- Smarket[, 9]
scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))
featurePlot(
  x = x,
  y = y,
  plot = "density",
  scales = scales
)


# Fitting the GLM to the data 
glm.fit <-
  glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
      data = Smarket,
      family = binomial)
summary(glm.fit)
