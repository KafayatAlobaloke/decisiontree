---
# title: "Classification (Supervised)"
# Author: Kafayat Alobaloke
# date: "2023-11-19"
---

install.packages("rpart", dependencies=TRUE)
install.packages("rpart.plot", dependencies=TRUE)
install.packages("rattle", dependencies=TRUE)
install.packages("party", dependencies=TRUE)
install.packages("partykit", dependencies=TRUE)
 
library(corrplot)
library(caTools)
library(tidyverse)
library(data.table)
library(dplyr)
library(party)
library(partykit)
library(tree)     
library(MASS)
library(ggplot2)
library(cowplot)
library(rpart) 
library(rpart.plot) 
library(rattle)
 
# Importing the data file
cmc <- read.csv("cmc.csv")

# Converting the data to a dataframe
data <- as.data.frame(cmc)

# Top rows of the data
head(data)
 
# EDA
# Summary of the data
summary(data)
 
# Understanding the datatype of dataset
str(data)

# Creating a flag for numerial variables age and number of children
 
# Creating the age flag for and define as WifeAge variable
data = data %>% mutate(HighWifeAge1 = if_else(WifeAge > 25,"Yes","No")) 
table(data$HighWifeAge)
 
# Creating the age flag for and define as WifeAge variable
data = data %>% mutate(HighNumChildren1 = if_else(NumChildren > 4,"Yes","No")) 
table(data$HighNumChildren)
 
head(data)

# Transforming the target and data variables that I will use to submit to the categorical algorithm into factors
ContraceptiveMethod = factor(data$ContraceptiveMethod, levels = c(1,2,3), labels = c("No_use", "Long_term", "Short_term"))
data = data[,-10]
HighWifeAge = factor(data$HighWifeAge1)
WifeEducation = factor(data$WifeEducation, levels = c(1,2,3,4), labels = c("Low", "Not_too_low", "Not_too_high", "High"))
data = data[,-2]
HusbandEducation = factor(data$HusbandEducation, levels = c(1,2,3,4), labels = c("Low", "Not_too_low", "Not_too_high", "High"))
data = data[,-2]
HighNumChildren = factor(data$HighNumChildren1)
WifeReligion = factor(data$WifeReligion, levels = c(0,1), labels = c("non_Islam", "Islam"))
data = data[,-3]
WifeWorking = factor(data$WifeWorking, levels = c(0,1), labels = c("Yes", "No"))
data = data[,-3]
HusbandOccupation = factor(data$HusbandOccupation)
data = data[,-3]
LivingStandardIndex = factor(data$LivingStandardIndex, levels = c(1,2,3,4), labels = c("Low", "Not_too_low", "Not_too_high", "High"))
data = data[,-3]
MediaExposure = factor(data$MediaExposure, levels = c(0,1), labels = c("Good", "Not_good"))
data = data[,-3]

head(data)
 
# Adding the newly recoded varaibles to our data
cmcdata <- cbind(ContraceptiveMethod, data, HighWifeAge, HighNumChildren, WifeEducation, HusbandEducation, WifeReligion, WifeWorking, HusbandOccupation, LivingStandardIndex, MediaExposure)

head(cmcdata)
 
# Understanding the new datatype of dataset
str(cmcdata)
 
# Exploratory data analysis
options(repr.plot.width = 4, repr.plot.height = 3)

## ggplot theme
theme <- theme(
  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
)

cmcdata %>%
  group_by(ContraceptiveMethod) %>%
  summarize(
    n = n()
  ) %>%
  mutate(
    percentage = round(n / sum(n), 3),
    n = NULL
  ) %>%
  ggplot(aes(x = ContraceptiveMethod, y = percentage)) + geom_col(aes(fill = ContraceptiveMethod)) + theme +
  geom_text(
    aes(x = ContraceptiveMethod, y = percentage, label = paste(percentage*100, "%", sep = ""))
  )
 
## ggplot theme
theme <- theme(
  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
  legend.position="none" 
)

# Decrease graph size from standard
options(repr.plot.width = 7, repr.plot.height = 3)

plot_grid(
  data %>%
    filter(ContraceptiveMethod == "Yes") %>%
    group_by(WifeAge) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
      aes(x = WifeAge, y = Percentage, color = WifeAge)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
      x = "WifeAge", y = "ContraceptiveMethod (%)"
    ),
  
  ggplot(
    data = cmcdata,
    aes(y = WifeAge, x = ContraceptiveMethod, color = ContraceptiveMethod)
  ) +
    theme +
    geom_boxplot()
  , align = "h")
 
# Decrease graph size from standard
options(repr.plot.width = 7, repr.plot.height = 3)
plot_grid(
  data %>%
    filter(ContraceptiveMethod == "Yes") %>%
    group_by(NumChildren) %>%
    summarize(
      n = n()
    ) %>%
    mutate(
      Percentage = round(n / sum(n), 3)
    ) %>%
    # Create plot
    ggplot(
      aes(x = NumChildren, y = Percentage, color = NumChildren)
    ) +
    stat_smooth(method = "lm", col = "red") +
    geom_point(alpha = 2/3) +
    # Clean graph visual a bit
    theme +
    labs(
      x = "NumChildren", y = "ContraceptiveMethod (%)"
    ),
  
  ggplot(
    data = cmcdata,
    aes(y = NumChildren, x = ContraceptiveMethod, color = ContraceptiveMethod)
  ) +
    theme +
    geom_boxplot()
  , align = "h")
 
# Contraceptive method by Wife Education
ggplot(cmcdata, aes(WifeEducation, fill = factor(ContraceptiveMethod))) +
  geom_bar(position = "dodge2") +
  labs(title = "Contraceptive Method by Education", fill = "Method")
 

# Contraceptive method by Media Exposure
ggplot(cmcdata, aes(MediaExposure, fill = factor(MediaExposure))) +
  geom_bar(position = "dodge2") +
  labs(title = "Contraceptive Method by Media Exposure", fill = "Exposure status")
 
#	Preprocessing
cmcdata %>%
  summarise_all(
    funs(sum(is.na(.)))
  )
 
# Deleting variables for the tree model
cmcdatatree = subset(cmcdata, select = c(-WifeAge, -NumChildren, -HighWifeAge1, -HighNumChildren1))
 
head(cmcdatatree)
 
# Splitting data into training and testing with 60 and 40 percent respectively
index <- sample(2, nrow(cmcdatatree), replace=TRUE, prob = c(0.60, 0.40))
traindata <- cmcdatatree[index==1, ]
testdata <- cmcdatatree[index==2, ]
 
# Building the ctree classification model
treefit <- ctree(ContraceptiveMethod ~., data = traindata)
plot(treefit, type="simple")
 
# Making prediction with the above model
predict <- predict(treefit, newdata = testdata[, -1], type="response")
matriz_conf <- table(testdata[,1], predict)
library(caret)
confusionMatrix(matriz_conf)