## README

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

Classification assigns items in a collection to target categories or classes. Its goal is to accurately predict the target class for each case in the data. One example where a classification model could be used is to identify loan applicants as low, medium, or high credit risks.

This analysis focuses classification approach using Decision Trees. A decision tree is a tool that uses a tree-like model of decisions and their possible outcomes. Each node represents a test on an attribute and each branch represents an outcome of that test. This assignment requires using decision tree (ctree) analysis as a classification tool to be applied to a data mining study within your domain of interest using R and RStudio. You need to provide a report on this.

# Using decision tree to classify the contraceptive method used by women
By leveraging decision tree modeling, we aim to uncover the most influential factors that contribute to the choice of contraceptive method among women. This can help healthcare providers tailor their counseling and support services, empower women in addressing barriers to contraception access and utilization. In summary, the decision tree approach is a well-founded and practical choice for this analysis, given its ability to provide actionable insights into a complex and important public health issue.

#	Understanding the Data
The dataset for this was selected from the module’s recommended datasets list.  It is a subset of the 1987 National Indonesia Contraceptive Prevalence Survey. The samples are married women who were either not pregnant or do not know if they were at the time of interview. Here is the Definitions of the columns of the data.

Description of Data Variables
Variable Name	Code	Description
Wife's age	Numerical	Age of the wife
Wife's education	Categorical
1=low, 2, 3, 4=high	Education level of wife
Husband's education	Categorical
1=low, 2, 3, 4=high	Education level of husband
Number of children ever born	Numerical	Number of children ever born by the wife
Wife's religion	Binary
0=non-Islam, 1=Islam	Religion of wife
Wife's now working	Binary
0=Yes, 1=No	Occupation status of wife
Husband's occupation	Categorical
1, 2, 3, 4	Occupation status of husband
Standard-of-living index	Categorical
1=low, 2, 3, 4=high	Living status
Media exposure	Binary
0=Good, 1=Not good	Exposure to media/ICT
Contraceptive method used	Class attribute
1=No-use, 2=Long-term, 3=Short-term	Choice of contraceptive method by the wife

# Exploratory Analysis
Using the ‘str’ function, it was observed that the variables in the dataset consist of integers. The summary function was used to obtain the minimum and maximum, as well as measures of central tendency (mean, median) and spread (1st and 3rd quartiles) for each of these variables.
Attempt was made to create a new binary variable for the wife age and number of children i.e., ‘WifeAge’ and ‘NumChildren’ based on the condition of the wife's age being greater than 25 and number of children being greater than 4. The frequency table for this new variable to count the occurrences of "Yes" and "No" values is shown below.

```{r, echo=FALSE}
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
```

```{r, echo=FALSE}
cmc <- read.csv("cmc.csv")
data <- as.data.frame(cmc)
head(data)
summary(data)
str(data)
```

```{r, echo=FALSE}
data = data %>% mutate(HighWifeAge1 = if_else(WifeAge > 25,"Yes","No")) 
table(data$HighWifeAge)
data = data %>% mutate(HighNumChildren1 = if_else(NumChildren > 4,"Yes","No")) 
table(data$HighNumChildren)

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
```

```{r, echo=FALSE}
cmcdata <- cbind(ContraceptiveMethod, data, HighWifeAge, HighNumChildren, WifeEducation, HusbandEducation, WifeReligion, WifeWorking, HusbandOccupation, LivingStandardIndex, MediaExposure)
```

# Variable Analysis
42.7% of women does not use contraceptive method, 22.6%  uses a long term contraceptive method, while 34.7%  uses a short term contraceptive method.
Older women do not seem to be using contraceptive. Also, women with the more children tend to use a long-term contraceptive.

```{r, echo=FALSE}
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
```

```{r, echo=FALSE}
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
```

```{r, echo=FALSE}
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
```

```{r, echo=FALSE}
ggplot(cmcdata, aes(WifeEducation, fill = factor(ContraceptiveMethod))) +
    geom_bar(position = "dodge2") +
    labs(title = "Contraceptive Method by Education", fill = "Method")

ggplot(cmcdata, aes(MediaExposure, fill = factor(MediaExposure))) +
    geom_bar(position = "dodge2") +
    labs(title = "Contraceptive Method by Media Exposure", fill = "Exposure status")

cmcdata %>%
    summarise_all(
        funs(sum(is.na(.)))
    )

# Deleting variables for the tree model
cmcdatatree = subset(cmcdata, select = c(-WifeAge, -NumChildren, -HighWifeAge1, -HighNumChildren1))
```

# Preprocessing - Check for missing data
Next, we check how many NA records we have, per column. Fortunately, there are no records of missing data in the dataset as shown below

# Model Fitting
The dataset was divided into training and test set using R code, after which we perform the decision tree model and evaluate the confusion matrix with model methods such as sensitivity, specificity, positive prediction value, negative prediction value, and prevalence of the data.
From above tree flow, we can have our inferences like the wife age and education level are the most significant variables as they tell us which contraceptive method is mostly used by women. 
It shows 4 terminal nodes and a vector representing the proportion of instances in the node representing one of the 3 class values. For example, terminal node 5 states that 122 instances apply to this exact classification. 50% of married women out of the 122 do not use contraceptive, for terminal node 7, 63.9% of women out of 338 use a long term contraceptive
Education, being of the highest significance and our first assumption for the likelihood of contraception use, may have played a part in the level of accuracy of the model. Additionally, it was interesting to start off with the fact that the survey gave no option for a husband to not be holding an occupation. For instance, if the wife was working a job, then we do not know if the husband also was working or was not. This is a factor that we were not able to consider.

# Model Properties
We observed 43% accuracy of the model on test data i.e 43% of the test values are correctly classified, and mis-classification rate is around 57%. The rate at which there was no information, i.e., "No Information Rate" is 60%.
This p-value is associated with a hypothesis test comparing the model's accuracy to the accuracy achieved by the "No Information Rate." In this case, the p-value is 1, which suggests that there is no statistically significant difference between the model's accuracy and the "No Information Rate."

```{r, echo=FALSE}
# Splitting data into training and testing with 60 and 40 percent respectively
index <- sample(2, nrow(cmcdatatree), replace=TRUE, prob = c(0.60, 0.40))
traindata <- cmcdatatree[index==1, ]
testdata <- cmcdatatree[index==2, ]

treefit <- ctree(ContraceptiveMethod ~., data = traindata)
plot(treefit, type="simple")
```

# Evaluation of the model
The confusion matrix, which is often used to evaluate the performance of a classification model is provided below. In a confusion matrix, the rows represent the actual classes or categories, while the columns represent the predicted classes or categories. Each cell in the matrix represents the number of observations that fall into a particular combination of actual and predicted classes. There are 174 instances where the actual class was "No_use," and the model correctly predicted "No_use." Also, here are 92 instances where the actual class was "Long_term," and the model correctly predicted "Long_term."

The model is observed to be 43% accurate, with 47% and 72% Sensitivity and Specificity respectively.

```{r, echo=FALSE}
# Making prediction with the above model
predict <- predict(treefit, newdata = testdata[, -1], type="response")
matriz_conf <- table(testdata[,1], predict)
library(caret)
confusionMatrix(matriz_conf)
```

# Conclusion
# Summary
As we see, using the accuracy of decision tree is 43%, and classification error is 57%. So, we can conjecture that using decision tree wouldn’t build a good model as we expect, though reliable. It was deduced that the wife age and education are the most important determinant of contraceptive metho used by women.

# Limitations of the study
Limited number of variables in the data: This may result in a loss of valuable information that could have contributed to a more comprehensive understanding of the study.
The values of the attribute named Media Exposure are mostly “Good”, so we cannot estimate whether this attribute works or not. 
It is hard to explain the detail of each attribute because there is no reference about this dissertation. Because of that, we maybe make some misunderstanding to attributes.
The outcomes change by the period and environment when the dataset is collected. For example, the environment of this dataset is Indonesia and the people there are Muslim. We think the outcomes are seldom the same in Taiwan.

# Recommendation/Improvement Areas:
There should be increased efforts to ensure adequate keeping of information or updating of database of contraceptive method used by women for future analysis.
Efforts should be made to increase awareness and proper documentation of other factors that may affect pregnancy.