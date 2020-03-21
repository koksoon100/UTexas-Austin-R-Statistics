## Project - Cardio Good Fitness

##################################################
# 1. Data Preparation
##################################################

########################
## 1.1 Import Libraries
library(readr)  ## To import .csv
library(ggplot2)
library(esquisse)
library(cowplot)
library("ggpubr")
library(dplyr)
library(DataExplorer)

########################
## 1.2. Set Up Working Directory
setwd("C:/Users/koksoon.chai/OneDrive - Kaer Pte Ltd/Machine_learning/GreatLearning/Week 3")

########################
## 1.3. Importing the dataset of cardio fitness
file_name <- "CardioGoodFitness.csv"
cardio_fitness_data = read_csv(file_name)
summary(cardio_fitness_data) # View the data before astarting the analysis
View(cardio_fitness_data)

##################################################
# 2. Perform uni-variate analysis
##################################################

########################
## 2.1 Understanding the structure of dataset
colnames(cardio_fitness_data)
colnames(cardio_fitness_data) = make.names(colnames(cardio_fitness_data))
cardio_fitness_data$Gender = as.factor(cardio_fitness_data$Gender)
cardio_fitness_data$MaritalStatus = as.factor(cardio_fitness_data$MaritalStatus)
cardio_fitness_data$Product = as.factor(cardio_fitness_data$Product)
summary(cardio_fitness_data)
colnames(cardio_fitness_data)
View(cardio_fitness_data)

dim(cardio_fitness_data)
names(cardio_fitness_data)
str(cardio_fitness_data)
head(cardio_fitness_data, 10)
tail(cardio_fitness_data, -10)

########################
# 2.2. Explore the data
count_number_of_outliers <- function(x) {
  Q3 = quantile(x,0.75)
  Q3 = unname(Q3)
  Q1 = unname(quantile(x,0.25))
  age_iqr = IQR(x)
  
  Q3_outlier <- Q3+1.5*age_iqr
  Q1_outlier <- Q1-1.5*age_iqr
  
  # Select the outliers
  outliers <- x[which( x < Q1_outlier | x > Q3_outlier )]
  
  # Print the value of outliers
  print(outliers)
  
  return(length(outliers))
}

explore_variable <- function(x) {
  mean_x <- round(mean(x),2)
  
  median_x <- round(median(x),2)
  range(x)

  std_x <- round(sd(x),1)
  
  count_number_of_outliers(x)
  print(x)
  
  summary_of_variable <- paste("Mean=", toString(mean_x), "; std=", toString(std_x), "; median=", toString(median_x))
  summary_of_variable <- paste(summary_of_variable, "; No of Outliers=", toString(count_number_of_outliers(x)))
  
  # Check the centrality of data
  if (median_x < mean_x)
    summary_of_variable <- paste(summary_of_variable, "; skew to the left")
  else
    summary_of_variable <- paste(summary_of_variable, "; skew to the right")
  
  # print(summary_of_variable)
  
  return(summary_of_variable)
}

explore_variable(cardio_fitness_data$Age)
count_number_of_outliers(cardio_fitness_data$Age)


########################
# 2.3 Univariate Analysis
#esquisser() 

# Classification of various variables
# 2.3.1 Plot the fequency of age bin
age_bin <- ggplot(cardio_fitness_data) +
  aes(x = Age) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Age", y = "Frequency", title = "Age",  subtitle = explore_variable(cardio_fitness_data$Age)) +
  theme_minimal()

age <- ggplot(cardio_fitness_data) +
  aes(x = "", y = Age) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Age",  subtitle = explore_variable(cardio_fitness_data$Age)) +
  theme_minimal()

## 2.3.2 Plot the frequency of education bin
edu_bin <- ggplot(cardio_fitness_data) +
  aes(x = Education) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Education", y = "Frequency", title = "Education",  subtitle = explore_variable(cardio_fitness_data$Education)) +
  theme_minimal()

education <- ggplot(cardio_fitness_data) +
  aes(x = "", y = Education) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Education",  subtitle = explore_variable(cardio_fitness_data$Education)) +
  theme_minimal()

## 2.3.3 Plot the frequency of gender bin
gender_bin <- ggplot(cardio_fitness_data) +
  aes(x = Gender) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Gender", y = "Frequency", title = "Gender") +
  theme_minimal()

# 2.3.4 Plot the frequency of gender bin
marital_bin <- ggplot(cardio_fitness_data) +
  aes(x = MaritalStatus) +
  geom_bar(fill = "#0c4c8a") +
  labs(y = "Frequency", title = "Marital Status") +
  theme_minimal()

# 2.3.5 Plot the frequency of usage bin
usage_bin <- ggplot(cardio_fitness_data) +
  aes(x = Usage) +
  geom_histogram(bins = 21L, fill = "#0c4c8a") +
  labs(y = "Frequency", title = "Usage",  subtitle = explore_variable(cardio_fitness_data$Usage)) +
  theme_minimal()

usage <- ggplot(cardio_fitness_data) +
  aes(x = "", y = Usage) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Usage",  subtitle = explore_variable(cardio_fitness_data$Usage)) +
  theme_minimal()

# 2.3.6 Plot the frequency of fitness bin
fitness_bin <- ggplot(cardio_fitness_data) +
  aes(x = Fitness) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(y = "Frequency", title = "Fitness",  subtitle = explore_variable(cardio_fitness_data$Fitness)) +
  theme_minimal()

fitness <- ggplot(cardio_fitness_data) +
  aes(x = "", y = Fitness) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Fitness",  subtitle = explore_variable(cardio_fitness_data$Fitness)) +
  theme_minimal()

# 2.3.7 Plot the frequency of income bin
income_bin <- ggplot(cardio_fitness_data) +
  aes(x = Income) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(y = "Frequency", title = "Income",  subtitle = explore_variable(cardio_fitness_data$Income)) +
  theme_minimal()

income <- ggplot(cardio_fitness_data) +
  aes(x = "", y = Income) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Income",  subtitle = explore_variable(cardio_fitness_data$Income)) +
  theme_minimal()

# 2.3.8 Plot the frequency of mile bin
miles_bin <- ggplot(cardio_fitness_data) +
  aes(x = Miles) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(y = "Frequency", title = "Miles",  subtitle = explore_variable(cardio_fitness_data$Miles)) +
  theme_minimal()

miles <- ggplot(cardio_fitness_data) +
  aes(x = "", y = Miles) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Miles",  subtitle = explore_variable(cardio_fitness_data$Miles)) +
  theme_minimal()

# 2.4 Generate status profile of users
status_profile <- ggarrange(income_bin, edu_bin, income, education, nrow = 2, ncol= 2, labels = "AUTO")
annotate_figure(status_profile,
                top = text_grob("Status profile of users", face = "bold", size = 14),
                bottom = text_grob("Data source: \n Cardio Good Fitness data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                fig.lab = "Figure 1", fig.lab.face = "bold"
)

# 2.5 Generate fitness profile of users
fitness_profile <- ggarrange(fitness_bin, age_bin, fitness, age, nrow = 2, ncol= 2, labels = "AUTO")
annotate_figure(fitness_profile,
                top = text_grob("Fitness profile of users", face = "bold", size = 14),
                bottom = text_grob("Data source: \n Cardio Good Fitness data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                fig.lab = "Figure 1", fig.lab.face = "bold"
)

# 2.6 Generate usage profile of users
usage_profile <- ggarrange(usage_bin, miles_bin, usage, miles, nrow = 2, ncol= 2, labels = "AUTO")
annotate_figure(usage_profile,
                top = text_grob("Usage profile of users", color = "red", face = "bold", size = 14),
                bottom = text_grob("Data source: \n Cardio Good Fitness data set", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Figure arranged using ggpubr", color = "green", rot = 90),
                fig.lab = "Figure 1", fig.lab.face = "bold"
)

##################################################
# 3. Perform Bi-Variate Analysis
##################################################

cardio_fitness_data$income_bins <- cut(cardio_fitness_data$Income, 3, labels=c("Low Income", "Medium Income", "High Income"))
cardio_fitness_data$education_bins <- cut(cardio_fitness_data$Education, 3, labels=c("Low Education", "Medium Education", "High Education"))
cardio_fitness_data$age_bins <- cut(cardio_fitness_data$Age, 3, breaks = c(0,30, 40, 100), labels=c("Young Age", "Middle Age", "Senior Age"))
cardio_fitness_data$miles_bins <- cut(cardio_fitness_data$Miles, 3, breaks = c(0, 50, 100, 200, 300), labels=c("Rare exercise", "Moderate exercise", "High exercise", "Very high exercise"))
View(cardio_fitness_data)

## 3.1 Gender vs products
table(cardio_fitness_data$Product, cardio_fitness_data$Gender)
prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$Gender))*100  
  
## 3.2 marital status vs products
table(cardio_fitness_data$Product, cardio_fitness_data$MaritalStatus)
prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$MaritalStatus))*100  
  
## 3.3 Age vs products
table(cardio_fitness_data$Product, cardio_fitness_data$age_bins)
prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$age_bins))*100  
  
## 3.4 Age vs products
table(cardio_fitness_data$Product, cardio_fitness_data$Fitness)
prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$Fitness))*100  
  
## 3.5 Age vs products
table(cardio_fitness_data$Product, cardio_fitness_data$education_bins)  
prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$education_bins))*100  

## 3.6 Age vs products
table(cardio_fitness_data$Product, cardio_fitness_data$income_bins)  
prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$income_bins))*100  
  
## 3.7 Age vs products
table(cardio_fitness_data$Product, cardio_fitness_data$miles_bins)  
prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$Gender))*100  
  
by(cardio_fitness_data,INDICES = cardio_fitness_data$Product, FUN=summary)
by(cardio_fitness_data,INDICES = cardio_fitness_data$Gender, FUN=summary)

is.na(cardio_fitness_data)

prop.table(table(cardio_fitness_data$Product, cardio_fitness_data$Fitness, cardio_fitness_data$age_bins))*100  
sum(is.na(cardio_fitness_data))

library(rpivotTable)
rpivotTable(cardio_fitness_data)

library(DataExplorer)

user_with_high_fitness <- cardio_fitness_data[which(cardio_fitness_data$Fitness>=4),] %>%
group_by(Fitness) %>%
  summarise()
View(user_with_high_fitness)

table(user_with_high_fitness$Age~user_with_high_fitness$, user_with_high_fitness$Product)
## Customer profile analysis based on product
## Study the product based on income percentile

## Summary table - Generate a set of insights and recommendations that will help the company in targeting new customers
#Region_sum = cardio_fitness_data %>% group_by(income_bins) %>% summarise()

#require(dplyr);

#output_1 <- cardio_fitness_data %>%
#  group_by(Product) %>%
#  mutate(Usage_usage=min_rank(Usage))

#View(output_1)

#library(rpivotTable)
#rpivotTable(cardio_fitness_data)


#### SUMMARY TABLES

## table()

#Region_vs_Income_Cat=table(Countries_All$Region,Countries_All$IncomeGroup)
#Region_vs_Income_Cat

#prop.table(Region_vs_Income_Cat,1) # Row Total is 1

#prop.table(Region_vs_Income_Cat,2) # Column Total is 1


## %>% group_by()







