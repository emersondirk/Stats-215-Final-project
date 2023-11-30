## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
table(data$severity_of_conversation)
mean(data$severity_of_conversation)
sd(data$severity_of_conversation)
summary(data$severity_of_conversation)

table(data$age_of_individual)
mean(data$age_of_individual)
sd(data$age_of_individual)
summary(data$age_of_individual)

table(data$`Relationship_to_character `)

table(data$Emotional_response_to_conversation)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
# Create a table showing the realtionship between Relationship and Emotional response
table(data$`Relationship_to_character `,data$Emotional_response_to_conversation)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
#
chisq.test(table(data$`Relationship_to_character `, data$Emotional_response_to_conversation))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
#
aov(severity_of_conversation ~ Emotional_response_to_conversation, data = data)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
#
cor(data$severity_of_conversation, data$Emotional_response_to_conversation)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
##### Examine the scatter plot
# showing the relationship between emotional response and severity
linear_relationship <- lm(age_of_individual ~ severity_of_conversation, data = data)
summary(linear_relationship)
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
#Calculate linear regression line (i.e., slope) and add to scatter plot
plot(data$severity_of_conversation, data$age_of_individual)
# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")
# Step 3: Add a line for the mean of X on the x-axis
abline(v=71, col = "blue")
# Step 4: Add a line for the mean of Y on the y-axis
abline(h=32, col="green")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
# Plot the residuals
plot(data$severity_of_conversation, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

