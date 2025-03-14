# Question 1: 
# Loading the necessary libraries
library(dplyr)
library(tidyverse)
library(stringr)

# Loading the Data set: 
df <- read.csv("/Users/oratilwephage/Documents/ITRDA3-B11/Assignment/Dataset/graduate_survey.csv", stringsAsFactors = FALSE)

# a. Selecting relevant columns noted in the scenario only: 
# Selecting the relevant columns: 
relevant_columns <- c("Campus", "StudyField", "Branch", "Role", "EduLevel", "ProgLang", 
                      "Databases", "Platform", "WebFramework", "Industry", "AISearch", "AITool", "Employment")

data_clean <-df %>% select(all_of(relevant_columns))

# b. Missing Values treatment:
missing_values <- colSums(is.na(data_clean))

#Removing rows with missing values for simplicity
data_clean <- data_clean %>%
  drop_na()

# c. Standardizing categorical columns
data_clean <- data_clean %>%
  mutate(Campus = case_when(
    Campus %in% c("Durban", "Umhlanga") ~ "Durban",
    TRUE ~ Campus
  ))

# d. Subsetting data to contain the 3-5 campuses with the most responses.
# Count responses per campus
campus_counts <- data_clean %>%
  count(Campus) %>%
  arrange(desc(n))

# Getting top 3-5 campuses with the most responses
top_campuses <- campus_counts$Campus[1:5]

# Filtering data for these top campuses: 
data_clean <- data_clean %>%
  filter(Campus %in% top_campuses)