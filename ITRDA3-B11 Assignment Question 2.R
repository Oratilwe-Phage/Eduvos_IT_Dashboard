# Question 2: 
# Loading the necessary libraries: 
library(ggplot2)
library(stringr)
library(tidyverse)
library(forcats)

# (I.) Example for Programming Languages: 
# Function to count occurrences of tools
count_tools <- function(prog_lang_count){
  data_clean %>%
    separate_rows(!!sym(prog_lang_count), sep = ";") %>%
    count(!!sym(prog_lang_count), sort = TRUE) %>%
    top_n(10, n) %>%
    mutate(Tool = !!sym(prog_lang_count))
}

# Count tools for each category: 
top_prog_langs <- count_tools("ProgLang")
top_databases <- count_tools("Databases")
top_platforms <- count_tools("Platform")
top_web_frameworks <- count_tools("WebFramework")
top_ai_search <- count_tools("AISearch")
top_ai_tools <- count_tools("AITool")

# Combining all tools for Visualizing the top programming languages: 
all_tools <- bind_rows(
  mutate(top_prog_langs, Category = "Programming Languages"),
  mutate(top_databases, Category = "Databases"),
  mutate(top_platforms, Category = "Platforms"),
  mutate(top_web_frameworks, Category = "Web Frameworks"),
  mutate(top_ai_search, Category = "AI Search Tools"),
  mutate(top_ai_tools, Category = "AI Developer Tools")
)

# Plotting the top tools
ggplot(all_tools, aes(x = reorder(Tool, n), y = n, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Tools Used by Graduates",
       x = "Tools",
       y = "Count",
       fill = "Category") +
  theme_minimal()

# (II) Most Popular Industries by Study Field: 
# Count industries by study field
industry_counts <- data_clean %>%
  separate_rows(Industry, sep = ";") %>%
  count(StudyField, Industry, sort = TRUE)

# Plotting the most popular industries
ggplot(industry_counts, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Most Popular Industries by Study Field",
       x = "Industry",
       y = "Count",
       fill = "Study Field") +
  theme_minimal()

# (III) Top Job Roles by Study Field: 
# Counting job roles by study field 
role_counts <- data_clean %>%
  count(StudyField, Role, sort = TRUE)

# Visualizing popular industries by StudyField
# Plotting the top job roles
ggplot(role_counts, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Top Job Roles by Field Study",
       x = "Job Role",
       y = "Count",
       fill = "Study Field") +
  theme_minimal()

# (IV) Employment rate of graduates from each study field: 
# Calculating the employment rate for each study field
employment_rate <- data_clean %>%
  group_by(StudyField, Employment) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(StudyField) %>%
  mutate(total = sum(count),percentage=count/total *100) %>%
  filter(str_detect(Employment,"Employed")) %>%
  ungroup()

# Visualizing the employment rate: 
ggplot(employment_rate, aes(x = StudyField, y = percentage, fill = StudyField)) +
  geom_bar(stat = "identity") +
  labs(title = "Employment Rate by Field of Study",
       x = "Study Field",
       y = "Employment Rate (%)") +
  theme_minimal()
