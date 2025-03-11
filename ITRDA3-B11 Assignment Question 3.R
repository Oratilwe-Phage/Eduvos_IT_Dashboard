# Question 3: 
# Needed libraries for the question: 
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinydashboard)

# Dashboard Layout and Design: 
# User Interface : 
ui <- dashboardPage(
  dashboardHeader(title= " Eduvos IT Graduate Survey Analysis"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Tools Analysis", tabName = "tools", icon = icon("tools")),
      menuItem("Industry Analysis", tabName = "industry", icon = icon("industry")),
      menuItem("Job Roles", tabName = "roles", icon = icon("briefcase")),
      menuItem("Employment Rate", tabName = "employment", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              h2("Overview of the Survey Data"),
              p("This dashboard offers information about the sectors and tools that Eduvos IT graduates are now working in.")
      ),
      tabItem(tabName = "tools",
              h2("Top Tools Used by Graduates"),
              plotOutput("toolsPlot")
      ),
      tabItem(tabName = "industry", 
              h2("Top Industries Graduates Work In"),
              plotOutput("industryPlot")
      ),
      tabItem(tabName = "roles",
              h2("Top Job Roles"),
              plotOutput("rolesPlot")
      ),
      tabItem(tabName = "employment",
              h2("Employment Rate by Study Field"),
              plotOutput("employmentPlot"))
    )
  )
)

# Server 
# Defining the Server logic: 
server <- function(input, output) {
  
  # Top Tools Plot
  output$toolsPlot <- renderPlot({
    all_tools <- data_clean %>%
      separate_rows(ProgLang, sep = ";") %>%
      count(ProgLang, sort = TRUE) %>%
      slice_max(n, n = 10)
    
    ggplot(all_tools, aes(x = reorder(ProgLang, n), y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "The Most Popular Programming Languages Among Graduates",
           x = "Programming Languages",
           y = "Count") +
      theme_minimal()
  })
  
  # Industries Plot 
  output$industryPlot <- renderPlot({
    industry_counts <- data_clean %>%
      separate_rows(Industry, sep = ";") %>%
      count(Industry, sort = TRUE)
    
    ggplot(industry_counts, aes(x = reorder(Industry, n), y = n)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      labs(title = "Industries Graduates Work In",
           x = "Industry",
           y = "Count") +
      theme_minimal()
  })
  
  # Job Roles Plot: 
  output$rolesPlot <- renderPlot({
    role_counts <- data_clean %>%
      count(Role, sort = TRUE)
    
    ggplot(role_counts, aes(x = reorder(Role, n), y = n)) +
      geom_bar(stat = "identity", fill = "lightpink") +
      coord_flip() +
      labs(title = "Top Job Roles of Graduates",
           x = "Job Role",
           y = "Count") +
      theme_minimal()
  })
  
  # Employment Rate Plot:
  output$employmentPlot <- renderPlot({
    employment_rate <- data_clean %>%
      group_by(StudyField) %>%
      summarise(Employed = sum(str_detect(Employment, "Employed")),
                Total = n(),
                EmploymentRate = Employed / Total * 100)
    
    ggplot(employment_rate, aes(x = StudyField, y = EmploymentRate)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Employment Rate by Field of Study",
           x = "Study Field",
           y = "Employment Rate (%)") +
      theme_minimal() +
      ylim(0, 100) # Setting y-axis limits from 0 to 100%
  })
}

# Running the Application: 
shinyApp(ui = ui, server = server)
