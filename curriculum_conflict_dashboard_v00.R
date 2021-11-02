library(googlesheets4)
library(tidyverse)
library(igraph)
library(stringr)
library(shiny)
library(shinydashboard)

gs4_deauth()
course_data<-read_sheet("https://docs.google.com/spreadsheets/d/1nCZXnE6hM8syZvrO7t6aN42uuxTGUXhMu7kOhMMN7jQ/edit?usp=sharing")

final<-data.frame(matrix(ncol=6, nrow=0))
colnames(final)<-c('College', 'Major','Concentration','Year','class1','class2')

for (i in 1:nrow(course_data)) {
  row_filter<-course_data %>% filter(row_number()==i)
  bg<-row_filter %>% select(College:Year)
  classes<-row_filter %>% select(-c(College:Year))
  classes_clean<- classes[!is.na(classes)]
  pairs<-combn(classes_clean,2, simplify=T) %>%
    t() %>%
    as.data.frame() %>%
    rename(class1= V1,
           class2= V2)
  reunite<-cbind(bg, pairs)
  final<-rbind(final, reunite)
}

ui<-fluidPage(
 checkboxGroupInput("year", "Select years", unique(final$Year), NULL),
  textOutput("chosen"),
  tableOutput("selected_data")
)

server<-function(input, output, session) {
  output$chosen<-renderText(paste0("You chose",input$year))
  output$selected_data<-renderTable(final %>% filter(Year==input$year))
}

shinyApp(ui, server)
