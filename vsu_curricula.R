library(googlesheets4)
library(tidyverse)

gs4_deauth()
course_data<-read_sheet("https://docs.google.com/spreadsheets/d/1nCZXnE6hM8syZvrO7t6aN42uuxTGUXhMu7kOhMMN7jQ/edit?usp=sharing")

row_filter<-course_data %>%
  filter(row_number()==1) 

test_bg<-row_filter %>% select(College:Year)

test_classes<-row_filter %>% select(-c(College:Year)) 
test_classes_clean<-test_classes[!is.na(test_classes)]

course_pairs<-combn(test_classes_clean,2, simplify=T) %>%
  t()%>%
  as.data.frame() %>%
  rename(class1= V1,
          class2= V2)

reunite<-cbind(test_bg, course_pairs)