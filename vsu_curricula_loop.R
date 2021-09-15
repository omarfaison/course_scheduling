library(googlesheets4)
library(tidyverse)
library(igraph)
library(stringr)

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

classpairs<-select(final, class1, class2)
graph01<-graph.edgelist(as.matrix(classpairs), directed=F)
plot(graph01)

nhspairs<- final %>%
  filter(College == "Natural & Health Sciences" ) %>%
  filter(Year < 3) %>%
  select(class1, class2)

nhsgraph01<-graph.edgelist(as.matrix(nhspairs), directed=F)
plot(nhsgraph01)

nhsweight<-degree(nhsgraph01, mode="all")
plot(nhsgraph01, vertex.size=nhsweight)

mass_com_pairs<-final %>%
  filter(str_detect(Major, "^Mass Comm"))%>%
  select(class1, class2)

mass_com_graph<-graph.edgelist(as.matrix(mass_com_pairs), directed=F)
plot(mass_com_graph)