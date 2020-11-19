library(readr)
library(dplyr)

# Read in data
ourData <- read.csv(File = "hw1_dataset_for_nnwokom1")

ourData

ourData$Totalcounty



 
#question 1 (Top 2 Results)
hw1_dataset_for_nnwokom1 %>%
  count(County, sort = TRUE) %>%
  top_n(2)

#question 2 (Top 2)
hw1_dataset_for_nnwokom1 %>%
  count(State, County, sort = TRUE)%>%
  filter(State =="MD") %>%
  top_n(2)
  

#question 3 (Top 2 results)
hw1_dataset_for_nnwokom1 %>%
  count(State, sort = TRUE) %>%
  top_n(2)

#question 4 (top 3 results)
hw1_dataset_for_nnwokom1 %>%
  count(State, Severity=4, sort = TRUE) %>%
  top_n(3)

  #question 5 (top 3 results)
hw1_dataset_for_nnwokom1 %>%
  count(State, Junction = TRUE, sort = TRUE) %>% 
  top_n(3)


#question 6 (top 3 results)
hw1_dataset_for_nnwokom1 %>%
  count(State, Weather_Condition, sort = TRUE)%>%
  filter(Weather_Condition== "Clear")%>%
  top_n(3)

#question 7 (top 3 results)
hw1_dataset_for_nnwokom1 %>%
 count(State, daytime, sort = TRUE)%>%
 filter(daytime == "night")%>%
 top_n(3)

#question 8 (Bottom 4 results)
hw1_dataset_for_nnwokom1 %>%
  group_by(State) %>%
  summarise(avg_temp = mean(Temperature.F.)) %>%
  top_n(-4)

#question 9
hw1_dataset_for_nnwokom1 %>%
  group_by(State) %>%
  summarise(avg_temp = mean(Temperature.F.)) %>%
  filter(avg_temp >='63')

#question 10
hw1_dataset_for_nnwokom1 %>%
  group_by(State)%>%
  summarise(total_Severity = sum(Severity)) %>%
  summarise(avg_Severity = (Severity = 4)/total_Severity) %>%
  filter(avg_Severity > .03)
  
  
  
  
  