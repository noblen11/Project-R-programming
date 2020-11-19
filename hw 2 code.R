library(readr)
library(dplyr)
 

# Read in data
data <- read.csv(file = "...../Downloads/hw1_dataset_for_nnwokom1.csv")


hw1_dataset_for_nnwokom1 %>%
  count(Temperature.F., sort = TRUE) 


# question 1
hw1_dataset_for_nnwokom1 %>%
  select(Temperature.F.) %>%
  mutate(Temperature.F..count = n())%>%
  slice(1)

#question 2
hw1_dataset_for_nnwokom1 %>%
  summarize(Average = mean(Temperature.F.)) %>%
  round(2)

#question 3
hw1_dataset_for_nnwokom1 %>%
  select(Temperature.F.) %>%
  summarise_all(funs(sd(Temperature.F.,na.rm = FALSE))) %>%
  round(2)

#question 4
hw1_dataset_for_nnwokom1 %>%
  summarise(max = max(Temperature.F.))

#question 5
hw1_dataset_for_nnwokom1 %>%
  summarize(min = min(Temperature.F.))

#question 6
hw1_dataset_for_nnwokom1 %>% 
  select(Temperature.F.)%>%
  mutate(zscore = (max(Temperature.F.) - mean(Temperature.F.))/sd(Temperature.F.))%>%
  slice(1) %>%
  round(2)

#question 7
hw1_dataset_for_nnwokom1 %>% 
  select(Temperature.F.)%>%
  mutate(zscore = (min(Temperature.F.) - mean(Temperature.F.))/sd(Temperature.F.))%>%
  slice(1) %>%
  round(2)

#question 8
q8 = sum((hw1_dataset_for_nnwokom1$Temperature.F. > mean(hw1_dataset_for_nnwokom1$Temperature.F.) - sd(hw1_dataset_for_nnwokom1$Temperature.F.)) & 
  (hw1_dataset_for_nnwokom1$Temperature.F. < mean(hw1_dataset_for_nnwokom1$Temperature.F.)+ sd(hw1_dataset_for_nnwokom1$Temperature.F.)))
 q8

#question 9
lenght = length(hw1_dataset_for_nnwokom1$Temperature.F.)
porportion_9 = q8/lenght
round(porportion_9, 2)

#question 10
q10 = sum((hw1_dataset_for_nnwokom1$Temperature.F. > mean(hw1_dataset_for_nnwokom1$Temperature.F.) - 2*sd(hw1_dataset_for_nnwokom1$Temperature.F.)) & 
           (hw1_dataset_for_nnwokom1$Temperature.F. < mean(hw1_dataset_for_nnwokom1$Temperature.F.)+ 2*sd(hw1_dataset_for_nnwokom1$Temperature.F.)))

porportion_10 = q10/lenght
round(porportion_10,2)

#question 11
q11 = sum((hw1_dataset_for_nnwokom1$Humidity... > mean(hw1_dataset_for_nnwokom1$Humidity...) - sd(hw1_dataset_for_nnwokom1$Humidity...)) & 
            (hw1_dataset_for_nnwokom1$Humidity... < mean(hw1_dataset_for_nnwokom1$Humidity...)+ sd(hw1_dataset_for_nnwokom1$Humidity...)))

porportion_11 = q11/length(hw1_dataset_for_nnwokom1$Humidity...) 
round(porportion_11,2)


#question 12
q12 = sum((hw1_dataset_for_nnwokom1$Humidity... > mean(hw1_dataset_for_nnwokom1$Humidity...) - 2*sd(hw1_dataset_for_nnwokom1$Humidity...)) & 
            (hw1_dataset_for_nnwokom1$Humidity... < mean(hw1_dataset_for_nnwokom1$Humidity...)+ 2*sd(hw1_dataset_for_nnwokom1$Humidity...)))
q12
porportion_12 = q12/length(hw1_dataset_for_nnwokom1$Humidity...) 
round(porportion_12,2)


 