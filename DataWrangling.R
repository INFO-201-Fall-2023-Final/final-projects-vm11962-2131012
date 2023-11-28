library(dplyr)
library(stringr)

annapurna_df <- read.csv("annapurna.csv") 
accidents_df <- read.csv("accidents_git.csv") 
members_df <- read.csv("members.csv") 

#You first need to create a unified dataset 
#(i.e. you need to join your datasets together). 
#This means that the records in your two datasets 
#need to be related somehow, either by a shared key 
#or a combination of fields.
new_df <- merge(accidents_df, members_df, 
                by.x = "Publication.Year", by.y = "year", all = T)
  
  
#You will then also need to create additional columns in your dataset:   
#Must create at least one new categorical variable
new_df$Year_descr <- cut(new_df$Year, breaks = c(2000, 2000, 2020), 
                         labels = c("Before 2000", "2000-2020"), include.lowest = T) 
  
#Must create at least one new continuous/numerical variable
  
  
#Must create at least one summarization data frame 


#Note - your summarization table does not need o be exported to a csv file, 
#you just need to have code that create this data frame. 
