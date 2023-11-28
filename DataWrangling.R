library(dplyr)
library(stringr)

annapurna_df <- read.csv("annapurna.csv") 
accidents_df <- read.csv("accidents_git.csv") 
#members_df <- read.csv("members.csv") 

#You fist need to to create a unified dataset. This means that the records in your 
#two datasets need to be related some how, either by a shared key or a combination of fields.
annapurna_df$Year <- substr(annapurna_df$Date, nchar(annapurna_df$Date) -3, nchar(annapurna_df$Date))  

new_df <- merge(annapurna_df, accidents_df, 
                by.x = "Year", by.y = "Publication.Year", all = T)

new_df <- new_df[!duplicated(new_df[c("Accident.Title")]), ]

  
#You will then also need to create additional columns in your dataset: 
#Must create at least one new categorical variable
new_df$Year_descr <- cut(new_df$Year, breaks = c(2000, 2000, 2020), 
                         labels = c("Before 2000", "2000-2020"), include.lowest = T) 
  
#Must create at least one new continuous/numerical variable
  
  
#Must create at least one summarization data frame 
#Note - your summarization table does not need to be exported to a csv file, 
#you just need to have code that create this data frame. 
