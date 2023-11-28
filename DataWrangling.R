#Victoria Martinez
#Luke Takechi

library(dplyr)
library(stringr)

annapurna_df <- read.csv("annapurna.csv") 
mount_e_df <- read.csv("mount_everest_deaths.csv")

#You first need to create a unified dataset. This means that the records in your 
#two datasets need to be related somehow, either by a shared key or a combination of fields.
annapurna_df$Year <- substr(annapurna_df$Date, nchar(annapurna_df$Date) -3, nchar(annapurna_df$Date))  
mount_e_df$Year <- substr(mount_e_df$Date, nchar(mount_e_df$Date) -4, nchar(mount_e_df$Date))  

new_df <- merge(annapurna_df, mount_e_df, 
                by.x = "Date", by.y = "Date", all = T)

col_to_con <- c("Name", "Year", "Nationality", "Cause.of.death")

for (col in col_to_con) {
  new_df[[col]] <- paste(new_df[[paste0(col, ".x")]], new_df[[paste0(col, ".y")]], sep = " ")
}

col_to_rem <- c("Date", "Name.x", "Name.y", "Year.x", "Year.y", "Nationality.x", "Nationality.y", "Cause.of.death.x", "Cause.of.death.y")
new_df <- new_df[, -which(names(new_df) %in% col_to_rem)]

col_to_clean <- c("Name", "Year", "Nationality", "Cause.of.death")
new_df[col_to_clean] <- lapply(new_df[col_to_clean], function(x) gsub("NA", "", as.character(x)))

#You will then also need to create additional columns in your dataset: 
#Must create at least one new categorical variable
new_df$Year <- as.numeric(gsub("[^0-9]", "", new_df$Year))

new_df$Year_descr <- cut(new_df$Year, breaks = c(-Inf, 2000, 2020, Inf),
                         labels = c("Before 2000", "2000-2020", "After 2020"), include.lowest = TRUE)


#Must create at least one new continuous/numerical variable
new_df$Num_Deaths_In_Yr <- ave(rep(1, nrow(new_df)), new_df$Year, FUN = sum)



#Must create at least one summarization data frame 
#Note - your summarization table does not need to be exported to a csv file, 
#you just need to have code that create this data frame. 
summary_df <- summarise(group_by(new_df, Year_descr), Total_Deaths = sum(Num_Deaths_In_Yr, na.rm = TRUE))

