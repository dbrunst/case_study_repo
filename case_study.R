##Task: Convert casual riders to members
##stakeholders: digital marketing reps


setwd("~/workspace/case_study")
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

files_all <- list("202012-divvy-tripdata.zip","202101-divvy-tripdata.zip","202102-divvy-tripdata.zip","202103-divvy-tripdata.zip","202104-divvy-tripdata.zip","202105-divvy-tripdata.zip","202106-divvy-tripdata.zip","202107-divvy-tripdata.zip","202108-divvy-tripdata.zip","202109-divvy-tripdata.zip","202110-divvy-tripdata.zip","202111-divvy-tripdata.zip")
files <- list("202012-divvy-tripdata.zip")

i <- 1
df <- data.frame()
for (f in files) {
  temp <- tempfile()
  file_url <- str_interp("https://divvy-tripdata.s3.amazonaws.com/${f}") 

  download.file(file_url, temp)

  unzipped_file <- unzip(temp)
  unlink(temp)

  df_temp <- read_csv(unzipped_file[1])
  df <- rbind(df, df_temp)

  print(sprintf("file: %s has %d rows in file ", f, nrow(df)))
  i = i +1
}
closeAllConnections()

#process data
df_updated <- df %>%   
  mutate(ride_length = ( ended_at - started_at)) %>% 
  mutate(day_of_week = as.numeric(format(as.Date(started_at), format="%w")))
#clean data
cleaned_df <- filter(df_updated, ride_length >= 0 & ride_length <= (60 * 60))

#summarize data: average ride length and max ride length
summarize(cleaned_df, mean_ride_length = mean(ride_length), max_ride_length= max(ride_length))

# get day of week with max number of rides
cleaned_df %>% select(day_of_week) %>% count(day_of_week) %>% slice(which.max(n)) %>% select(day_of_week)

#average ride length by member_casual type
cleaned_df %>% group_by(member_casual) %>% summarize(mean(ride_length))

#average ride length by member_casual type per day of the week
x <- cleaned_df %>% group_by(member_casual, day_of_week) %>% summarize(mean(ride_length))
#plot of ride length each day of week per member_casual
ggplot(data = x, aes(x= day_of_week, y=`mean(ride_length)`, color=member_casual)) +
  geom_point() +
  labs(title="Average ride length per day of week", caption="2021 data from https://divvy-tripdata.s3.amazonaws.com/index.html")

#number of rides by member_casual type per day of the week
x <- cleaned_df %>% group_by(member_casual, day_of_week) %>% summarise(total_rides = n())
#plot of total rides each day of week per member_casual
ggplot(data = x, aes(x= day_of_week, y=total_rides, color=member_casual)) +
  geom_point() +
  labs(title="Total rides per day of week", caption="2021 data from https://divvy-tripdata.s3.amazonaws.com/index.html")


