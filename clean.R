install.packages("janitor")
install.packages("kimisc")
library("tidyverse")
library("dplyr")
library("janitor")
library("lubridate")
df <- read.csv("compiled-divvy-tripdata.csv")
sum(is.na(df$ride_length))
str(df)
df <- rename_with(df,tolower)
df <- clean_names(df)
glimpse(df)
sum(is.na(parse_date_time(df$ride_length,orders="HMS")))
df <- df %>%
  filter(member_casual=="member" || member_casual=="casual") %>%
  filter(!is.na(parse_date_time(df$ride_length,orders="HMS")))
sum(is.na(parse_date_time(df$ride_length,orders="HMS")))
df$started_at <- strptime(df$started_at,format="%d-%m-%Y %H:%M")
df$ended_at <- strptime(df$ended_at,format="%d-%m-%Y %H:%M")

write.csv(df,"clean-divvy-tripdata.csv", row.names=FALSE)


