library("tidyverse")
library("lubridate")
library("dplyr")
library("pivottabler")
df <- read.csv("clean-divvy-tripdata.csv")
str(df)
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
df$started_at <- strptime(df$started_at,format="%Y-%m-%d %H:%M")
df$ended_at <- strptime(df$ended_at,format="%Y-%m-%d %H:%M")
#Winter
dfs <- df %>%
  filter(months(df$started_at)=="December" | months(df$started_at)=="January" | months(df$started_at)=="Febuary")
str(dfs)
dfs$ride_length <- hms(dfs$ride_length)
sum(is.na(dfs$ride_length))
dfs <- dfs[!is.na(dfs$ride_length),]
sum(is.na(dfs$ride_length))
seconds_to_period(mean(period_to_seconds(dfs$ride_length)))
seconds_to_period(max(period_to_seconds(dfs$ride_length)))
Mode(dfs$day_of_week)
unique(months(df$started_at))
pt1 <- PivotTable$new()
pt1$addData(dfs)
pt1$addRowDataGroups("member_casual")
pt1$defineCalculation(calculationName="avg_ride_length (min)",summariseExpression = "mean(period_to_seconds(ride_length))/60")
pt1$renderPivot()

pt2 <- PivotTable$new()
pt2$addData(dfs)
pt2$addColumnDataGroups("member_casual")
pt2$addRowDataGroups("day_of_week")
pt2$defineCalculation(calculationName="ride_id",summariseExpression = "n()")
pt2$renderPivot()

pt3 <- PivotTable$new()
pt3$addData(dfs)
pt3$addColumnDataGroups("member_casual")
pt3$addRowDataGroups("day_of_week")
pt3$defineCalculation(calculationName="avg_ride_length (min)",summariseExpression = "mean(period_to_seconds(ride_length))/60")
pt3$renderPivot()

