library("tidyverse")
library("lubridate")
library("pivottabler")
library("openxlsx")
library("readxl")
df <- read.csv("clean-divvy-tripdata.csv")
str(df)
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
df$started_at <- strptime(df$started_at,format="%Y-%m-%d %H:%M")
df$ended_at <- strptime(df$ended_at,format="%Y-%m-%d %H:%M")
#Annual
str(df)
df$ride_length <- hms(df$ride_length)
sum(is.na(df$ride_length))
df <- df[!is.na(df$ride_length),]
sum(is.na(df$ride_length))
seconds_to_period(mean(period_to_seconds(df$ride_length)))
seconds_to_period(max(period_to_seconds(df$ride_length)))
Mode(df$day_of_week)

pt1 <- PivotTable$new()
pt1$addData(df)
pt1$addRowDataGroups("member_casual")
pt1$defineCalculation(calculationName="avg_ride_length (min)",summariseExpression = "mean(period_to_seconds(ride_length))/60")
pt1$renderPivot()

pt2 <- PivotTable$new()
pt2$addData(df)
pt2$addColumnDataGroups("member_casual")
pt2$addRowDataGroups("day_of_week")
pt2$defineCalculation(calculationName="ride_id",summariseExpression = "n()")
pt2$renderPivot()
pt2$evaluatePivot()
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt2$writeToExcelWorksheet(wb=wb,wsName="Data",topRowNumber = 1, leftMostColumnNumber = 1, applyStyles=F)
saveWorkbook(wb, file="Users_per_day.xlsx",overwrite=TRUE)

pt3 <- PivotTable$new()
pt3$addData(df)
pt3$addColumnDataGroups("member_casual")
pt3$addRowDataGroups("day_of_week")
pt3$defineCalculation(calculationName="avg_ride_length (min)",summariseExpression = "mean(period_to_seconds(ride_length))/60")
pt3$renderPivot()
pt3$evaluatePivot()
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt3$writeToExcelWorksheet(wb=wb,wsName="Data",topRowNumber = 1, leftMostColumnNumber = 1, applyStyles=F)
saveWorkbook(wb, file="Avg_time_per_day.xlsx",overwrite=TRUE)

pt4 <- PivotTable$new()
pt4$addData(df)
pt4$addColumnDataGroups("member_casual")
pt4$addRowDataGroups("rideable_type")
pt4$defineCalculation(calculationName="type",summariseExpression = "n()")
pt4$renderPivot()
pt4$evaluatePivot()
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt4$writeToExcelWorksheet(wb=wb,wsName="Data",topRowNumber = 1, leftMostColumnNumber = 1, applyStyles=F)
saveWorkbook(wb, file="Ride_type.xlsx",overwrite=TRUE)







