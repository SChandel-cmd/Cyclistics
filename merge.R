df1 <- read.csv("202103-divvy-tripdata.csv")
df2 <- read.csv("202104-divvy-tripdata.csv")
df3 <- read.csv("202105-divvy-tripdata.csv")
df4 <- read.csv("202106-divvy-tripdata.csv")
df5 <- read.csv("202107-divvy-tripdata.csv")
df6 <- read.csv("202108-divvy-tripdata.csv")
df7 <- read.csv("202109-divvy-tripdata.csv")
df8 <- read.csv("202110-divvy-tripdata.csv")
df9 <- read.csv("202111-divvy-tripdata.csv")
df10 <- read.csv("202112-divvy-tripdata.csv")
df11 <- read.csv("202201-divvy-tripdata.csv")
df12 <- read.csv("202202-divvy-tripdata.csv")

df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

write.csv(df,"compiled-divvy-tripdata.csv", row.names=FALSE)
