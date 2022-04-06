library("readxl")
library("tidyverse")
library("dplyr")
library("janitor")
library("reshape2")
df <- read_excel("users_per_day.xlsx")
df <- rename_with(df,tolower)
df <- clean_names(df)
df <- select(df, -total)
str(df)
options(scipen=100)

df2 = melt(df, id=c("days"))                               
str(df2)
ggplot(df2) + 
  geom_line(aes(x=days, y=value, colour=variable), size=1.5)+
  scale_colour_manual(values=c("gold","gray")) + 
  geom_point(aes(x=days, y=value)) +
  labs(y="No. of Users", x="Day of week", colour="Type")

df3 <- read_excel("Avg_time_per_day.xlsx")
df3 <- rename_with(df3,tolower)
df3 <- clean_names(df3)
df3 <- select(df3, -total)
str(df3)
df4 = melt(df3, id=c("day"))                               
str(df4)
ggplot(df4) + 
  geom_line(aes(x=day, y=value, colour=variable), size=1.5)+
  scale_colour_manual(values=c("gold","gray")) + 
  geom_point(aes(x=day, y=value)) +
  labs(y="Avg Ride Length (min)", x="Day of week", colour="Type")

df5 <- read_excel("ride_type.xlsx")
df5$fraction <- df5$member / sum(df5$member)
df5$ymax <- cumsum(df5$fraction)
df5$ymin <- c(0, head(df5$ymax, n=-1))
df5$labelPosition <- (df5$ymax + df5$ymin) / 2
df5$label <- paste0(df5$type)

ggplot(df5, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() +
  #geom_text( x=2, aes(y=labelPosition, label=label, color=type), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "right")

dfx <- read.csv("clean-divvy-tripdata.csv")
str(dfx)
dfx$started_at <- strptime(dfx$started_at,format="%Y-%m-%d %H:%M")
dfx$ended_at <- strptime(dfx$ended_at,format="%Y-%m-%d %H:%M")
str(dfx)

wim = count(filter(dfx,(months(dfx$started_at)=="December" | months(dfx$started_at)=="January" | months(dfx$started_at)=="Febuary") & member_casual=="member"))
wic = count(filter(dfx,(months(dfx$started_at)=="December" | months(dfx$started_at)=="January" | months(dfx$started_at)=="Febuary") & member_casual=="casual"))
spm = count(filter(dfx,(months(dfx$started_at)=="March" | months(dfx$started_at)=="April" | months(dfx$started_at)=="May") & member_casual=="member"))
spc = count(filter(dfx,(months(dfx$started_at)=="March" | months(dfx$started_at)=="April" | months(dfx$started_at)=="May") & member_casual=="casual"))
summ = count(filter(dfx,(months(dfx$started_at)=="June" | months(dfx$started_at)=="July" | months(dfx$started_at)=="August") & member_casual=="member"))
sumc = count(filter(dfx,(months(dfx$started_at)=="June" | months(dfx$started_at)=="July" | months(dfx$started_at)=="August") & member_casual=="casual"))
fam = count(filter(dfx,(months(dfx$started_at)=="September" | months(dfx$started_at)=="October" | months(dfx$started_at)=="November") & member_casual=="member"))
fac = count(filter(dfx,(months(dfx$started_at)=="September" | months(dfx$started_at)=="October" | months(dfx$started_at)=="November") & member_casual=="casual"))

type <- c("Winter","Spring","Summer","Fall")
m <- c(as.numeric(wim),as.numeric(spm),as.numeric(summ),as.numeric(fam))
c <- c(as.numeric(wic),as.numeric(spc),as.numeric(sumc),as.numeric(fac))
dfy <- data.frame(type,c)
str(dfy)
dfy$fraction <- dfy$c / sum(dfy$c)
dfy$ymax <- cumsum(dfy$fraction)
dfy$ymin <- c(0, head(dfy$ymax, n=-1))
dfy$labelPosition <- (dfy$ymax + dfy$ymin) / 2
dfy$label <- paste0(dfy$type)

ggplot(dfy, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=type)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=type), size=6) + # x here controls label position (inner / outer)
  #scale_fill_brewer(palette="Dark2") +
  #scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "right")