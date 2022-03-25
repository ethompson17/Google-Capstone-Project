#load packages
library("tidyverse")
library("janitor")
library("lubridate")

#set working directory pc
setwd("C:/Users/ethom/Desktop/Data Analytics/Cyclistic/Dataset Files (Cyclistic)")


#load dataset

Mar21Data <- read.csv("202103-divvy-tripdata.csv")
Apr21Data <- read.csv("202104-divvy-tripdata.csv")
May21Data <- read.csv("202105-divvy-tripdata.csv")
Jun21Data <- read.csv("202106-divvy-tripdata.csv")
Jul21Data <- read.csv("202107-divvy-tripdata.csv")
Aug21Data <- read.csv("202108-divvy-tripdata.csv")
Sep21Data <- read.csv("202109-divvy-tripdata.csv")
Oct21Data <- read.csv("202110-divvy-tripdata.csv")
Nov21Data <- read.csv("202111-divvy-tripdata.csv")
Dec21Data <- read.csv("202112-divvy-tripdata.csv")
Jan22Data <- read.csv("202201-divvy-tripdata.csv")
Feb22Data <- read.csv("202202-divvy-tripdata.csv")


#combine into one dataframe
df <- rbind(Mar21Data,Apr21Data,May21Data,Jun21Data,Jul21Data,Aug21Data,Sep21Data,Oct21Data,Nov21Data,Dec21Data,Jan22Data,Feb22Data)





#data preview
head(df)

glimpse(df)

str(df)

colnames(df)


df$started_at = as_datetime(df$started_at, tz = "America/Chicago")
class(df$started_at)

df$ended_at = as_datetime(df$ended_at, tz = "America/Chicago")
class(df$ended_at)



---------------------------------
  
  #all casual/members
  casualriders <- df %>% filter(member_casual == "casual")
memberriders <- df %>% filter(member_casual == "member")



#Average Ride time

df$started_at = as_datetime(df$started_at, tz = "America/Chicago")
class(df$started_at)

df$ended_at = as_datetime(df$ended_at, tz = "America/Chicago")
class(df$ended_at)

df$ridetime <- difftime(df$ended_at,df$started_at, units = "mins")

df$ridetime <- as.period(df$ridetime)


#To Check if there are Negative Ride times (`started_at` is later than `ended_at`)

df %>% filter(ridetime < 0)

df_avg_ridetime <- df %>% filter(ridetime > 0)

df_avg_ridetime %>% filter(member_casual == "casual") %>% summarise(mean(ridetime))

df_avg_ridetime %>% filter(member_casual == "member") %>% summarise(mean(ridetime))

# df %>% filter(ridetime > 20)

# Split data into casual and member

casual <- df %>% filter(member_casual == "casual")

member <- df %>% filter(member_casual == "member")


# Visualize Average ride time (NOT DONE)


ggplot(data = df)+
  geom_boxplot(mapping = aes(x = member_casual,y= ridetime, fill = member_casual))+
  labs(title = "Average Ride Time: Casual vs. Member")

ggplot(data = df)+
  geom_point(mapping = aes(y= ridetime, color = member_casual))+
  labs(title = "Average Ride Time: Casual vs. Member")

df %>% filter(is.na(ridetime))


# Weekends vs. Weekdays Pie Chart (Members vs. Casual)
df$day <- weekdays(as.Date(df$started_at))

df$weekday <- df %>% filter(day == "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

df$weekend <- df %>% filter(day == "Saturday", "Sunday")

df %>% ggplot()+
  geom_line(mapping = aes(x = weekday))+
  geom_line(mapping = aes(x = weekend))


