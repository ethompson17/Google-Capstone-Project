#load packages
library("tidyverse")
library("janitor")
library("lubridate")



#set working directory (PC)
setwd("C:/Users/ethom/Desktop/Data Analytics/Cyclistic/Dataset Files (Cyclistic)")



#load dataset (PC)

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






# set working directory (Mac)
setwd("~/Desktop/Dataset Files (Cyclistic)")

# Load Dataset (Mac)
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





# 1.combine into one dataframe
df <- rbind(Mar21Data,Apr21Data,May21Data,Jun21Data,Jul21Data,Aug21Data,Sep21Data,Oct21Data,Nov21Data,Dec21Data,Jan22Data,Feb22Data)







#data preview
head(df)

glimpse(df)

glimpse(df_avg_ridetime)

str(df)

colnames(df)


# 2.
df$started_at = as_datetime(df$started_at, tz = "America/Chicago")
class(df$started_at)

#3. 
df$ended_at = as_datetime(df$ended_at, tz = "America/Chicago")
class(df$ended_at)








#all casual/members

casualriders <- df %>% filter(member_casual == "casual")
memberriders <- df %>% filter(member_casual == "member")












# 3. Average Ride time

df$started_at = as_datetime(df$started_at, tz = "America/Chicago")
class(df$started_at)

df$ended_at = as_datetime(df$ended_at, tz = "America/Chicago")
class(df$ended_at)

df$ridetime <- difftime(df$ended_at,df$started_at, units = "mins")






#To Check if there are Negative Ride times (`started_at` is later than `ended_at`)

df %>% filter(ridetime < 0)

df_avg_ridetime %>% filter(ridetime < 0)




# Make Data Frame with no negative ridetime values

df_avg_ridetime <- df %>% filter(ridetime > 0) %>% filter(!is.na(ridetime))



# Mean Ridetime Per Member/Casual

df_avg_ridetime %>% filter(member_casual == "casual") %>% filter(ridetime < 480) %>% summarise(mean(ridetime))

df_avg_ridetime %>% filter(member_casual == "member") %>% filter(ridetime < 480) %>% summarise(mean(ridetime))

df_avg_ridetime %>% group_by(member_casual) %>% summary(ridetime)




# Change df_avg_ridetime$ridetime from difftime to numeric in order to fun a summary function to find median and quartiles to find outliers

df_avg_ridetime$ridetime = as.numeric(df_avg_ridetime$ridetime)

str(df_avg_ridetime$ridetime)

sort(df_avg_ridetime$ridetime) %>% summary(df_avg_ridetime$ridetime)






# Filter out Ridetime Outliers above 8 Hours

df$ridetime <- df_avg_ridetime %>% filter(0 < df_avg_ridetime$ridetime < 480)


glimpse(df)




# Split data into casual and member

casual <- df %>% filter(member_casual == "casual")

member <- df %>% filter(member_casual == "member")

str(df_avg_ridetime$member_casual)







# Visualize Average ride time (NOT DONE)

ggplot(df_avg_ridetime, aes(x= member_casual, y = ridetime))+
  geom_boxplot()

ggplot(data = df_avg_ridetime, aes(x = member_casual, y = ridetime))+
  geom_boxplot()



# Summaries
df %>% filter(is.na(ridetime))
df_avg_ridetime %>% summary(ridetime)

df %>% filter(is.na(member_casual))
df_avg_ridetime %>% summary(member_casual)

df %>% summary(ridetime)


str(df)


# Summaries per Member/Casual

df_avg_ridetime %>% filter(member_casual == "Casual") %>% summary(ridetime)
df_avg_ridetime %>% filter(member_casual == "Member") %>% summary(ridetime)

# Weekends vs. Weekdays Pie Chart (Members vs. Casual)
df$day <- weekdays(as.Date(df$started_at))

df$weekday <- df %>% filter(day == "Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

df$weekend <- df %>% filter(day == "Saturday", "Sunday")

df %>% ggplot()+
  geom_line(mapping = aes(x = weekday))+
  geom_line(mapping = aes(x = weekend))

disthaversine

day_member <- df$day %>% filter(member_casual == "member")

df_member <- df %>% filter(member_casual== "member")


# Find mean to make mean bar on graph
df.mean = df_avg_ridetime %>% group_by(member_casual) %>% mutate(ridetime_mean = mean(ridetime))


# Visualize Number of Rides each day of the week (TOTAL RIDES PER DAY GRAPH)

df %>% filter(member_casual == "member") %>% count(member_casual)/7

ggplot(df, aes(x = day))+
  geom_bar(aes(fill = member_casual), col= "black")+
  facet_grid(~member_casual)+
  scale_y_continuous(labels = scales::comma, breaks = seq(0,550000,50000))+
  labs(x = "Day", y = "Total Number of Rides", title = "Total Rides Per Day")+
  geom_hline(yintercept = mean(df_avg_ridetime$ridetime), color = "black")



# Visualize average ride time per Member/Casual

ggplot(df_avg_ridetime, aes(x= df_avg_ridetime$day, y= ridetime))+
  geom_bar(aes(x= df_avg_ridetime$day, y = ridetime, fill = member_casual), color = "black")+
  facet_grid(~member_casual)


df %>% filter(member_casual == "member" | "casual")

glimpse(df$day)

df %>% summary(day~member_casual)



