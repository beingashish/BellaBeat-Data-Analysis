#Step 1: Load the packages
library(tidyverse)
library(ggplot2) 

#Step 2: Import the data set into three data frames
Daily_Activity<-read_csv("D:\\GDAC\\Bellabeat Data\\Daily_Activity.csv")
Sleep_Day<-read_csv("D:\\GDAC\\Bellabeat Data\\Sleep_Day.csv")
Weight_Log<-read_csv("D:\\GDAC\\Bellabeat Data\\Weight_Log.csv")

#View the Data
View(Daily_Activity)
View(Sleep_Day)
View(Weight_Log)

#Check for internal structure
str(Daily_Activity)

#Explore the data sets
summary(Daily_Activity)
summary(Sleep_Day)
summary(Weight_Log)

# Convert Activity Date which is a character to Date and add column Weekday to the data frame
Daily_Activity<-Daily_Activity %>%
  mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))

#Merge Data
merged1 <- merge(Daily_Activity,Sleep_Day,by = c("Id"), all=TRUE)
merged_data <- merge(merged1, Weight_Log, by = c("Id"), all=TRUE)
view(merged_data)

write_csv(merged_data, "merged_data.csv")

#Analyze Data
#1. Check for uniqueness in all three tables
n_distinct(Daily_Activity$Id)
n_distinct(Sleep_Day$Id)
n_distinct(Weight_Log$Id)

#In the weight log table, 5 users manually report their data while 3 users uses wifi to report the data
Weight_Log %>% 
  filter(IsManualReport == "True") %>% 
  group_by(Id) %>% 
  summarise("Manual Weight Report"=n()) %>%
  distinct()

#Sedentary people takes less steps but burns more calories
ggplot(data=Daily_Activity,mapping = aes(x=TotalSteps, y=Calories, color=SedentaryMinutes))+
  geom_smooth(se=FALSE)+
  geom_point(alpha=0.5)+
  labs(title="Total Steps Vs Calories")+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  xlab("Total Steps")+
  ylab("Calories")

#Checking for Sedentary User Lifestyle
#Steps and Calories- More active users burn more calories
ggplot(data=Daily_Activity, aes(x=TotalSteps, y=SedentaryMinutes, color=Calories))+ 
  geom_point()+
  geom_smooth(se=FALSE)+
  scale_color_gradient(low="Red", high="green")+
  labs(title="Calories burnt against number of steps")+
  xlab("Total Steps")+
  ylab("Sedentary Minutes")

ggplot(data=Daily_Activity, aes(x=TotalSteps, y=SedentaryMinutes, color=TotalDistance))+ 
  geom_point()+
  stat_smooth(se=FALSE)+
  scale_color_gradient(low="blue", high="yellow")+
  labs(title="Calories burnt against distance")+
  xlab("Total Steps")+
  ylab("Sedentary Minutes")

#User with higher steps count has lower BMI
ggplot(data=merged_data, aes(x=TotalSteps, y = BMI, color=Calories))+ 
  geom_point()+ 
  stat_smooth(se=FALSE)+
  scale_color_gradient(low="blue", high="yellow")

#Total Distance Vs Total Calories
ggplot(data=Daily_Activity,mapping = aes(x=TotalDistance, y=Calories))+
  geom_smooth(se=FALSE)+
  geom_point(alpha=0.5, color="salmon")+
  labs(title="Total Steps Vs Calories")+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  xlab("Total Distance")+
  ylab("Calories")

#Sleep Activity
ggplot(data=Sleep_Day, mapping = aes(x=TotalTimeInBed, y=TotalMinutesAsleep))+
  geom_point()+
  geom_smooth(se=F)+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))+
  labs(title="Total Minutes Asleep vs Total Time in Bed")+
  xlab("Total Time in Bed")+
  ylab("Total Minutes Asleep")
  
#Sleep time in hours instead of minutes
sleep_day_in_hour <-Sleep_Day
sleep_day_in_hour$TotalMinutesAsleep <- sleep_day_in_hour$TotalMinutesAsleep/60
sleep_day_in_hour$TotalTimeInBed <- sleep_day_in_hour$TotalTimeInBed/60
head(sleep_day_in_hour)

#Check for any sleep outliers. # of times user sleep more than 10 hours or less than 1  
sum(sleep_day_in_hour$TotalMinutesAsleep > 9)
sum(sleep_day_in_hour$TotalTimeInBed > 9)
sum(sleep_day_in_hour$TotalMinutesAsleep < 2)
sum(sleep_day_in_hour$TotalTimeInBed < 2)

awake_in_bed <- mutate(Sleep_Day, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)
awake_in_bed <- awake_in_bed %>% 
  filter(AwakeTime >= 55) %>% 
  group_by(Id) %>% 
  arrange(AwakeTime, desc=TRUE) 
#Total 13 users are awake for more 
n_distinct(awake_in_bed$Id)
















