# BellaBeat-Data-Analysis

Author- Ashish Verma

Date- Wednesday, May 11, 2022

The case study follows the six steps of data analysis process

1.	Ask

2.	Prepare

3.	Process

4.	Analyze

5.	Share

6.	Act

**Scenario**

Bellabeat is a successful small company that has the potential to become a larger player in global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. I have been asked to focus on one of Bellabeat’s products (Bellabeat app, Leaf, Time, Spring, Bellabeat Membership) and analyze smart device data to gain insight into how consumers are using their smart devices. The insights you discover will then help guide marketing strategy for the company

# **Case Study Roadmap- Ask**

**Business Task**- “To analyze FitBit Fitness Tracker Data to gain insight into how consumers use non-Bellabeat smart devices and apply these insights into one of the Bellabeat Products “

**Primary Stakeholders**- Urška Sršen and Sando Mur

**Secondary Stakeholders**- Bellabeat marketing analytics team

# **Case Study Roadmap- Prepare**

Step 1.	**Data Source**- https://www.kaggle.com/datasets/arashnic/fitbit

Step 2.	**Data Organisation**- The data is organised in both long and wide format and stored across across 18 CSV files

Step 3.	**ROCCC** 

**Reliable**- The data is reliable and generated by 30 FitBit users and distributed by Amazon Mechanical Turk

**Original**- The data is original and collected from 30 FitBit users and distributed by Amazon Mechanical Turk

**Comprehensive**- The data doesn’t look comprehensive as it doesn’t talk about the gender the data is related to. The data has 33 user’s data where the dataset talks about 30 users only. Some of the data entered in weight log is done manually and some over the Wi-Fi. This raises the issue about comprehensiveness of data.

**Current**- Data is not current as it was recorded between 03.12.2016 and 05.12.2016 and is 6 years old. The data may not reflect the current user habits

**Cited**- Not Known

## Questions and Answers

Q 1.	How are you addressing licensing, privacy, security, and accessibility?

Answer- The data is available in public domain and hence, can be used without any license requirements.
The 30 users have given their content to use the data and hence, privacy is not an issue here.
The data is available under public domain with the consent of the users and is old, hence, security is not an issue here.
The data is accessible by anyone who is a registered user on Kaggle.

Q 2.	How did you verify the data’s integrity?

Answer: There is no way to verify the data integrity

Q 3.	Are there any problems with the data?

Answer: Yes, there are problems with the data. There is total 33 participants data instead of 30 as claimed in the data description. Gender details are not outlined in the dataset.

#  Case Study Roadmap -Process


## Step 1: Load the packages

library(tidyverse)

library(ggplot2) 

## Step 2: Import the data set into three data frames
Daily_Activity<-read_csv("D:\\GDAC\\Bellabeat Data\\Daily_Activity.csv")

Sleep_Day<-read_csv("D:\\GDAC\\Bellabeat Data\\Sleep_Day.csv")

Weight_Log<-read_csv("D:\\GDAC\\Bellabeat Data\\Weight_Log.csv")


# View the Data
View(Daily_Activity)
View(Sleep_Day)
View(Weight_Log)

# Check for internal structure
str(Daily_Activity)

# Explore the data sets
summary(Daily_Activity)
summary(Sleep_Day)
summary(Weight_Log)

# Convert Activity Date which is a character to Date and add column Weekday to the data frame
Daily_Activity<-Daily_Activity %>%
  mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))
  
# Analyze Data
**1. Check for uniqueness in all three tables**

n_distinct(Daily_Activity$Id)

n_distinct(Sleep_Day$Id)

n_distinct(Weight_Log$Id)

**2. In the weight log table, 5 users manually report their data while 3 users uses wifi to report the data**

Weight_Log %>% 
  filter(IsManualReport == "True") %>% 
  group_by(Id) %>% 
  summarise("Manual Weight Report"=n()) %>%
  distinct()

#Sedentary people takes less steps but burns more calories

![Total Steps vs Calories](https://user-images.githubusercontent.com/40716332/173922698-b6473c44-73e7-4a3f-9e7a-f301b470d3e1.png)


### Checking for Sedentary User Lifestyle

##### Steps and Calories- More active users burn more calories

![Calories vs Number of Steps](https://user-images.githubusercontent.com/40716332/173922724-795e5652-c8f4-4a1d-a2b1-01d8868fd03b.png)


### User with higher steps count has lower BMI

![Total Steps Vs BMI](https://user-images.githubusercontent.com/40716332/173923768-220cb4dc-ab81-4c24-8d4b-91b02d1aaa61.png)


### Total Distance Vs Total Calories

![Total Distance Vs Total Calories](https://user-images.githubusercontent.com/40716332/173923802-48825ef4-1b5e-473c-be57-04eab5f732ea.png)


### Sleep Activity

 ![Sleep Activitu](https://user-images.githubusercontent.com/40716332/173923864-c6deed4e-0579-441d-8212-f2904751e300.png)

  
### Sleep time in hours instead of minutes
sleep_day_in_hour <-Sleep_Day

sleep_day_in_hour$TotalMinutesAsleep <- sleep_day_in_hour$TotalMinutesAsleep/60

sleep_day_in_hour$TotalTimeInBed <- sleep_day_in_hour$TotalTimeInBed/60

head(sleep_day_in_hour)

### Check for any sleep outliers. # of times user sleep more than 10 hours or less than 1

sum(sleep_day_in_hour$TotalMinutesAsleep > 9)

sum(sleep_day_in_hour$TotalTimeInBed > 9)

sum(sleep_day_in_hour$TotalMinutesAsleep < 2)

sum(sleep_day_in_hour$TotalTimeInBed < 2)

### Awake time in bed

awake_in_bed <- mutate(Sleep_Day, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)

awake_in_bed <- awake_in_bed %>% 

  filter(AwakeTime >= 55) %>% 
  
  group_by(Id) %>% 
  
  arrange(AwakeTime, desc=TRUE) 
 
  
### Total 13 users are awake for more number of minutes in bed

n_distinct(awake_in_bed$Id)

