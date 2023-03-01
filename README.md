# Google-Data-Analytics-Bellabeat-Case-Study

Author: Payton Person

Date: 2023-02-15

## About Bellabeat

Here at Bellabeat, women’s health is our passion. Bellabeat is a high-tech company that manufactures health-focused smart products worldwide. Urška Sršen and Sando Mur founded Bellabeat in 2013, with the intent to develop beautifully designed technology that informs and inspires women around the world.

## Business Task

Analyze FitBit data to gain insight and help guide marketing strategy for Bellabeat to grow as a global force in the smart device market.
Utilize the Fitbit Fitness Tracker dataset to derive potential growth opportunities and make analysis based recommendations to our marketing operations team.

•	Problem: Improve Bellabeat marketing strategy

•	What are other products doing better?
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?


Primary stakeholders: Urška Sršen and Sando Mur, executive team members.

Secondary stakeholders: Bellabeat marketing analytics team.

## Environment Setup:

```{r}
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("viridisLite")
install.packages("scales")
install.packages("devtools")
devtools::install_github("hadley/devtools")
remotes::install_github("gadenbuie/cleanrmd")
```

```{r}
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridisLite)
library(scales)
library(devtools)
library(readr)
```

### Importing datasets

```{r}
activity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
View(activity)

hourly_calories <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
View(hourly_calories)

hourly_intensities <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
View(hourly_intensities)

sleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
View(sleep)

weight_log <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
View(weight_log)
```

## Data Cleaning & Preparation

```{r}
n_distinct(activity$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight_log$Id)
```

 Based on our total population of 33 users, the weightlog dataset will have an insufficient sample size to be used in this analysis. 

```{r}
sum(duplicated(sleep))
sum(duplicated(activity))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_calories))
```

### Removing duplicates from sleep 

```{r}
sleep <- unique(sleep)
sum(duplicated(sleep)) 
```

### Standardizing column names 

```{r}
activity <- rename_with(activity, tolower)
sleep <- rename_with(sleep, tolower)
hourly_calories <- rename_with(hourly_calories, tolower)
hourly_intensities <- rename_with(hourly_intensities, tolower)
```

### Standardizing date and time 

```{r}
activity <- activity %>% 
  rename(date= activitydate) %>% 
  mutate(date= as_date(date, format= "%m/%d/%Y"))

sleep <- sleep %>%
  rename(date= sleepday) %>%
  mutate(date= as_date(date, format= "%m/%d/%Y  %I:%M:%S %p", tz= Sys.timezone()))

hourly_intensities <- hourly_intensities %>% 
  rename(date_time= activityhour) %>% 
  mutate(date_time= as.POSIXct(date_time, format="%m/%d/%Y %I:%M:%S %p", tz= Sys.timezone()))

hourly_calories <- hourly_calories %>% 
  rename(date_time= activityhour) %>% 
  mutate(date_time= as.POSIXct(date_time, format="%m/%d/%Y %I:%M:%S %p", tz= Sys.timezone()))

hourly_calories_intensities <- merge(x = hourly_calories, y = hourly_intensities, by = c("id","date_time"))
activity_sleep <- merge( x = activity, y = sleep, by = c("id", "date"))
```

### Merge Data 

```{r}
hourly_calories_intensities <- merge(x = hourly_calories, y = hourly_intensities, by = c("id","date_time"))
activity_sleep <- merge( x = activity, y = sleep, by = c("id", "date"))
```

## Data summarization 

```{r}
activity %>% 
  select(totalsteps, totaldistance,calories) %>% 
  summary()

activity %>% 
  select(veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes, sedentaryminutes) %>% 
  summary()

sleep %>% 
  select(totalminutesasleep) %>% 
  summary()

hourly_calories_intensities %>% 
  select(totalintensity, averageintensity, calories) %>% 
  summary()
```

## Add weekday & seperate date & time

```{r}
hourly_calories_intensities <- hourly_calories_intensities %>% 
  separate(date_time, into= c('date', 'time'), sep= c(' ')) %>% 
  mutate(date= ymd (date))

hourly_calories_intensities$weekday <- weekdays(hourly_calories_intensities$date) 

hourly_calories_intensities_day_time <- (hourly_calories_intensities) %>% 
  group_by(weekday, time)%>% 
  summarize(mean_avg_intensity= mean(averageintensity, na.rm = TRUE))
```

## Organize with Monday starting the week 

```{r}
hourly_calories_intensities_day_time$weekday <- factor(hourly_calories_intensities_day_time$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
```

## Data visualizations 

```{r}
ggplot(hourly_calories_intensities_day_time, aes(time, weekday))+
  theme(axis.text.x= element_text(angle = 90))+
  labs(title= "Daily Intensity Output", x = " ", y = " ", fill = "Average Intensity Output", caption = 'Data Source: Fitabase Data 4.1.2.16-5.12.16')+
  geom_tile(color = "black", aes(fill = mean_avg_intensity))+
  scale_fill_gradient(low= "grey", high= "purple")+
  theme(plot.title = element_text(hjust = 0.5, size = 16))
```

Grouping by steps 

```{r}
activity_sleep$user_steps <- " "

activity_sleep_grouped <- activity_sleep %>% 
  group_by (id) %>% 
  summarize(average_totalsteps = mean(totalsteps),
            average_totalcalories = mean(calories),
            average_totaldistance = mean(totaldistance),
            average_minutesasleep = mean(totalminutesasleep, na.rm = TRUE)) %>% 
  mutate(user_steps = case_when(
    average_totalsteps >= 10000 ~ "Highly Active/Active",
    average_totalsteps >= 7500 & average_totalsteps < 10000 ~ "Somewhat Active",
    average_totalsteps >= 5000 & average_totalsteps < 7500 ~ "Low Active",
    average_totalsteps < 5000 ~ "Sedentary"))

activity_sleep <- subset(activity_sleep, select = -user_steps)

activity_sleep_grouped <- merge(activity_sleep, activity_sleep_grouped, by= c("id"))
```

```{r}
ggplot(activity_sleep_grouped, aes(user_steps, totalminutesasleep))+
  geom_boxplot(aes(fill= user_steps))+
  geom_point(alpha = 0.5, aes(size = calories, color = calories))+
  labs(title = "Activity Level vs Daily Sleep Minutes", x = "Activity Level", y = "Daily Sleep Minutes", fill= "Activity Level", color= "Daily Calories Burned", caption= "Data Source: 
Physical activity for campus employees: a university worksite wellness program")+
  coord_flip()+
  scale_fill_brewer(palette="PiYG")+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none",fill ="none")
activity_sleep_grouped$user_steps <- factor(activity_sleep_grouped$user_steps, levels = c("Sedentary", "Low Active", "Somewhat Active", "Highly Active/Active"))
```

```{r}
ggplot(activity_sleep_grouped, aes(user_steps, totalsteps))+
  geom_boxplot(aes(fill= user_steps))+
  geom_point(alpha = 0.5, aes(size = calories, color = calories))+
  labs(title = "Activity Level vs Daily Steps", x = "Activity Level", y = "Daily Steps", fill= "Activity Level", size= "", color= "Daily Calories Burned", caption= "Data Source: Physical activity for campus employees: a university worksite wellness program")+
  coord_flip()+
  scale_fill_brewer(palette="PiYG")+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none",fill ="none")
```

```{r}
ggplot(data=activity, aes(x=totalsteps, y = calories, color=sedentaryminutes))+ 
  geom_point()+ 
  stat_smooth(method=lm)+
  scale_color_gradient(low="steelblue", high="red")+
  labs(title="Total Steps vs Calories")

```

