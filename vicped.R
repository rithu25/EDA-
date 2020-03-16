# Install and load needed packages
install.packages(mongolite)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

#---------- Import packages ----------
library(mongolite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

#----------- Import dataset --------
# Connecting to mongo database
df2017 <- mongo(collection ="2017_c", db = "ass2db")
df2018 <- mongo(collection ="2018_c", db = "ass2db")
df2019 <- mongo(collection ="2019_c", db = "ass2db")

##location data

location <- mongo(collection ="loc", db = "ass2db")
locc <- location$find()

#check type
class(df2017)

#converting to dataframe
df_2017 <- df2017$find()
head(df_2017)

df_2018 <- df2018$find()
head(df_2018)

df_2019 <- df2019$find()
head(df_2019)

#new_df <- merge(df_2017, df_2018) %>%
#  merge(df_2019)
# Aggregating based on months
df1 <- aggregate(df_2017$Hourly_Counts, by=list(Month=df_2017$Month), FUN=sum)
df1

df2 <- aggregate(df_2018$Hourly_Counts, by=list(Month=df_2018$Month), FUN=sum)
df2

df3 <- aggregate(df_2019$Hourly_Counts, by=list(Month=df_2019$Month), FUN=sum)
df3

year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017)
df1["Year"] <- year
year <- c(2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018)
df2["Year"] <- year
year <- c(2019,2019,2019,2019,2019,2019,2019,2019)
df3["Year"] <- year

#combining the datasets 
new_df <- rbind(df1, df2)
new_df <- rbind(new_df,df3)
#new_df
new_df

new_df$Month = factor(new_df$Month,levels=c("January","February","March", "April","May","June","July","August","September", "October","November","December"),ordered=TRUE)

#Pedestrian flow through different months in the 3 years.  
ggplot(new_df, aes(x = Month, y = x, group = Year ,colour = as.factor(Year))) + geom_line() +  ggtitle("Pedestrian Flow through different months in 2017,2018 & 2019") + 
  xlab("Months") + ylab("Pedestrian Count") + labs(colour = "Years") + theme(plot.title = element_text(hjust = 0.5)) +theme_cleveland()

# Based on days - Which days are crowded in the past two years and 2019?
df1 <- aggregate(df_2017$Hourly_Counts, by=list(Day=df_2017$Day), FUN=sum)
df1

df2 <- aggregate(df_2018$Hourly_Counts, by=list(Day=df_2018$Day), FUN=sum)
df2

df3 <- aggregate(df_2019$Hourly_Counts, by=list(Day=df_2019$Day), FUN=sum)
df3


# Year 2017
ggplot(data = df1, mapping = aes(x = "", y = x, fill = Day)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(df1$x)), 1)) +
  coord_polar("y", start = 0) +  ggtitle("Which days were crowded in 2017?") + 
  xlab("Pedestrian Count") + ylab("Pedestrian Count") + theme(plot.title = element_text(hjust = 0.5)) 



#Year 2018
ggplot(data = df2, mapping = aes(x = "", y = x, fill = Day)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(df2$x)), 1)) +
  coord_polar("y", start = 0) +  ggtitle("Which days were crowded in 2018?") + 
  xlab("Pedestrian Count") + ylab("Pedestrian Count") + theme(plot.title = element_text(hjust = 0.5)) 


#Year 2019
ggplot(data = df3, mapping = aes(x = "", y = x, fill = Day)) + 
  geom_bar(width = 1, stat = "identity") + 
  scale_y_continuous(breaks = round(cumsum(rev(df3$x)), 1)) +
  coord_polar("y", start = 0) +  ggtitle("Which days are mostly crowded in 2019 so far?") + 
  xlab("Pedestrian Count") + ylab("Pedestrian Count") + theme(plot.title = element_text(hjust = 0.5)) 


###On the crowded day, which station is most crowded?
##It was found that Friday was the most crowded day. 

d_17 <- df_2017 %>% filter(Day == 'Friday')
d_18 <- df_2018 %>% filter(Day == 'Friday')
d_19 <- df_2019 %>% filter(Day == 'Friday')

df_17 <- d_17 %>% 
  arrange(desc(Hourly_Counts)) %>% slice(1:5)

df_18 <- d_18 %>% 
  arrange(desc(Hourly_Counts)) %>% slice(1:5)

df_19 <- d_19 %>% 
  arrange(desc(Hourly_Counts)) %>% slice(1:5)


ggplot(df_17, aes(Sensor_Name, Hourly_Counts)) +   
  geom_bar(aes(fill = Sensor_Name), position = "dodge", stat="identity") + 
  ggtitle("Stations with highest pedestrian count on the most crowded day in 2017") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Stations") +  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(df_18, aes(Sensor_Name, Hourly_Counts)) +   
  geom_bar(aes(fill = Sensor_Name), position = "dodge", stat="identity") + 
  ggtitle("Stations with highest pedestrian count on the most crowded day in 2018") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Stations") +  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(df_19, aes(Sensor_Name, Hourly_Counts)) +   
  geom_bar(aes(fill = Sensor_Name), position = "dodge", stat="identity") + 
  ggtitle("Stations with highest pedestrian count on the most crowded day in 19") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Stations") +  theme(plot.title = element_text(hjust = 0.5)) 


#Top 5 places that are crowded at different times of the day 
## Based on the time of the day 
df1 <- aggregate(df_2017$Hourly_Counts,df_2017$Sensor_Name, by=list(Time=df_2017$Time), FUN=sum)
df1

df2 <- aggregate(df_2018$Hourly_Counts, df_2018$Sensor_Name, by=list(Time=df_2018$Time), FUN=sum)
df2

df3 <- aggregate(df_2019$Hourly_Counts, df_2019$Sensor_Name, by=list(Time=df_2019$Time), FUN=sum)
df3

### For tableau ---------------------------------------------------------------- 
t_17 <- df_2017[,c(3,6,7,8,9,10)]
t_17 <- aggregate(t_17$Hourly_Counts, by=list(t_17$Sensor_Name, t_17$Time, t_17$Day,t_17$Sensor_ID), FUN=sum)
levels <- c(-Inf, 11, 16,Inf)
labels <- c("Morning", "Afternoon", "Night")
t_17 <- t_17 %>% mutate(y = cut(Group.2, levels, labels = labels))

t_18 <- df_2018[,c(3,6,7,8,9,10)]
t_18 <- aggregate(t_18$Hourly_Counts, by=list(t_18$Sensor_Name, t_18$Time, t_18$Day,t_18$Sensor_ID), FUN=sum)
levels <- c(-Inf, 11, 16,Inf)
labels <- c("Morning", "Afternoon", "Night")
t_18 <- t_18 %>% mutate(y = cut(Group.2, levels, labels = labels))

t_19 <- df_2019[,c(3,6,7,8,9,10)]
t_19 <- aggregate(t_19$Hourly_Counts, by=list(t_19$Sensor_Name, t_19$Time, t_19$Day,t_19$Sensor_ID), FUN=sum)
levels <- c(-Inf, 11, 16,Inf)
labels <- c("Morning", "Afternoon", "Night")
t_19 <- t_19 %>% mutate(y = cut(Group.2, levels, labels = labels))

new_crowd <- rbind(t_17,t_18,t_19)
write.csv(new_crowd,'byhour.csv')


#--------------------------------------------------------------------------------

new_2017 <- aggregate(df_2017$Hourly_Counts, by=list(df_2017$Sensor_Name, df_2017$Time), FUN=sum)
new_2018 <- aggregate(df_2018$Hourly_Counts, by=list(df_2018$Sensor_Name, df_2018$Time), FUN=sum)
new_2019 <- aggregate(df_2019$Hourly_Counts, by=list(df_2019$Sensor_Name, df_2019$Time), FUN=sum)


ggplot(new_df, aes(x = month, y = x, group = Year ,colour = as.factor(Year))) + geom_line()

levels <- c(-Inf, 11, 16,Inf)
labels <- c("Morning", "Afternoon", "Night")
new_2017 <- new_2017 %>% mutate(y = cut(Group.2, levels, labels = labels))
new_2018 <- new_2018 %>% mutate(y = cut(Group.2, levels, labels = labels))

new_2019 <- new_2019 %>% mutate(y = cut(Group.2, levels, labels = labels))

#Top 5 places that are crowded at different times of the day 
#2017
crowd_2017 <- new_2017 %>% 
  arrange(desc(x)) %>% 
  group_by(y) %>% slice(1:5)

ggplot(crowd_2017, aes(Group.1, x)) +   
  geom_bar(aes(fill = y), position = "dodge", stat="identity") + 
  ggtitle("Top 5 Stations with highest pedestrian count at different time periods - 2017") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Time of the day") +  theme(plot.title = element_text(hjust = 0.5)) 

#2018

crowd_2018 <- new_2018 %>% 
  arrange(desc(x)) %>% 
  group_by(y) %>% slice(1:5)

ggplot(crowd_2018, aes(Group.1, x)) +   
  geom_bar(aes(fill = y), position = "dodge", stat="identity") +
  ggtitle("Top 5 Stations with highest pedestrian count at different time periods - 2018") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Time of the day") +  theme(plot.title = element_text(hjust = 0.5)) 

#2019
crowd_2019 <- new_2019 %>% 
  arrange(desc(x)) %>% 
  group_by(y) %>% slice(1:5)

ggplot(crowd_2019, aes(Group.1, x)) +   
  geom_bar(aes(fill = y), position = "dodge", stat="identity") +
  ggtitle("Top 5 Stations with highest pedestrian count at different time periods - 2019") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Time of the day") +  theme(plot.title = element_text(hjust = 0.5)) 

#Locations with the highest pedestrian count and the days on which it is crowded

sensor_2018 <- df_2018 %>% 
  arrange(desc(Hourly_Counts)) %>% slice(1:5)

ggplot(sensor_2018, aes(Sensor_Name, Hourly_Counts)) +   
  geom_bar(aes(fill = Day), position = "dodge", stat="identity") + 
  ggtitle("Stations with highest pedestrian count in 18") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Stations") +  theme(plot.title = element_text(hjust = 0.5)) 

sensor_2017 <- df_2017 %>% 
  arrange(desc(Hourly_Counts)) %>% slice(1:5)

ggplot(sensor_2017, aes(Sensor_Name, Hourly_Counts)) +   
  geom_bar(aes(fill = Day), position = "dodge", stat="identity") + 
  ggtitle("Stations with highest pedestrian count in 17") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Stations") +  theme(plot.title = element_text(hjust = 0.5)) 

sensor_2019 <- df_2019 %>% 
  arrange(desc(Hourly_Counts)) %>% slice(1:5)

ggplot(sensor_2019, aes(Sensor_Name, Hourly_Counts)) +   
  geom_bar(aes(fill = Day), position = "dodge", stat="identity") + 
  ggtitle("Stations with highest pedestrian count in 19") + 
  xlab("Station Names") + ylab("Pedestrian Count") + labs(fill = "Stations") +  theme(plot.title = element_text(hjust = 0.5)) 


pppp <- rbind(df_2017,df_2018,df_2019)
ppppp <- pppp %>% filter(Day == 'Friday')
pppppp <- ppppp %>% filter(Sensor_Name == c('Bourke St Bridge','Southbank','Birrarung Marr'))
write.csv(pppppp,'forecast.csv')
write.csv(ppppp,'fulldate.csv')

#-------Prediction (trial)------------

typeof(df_2019$Time)
dfnew2017 <- df_2017[,c(4,5,7,8,9,10)]
colnames(dfnew2017)[6]<-"2017"



dfnew2019 <- df_2019[,c(4,5,7,8,9,10)]
colnames(dfnew2019)[6]<-"2019"

dfnew2018 <- df_2018[,c(4,5,7,8,9,10)]
colnames(dfnew2018)[6]<-"2018"

common_col_names <- intersect(names(dfnew2018), names(dfnew2019))
common_col_names
yo <- merge.data.frame(dfnew2018, dfnew2019, by=common_col_names, all.x=TRUE)

common_col_names <- intersect(names(dfnew2017), names(yo))
common_col_names
yo <- merge.data.frame(dfnew2017, yo, by=common_col_names, all.x=TRUE)

library(reshape2)
yoyo <- yo[,c(1,6,7,8)]
yoyo <- melt(yoyo, id.vars='Month')
#ggplot it. x axis will be m, y will be the value and fill will be
#essentially your x,y,z
library(ggplot2)
ggplot(yoyo, aes(x=Month, y=value, fill=variable)) + geom_bar(stat='identity')

#loading package
library(caTools)
#use caTools function to split, SplitRatio for 70%:30% splitting
yo1= sample.split(yo,SplitRatio = 0.3)

#subsetting into Train data
train =subset(yo,yo1==TRUE)

#subsetting into Test data
test =subset(yo,yo1==FALSE)

yo[is.na(yo)] <- 0
yo <- na.omit(yo)

yo[`2020`] <- 0

model <- lm(yo$`2019` ~ yo$`2017` + yo$`2018`+yo$Mdate)

summary(model)

verynew <- data.frame(yo$`2018`,yo$`2017`,yo$Mdate)
verynew$`2019` <- predict(model, newdata = verynew)












