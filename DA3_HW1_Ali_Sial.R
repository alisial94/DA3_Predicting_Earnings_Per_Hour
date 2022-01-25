# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(tidyverse)
library(modelsummary)
library(fixest)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)
library(gridExtra)



#################
#  DATA IMPORT  #
#################
data_raw <- read.csv( 'https://osf.io/4ay9x/download', stringsAsFactors = TRUE)


################
# Data Munging #
################

# Selecting Occupation: "5700" i.e Retail Salesperson. 
df <- data_raw %>% 
  filter(occ2012==5700)


# Filtering data for only full time employees i.e. >=40 Hrs per week
df <- df %>%
  filter(uhours>=40) 

# Creating the target variable: Earnings per Hour as "eph"
df$eph <- (df$earnwke/df$uhours)


# Quick look at all variables
datasummary_skim(df)


# Checking for missing values count in the variables 
to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]             

# Dropping missing: in this case ethnic variable has 89% missing values
df$ethnic <- NULL     

# Investigating Variables #

########
## Earnings per hour (target variable)
ggplot(df , aes(x = eph)) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  theme_bw()

## it appears that the distribution has a right long tail, 
## therefore taking the log for this variable
ggplot(df , aes(x = log(eph))) +
  geom_histogram( fill='navyblue', color = 'white' ) +
  theme_bw()

########
## Age
ggplot( df , aes(x = age2, y = eph)) +
  geom_point(color='red',size=2,alpha=0.6) +
  geom_smooth(method="loess" , formula = y ~ x )+
  theme_bw()


## Adding age squared column
df$age2 <- df$age^2

########
## Edudcational Degree (grade92)
ggplot( df , aes(x = grade92, y = ep)) +
  geom_point(color='red',size=2,alpha=0.6) +
  geom_smooth(method="loess" , formula = y ~ x )+
  theme_bw()

## Dropping values unrealistic values in educational level for secretaries,
## in this case PhD and Doctorate will be dropped since its unlikely for 
## people working within this profession after such educational level

df <- df %>% 
  filter(grade92 <= 44)

## Regrouping the levels in grade92 variable based on the graph above for ep and grade92
## It made sense to combine the eduction level below 39 since whether the employer has
## completed 9th grade or 10th grade, both count as not having a diploma.
## 


df <- df %>% mutate(edu=case_when(
  grade92<=38 ~ "No Diploma",                 # for education levels of less than 12th grade
  grade92==39 ~ "Diploma",                    # High school, diploma, GED
  grade92==40 ~ "College without Degree",    # college education without degree
  grade92<=42 ~ "Associate Degree",           # Associate Degree
  grade92==43 ~ "Bachelors Degree",
  grade92==44 ~ "Masters Degree"
))

########
## Race
## Adding dummy variable for race, white and not white
df$race_dummy <- ifelse(df$race == 1, 'white','other')

########
## Sex 
## Redefining the sex variable as Male and Female
df$gender <- ifelse(df$sex == 1, 'male', 'female')

#######
## Marital Status
## Regrouping the marital status "Married, "Single Parent", "Never Married"

df <- df %>% mutate(marital_status=case_when(
  marital<=2 ~ "Married",
  marital<=6 ~ "Single Parent",
  marital==7 ~ "Never Married"
))

#######
## Own Child
## Creating binary for own child 
df <- df %>% mutate(ownchild=case_when(
  ownchild==0 ~ 0,
  TRUE ~ 1))

#######
## Class
datasummary( eph*factor(class) ~ N + Percent() + Mean, data = df ) 
# will include as factor because this kind occupation has a difference in earnings 
# based on class - so it will be added as is

#######
## Union Membership 
datasummary( eph*factor(unionmme) ~ N + Percent() + Mean, data = df ) 
## will be included inthe model


#######
## Stfips 

datasummary( eph*factor(stfips) ~ N + Percent() + Mean, data = df ) 

## Stfips and state variable are kind of similar with alot of different values
## Therefor, i'll be grouping these states in 4 regions to avoid complexity

df <- df %>% mutate(region=case_when(
  stfips %in% c("WA", "OR", "MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM", "HI", "AK", "CA") ~ "West",
  stfips %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH") ~ "Mid-West",
  stfips %in% c("OK", "TX", "AR", "LA", "KY", "TN", "MS", "AL", "WV", "VA", "NC", "SC", "GA", "FL", "DC","MD","DE") ~ "South",
  stfips %in% c("PA", "NY", "VT", "NH", "ME","MA","RI","CT","NJ") ~ "North East"
))


# Removing all unwanted variable
df_clean <- df %>% select(-c(hhid,intmonth,stfips,weight,earnwke,uhours,grade92,
                             race,sex,marital,chldpres,prcitshp,state,ind02,unioncov,
                             lfsr94,occ2012,state))

## Variables dropped that were not explained or explored above are dropped
## becuase either there wasn't much variation in the observations
## or a similar variable was already included for further modeling (e.g: prcitshp
## majority of the observations were from the same category thus adding not much value 
## to the data available). 


#################################
## Setting models interactions ##
#################################

# Investigating interactions within Variables #
## checking to see if further variables or reclassification of variables is required

### 1
## Earnings, Education and Gender
datasummary( eph*factor(edu)*gender ~ N + Percent() + Mean, data = df_clean )
# the summary suggest that their is a difference in earnings based on education and race

gg1 <- ggplot(df_clean, aes(x = factor(edu), y = eph,
                            fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Education Level",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle=45, vjust=.5))

gg1


### 2
## Earnings, Race and Gender
datasummary( eph*factor(race_dummy)*gender ~ N + Percent() + Mean, data = df_clean )
# it appears that majority of the employees are white and earnings differ based on summary

gg2 <- ggplot(df_clean, aes(x = factor(race_dummy), y = eph,
                            fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Race",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() 

gg2


### 3
## Earnings, Education Level, Race and Gender
datasummary( eph*edu*race_dummy*gender ~ N + Percent() + Mean, data = df_clean )
# There appears to be a difference in earngs based on all the 3 variables
# Graph below only for Education and Race comparison

gg3 <- ggplot(df_clean, aes(x = factor(edu), y = eph,
                            fill = factor(race_dummy), color=factor(race_dummy))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Education Level",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle=45, vjust=.5))


gg3

### 4
## Earnings, Marital Status and Gender
datasummary( eph*marital_status*gender ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different for marital status and gender

gg4 <- ggplot(df_clean, aes(x = marital_status, y = eph,
                            fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Marital Status",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() 

gg4


### 5
## Earnings, Union Member and Gender
datasummary( eph*unionmme*gender ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different for union member and gender

gg5 <- ggplot(df_clean, aes(x = unionmme, y = eph,
                            fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Union Member",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() 

gg5


### 6
## Earnings, Own Child and Gender
datasummary( eph*factor(ownchild)*gender ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different especially for males for if they have a child

gg6 <- ggplot(df_clean, aes(x = factor(ownchild), y = eph,
                            fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Own Child",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() 

gg6


### 7
## Earnings, Class and Gender
datasummary( eph*factor(class)*gender ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different based on class and gender

gg7 <- ggplot(df_clean, aes(x = factor(class), y = eph,
                            fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Class",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle=45, vjust=.5))

gg7


### 8
## Earnings, Region and Gender
datasummary( eph*gender*region ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different based on gender and region

gg8 <- ggplot(df_clean, aes(x = factor(region), y = eph,
                            fill = factor(gender), color=factor(gender))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Region",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist() +
  theme(axis.text.x = element_text(angle=45, vjust=.5))

gg8




### 9
## Earnings, Race and Marital Status
datasummary( eph*race_dummy*marital_status ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different based on race and marital status

gg9 <- ggplot(df_clean, aes(x = factor(marital_status), y = eph,
                            fill = factor(race_dummy), color=factor(race_dummy))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Race",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist()

gg9


### 10
## Earnings, Union Member and Class
datasummary( eph*unionmme*class ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different based on union membership and class

gg10 <- ggplot(df_clean, aes(x = factor(class), y = eph,
                             fill = factor(unionmme), color=factor(unionmme))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Class",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist()+
  theme(axis.text.x = element_text(angle=45, vjust=.5))

gg10



### 11
## Earnings, Union Member and Region
datasummary( eph*unionmme*region ~ N + Percent() + Mean, data = df_clean )
# It appears that earnings is different based on union membership and region, especially
# in the case of south region


gg11 <- ggplot(df_clean, aes(x = factor(region), y = eph,
                             fill = factor(unionmme), color=factor(unionmme))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c('red','blue')) +
  scale_fill_manual(name="",
                    values=c('red','blue')) +
  labs(x = "Region",y = "Earnings per Hour (USD)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 50), breaks = seq(0,50, 10))+
  ggthemes::theme_economist()+
  theme(axis.text.x = element_text(angle=45, vjust=.5))

gg11



