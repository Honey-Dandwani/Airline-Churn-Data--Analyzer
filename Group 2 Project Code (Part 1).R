#################################################################

# IST687 M003 Final Project Code (Part 1)
# 1. Processing
# 2. Attribute analysis
# 3. Linear modeling
#----------------------------------------------------------------
# GROUP 2:
#  WENZHE YANG, TIEHAO CHEN, CHENG LI, SAHEB SINGH, YIYUAN CHENG

#################################################################

# setwd()

library(tidyverse)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)

#---------------------------------Reading data---------------------------------
df<-jsonlite::fromJSON("fall2019-survey-M03.json")

str(df)
head(df,10)

#----------------------------------Processing----------------------------------
#------------------------------Attribute analysis------------------------------

# lower the character of states
df$Origin.State<-tolower(df$Origin.State)
df$Destination.State<-tolower(df$Destination.State)

# table shows that most of cites in a same level, except last 20
?summarise
Cities<-df%>%
  group_by(Destination.City,Destination.State)%>%
  summarise(f=n())
colnames(Cities)<-c('City','stateName','Destination_frequency')

# Frequency analyze of Origin cities
Cities2<-df%>%
  group_by(Origin.City,Origin.State)%>%
  summarise(f=n())
colnames(Cities2)<-c('City','stateName','Origin_frequency')


#Merge all the data
?merge
cities<-merge(Cities,Cities2,by=c('City','stateName'),all = TRUE)
cities[is.na(cities)]<-0

cities$Mean<-(cities$Destination_frequency + cities$Origin_frequency) /2
cities$Diff<-(cities$Destination_frequency - cities$Origin_frequency)
cities$Diff_rate<-cities$Diff/cities$Mean
# lowest 10 Diff_rate
head(cities[order(cities$Diff_rate),],10)
# lowest 10 Diff
head(cities[order(cities$Diff),],10)
# Highest 10 Diff_rate
tail(cities[order(cities$Diff_rate),],10)
# Highest 10 Diff
tail(cities[order(cities$Diff),],10)

# visualization
OriCity<-df%>%
  group_by(Origin.City,Origin.State)%>%
  summarise(long=mean(olong),lat=mean(olat))
colnames(OriCity)<-c('City','stateName','clong','clat')

DesCity<-df%>%
  group_by(Destination.City,Destination.State)%>%
  summarise(long=mean(dlong),lat=mean(dlat))
colnames(DesCity)<-c('City','stateName','clong','clat')

# merge lat and long
tempCity<-merge(OriCity,DesCity,all = TRUE, by=c('City','stateName'))
tempCity$clat<-tempCity$clat.x
tempCity$clat[is.na(tempCity$clat)]<-tempCity$clat.y[is.na(tempCity$clat)]
tempCity$clong<-tempCity$clong.x
tempCity$clong[is.na(tempCity$clong)]<-tempCity$clong.y[is.na(tempCity$clong)]

cities<-merge(cities,tempCity[,c(-3,-4,-5,-6)],by =c('City','stateName'))


# adding stateName
us <- map_data("state")
simplemap<- ggplot(us)+
  geom_polygon(color='black',fill='white',aes(x=long,y=lat,group=group))+
  coord_map()
simplemap

cities$stateName<-tolower(cities$stateName)
colnames(cities)[2]<-'region'
usmap<-merge(us,cities,by='region',all = TRUE)
usmap<-usmap%>%arrange(order)


# most popular city
colormap<-ggplot(usmap)+
  geom_polygon(color='black',aes(x=long,y=lat,group=group))+
  geom_point(aes(x=clong,y=clat,size=Mean,color="yellow"))+
  coord_map()
colormap

ggplot(df)+
  geom_segment(aes(x=olong,y=olat,xend=dlong,yend=dlat,color='green'),
               arrow = arrow(length = unit(0.03, "inches")),alpha = 0.05)
Edgemap<-simplemap+geom_segment(data = df, 
                                aes(x=olong,y=olat,xend=dlong,yend=dlat,color='green'),
                                arrow = arrow(length = unit(0.03, "inches")), alpha = 0.05)
Edgemap
df$Destination.State

#add cities counter to df as new attribute
Ocity<-cities[,c(1,2,4,5)]
colnames(Ocity)<-c('Origin.City','Origin.State','Ocounter','Omean')

df<-merge(df,Ocity,by=c('Origin.City','Origin.State'))

Dcity<-cities[,c(1,2,3,5)]
colnames(Dcity)<-c('Destination.City','Destination.State','Dcounter','Dmean')

df<-merge(df,Dcity,by=c('Destination.City','Destination.State'))

#---------airline status---------
str(df$Airline.Status)
# the order of airline status is platinum, gold, silver, and blue.
df$Airline.Status<-factor(df$Airline.Status,ordered = TRUE,
                          levels = c('Blue','Silver','Gold','Platinum'))
str(df$Airline.Status)

#---------Gender---------
str(df$Gender)
#its type is character right now.
df$Gender<-factor(df$Gender)
summary(df$Gender)

#---------Price sensitive---------
#order the price senstive
df$Price.Sensitivity<-factor(df$Price.Sensitivity,ordered = TRUE)

#---------Type of Travel---------
#character to factor
df$Type.of.Travel<-factor(df$Type.of.Travel)
summary(df$Type.of.Travel)
#Business travel Mileage tickets Personal Travel 
#6319             855            3108 

#----------Class----------
summary(factor(df$Class))
#order the class
df$Class<-factor(df$Class,ordered = TRUE,levels = c('Eco','Eco Plus','Business'))

#------Total.Freq.Flyer.Accts-----
#need to be discussed
#half are sparse
summary(factor(df$Total.Freq.Flyer.Accts))

#---------Shopping.Amount.at.Airport------
#half are sparse
summary(factor(df$Shopping.Amount.at.Airport))

#---------Eating.and.Drinking.at.Airport------
summary(factor(df$Eating.and.Drinking.at.Airport))

#---------Partner Code and Name-----
df$Partner.Code<-factor(df$Partner.Code)
summary(df$Partner.Code)
# it shows there are 14 companies
df$Partner.Name<-factor(df$Partner.Name)

#---------Scheduled.Departure.Hour------------
hist(df$Scheduled.Departure.Hour,breaks = 24)

#-----------Delay-------------
#half are no delay
summary(factor(df$Departure.Delay.in.Minutes))
#half are no delay
summary(factor(df$Arrival.Delay.in.Minutes))

#------------Date-------------
df$DATE<-as.Date(df$Flight.date,format = "%m/%d/%y")
df$DayOfWeek<-factor(format(df$DATE,format='%A'),ordered = TRUE,
                     levels = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))
?barplot
barplot(table(df$DayOfWeek),las=2)

# holiday in this period, including adjacent weekend, are Jan 1st, Jan 18th to Jan 20th, Feb 15th to Feb 17th and Spring break (assuming March 9 to March 15)
df$holiday<-'No'
df$holiday[which(df$DATE=='2014-01-01')]<-'Yes'
df$holiday[which(df$DATE>='2014-01-18' & df$DATE<='2014-01-20')]<-'Yes'
df$holiday[which(df$DATE>='2014-02-15' & df$DATE<='2014-02-17')]<-'Yes'
df$holiday[which(df$DATE>='2014-03-09' & df$DATE<='2014-03-15')]<-'Yes'
df$holiday<-factor(df$holiday)
summary(df$holiday)

#---------------generate 4 new column of sparse data--------
df$NoAccount<-'No'
df$NoAccount[which(df$Total.Freq.Flyer.Accts==0)]<-'Yes'
df$NoAccount<-factor(df$NoAccount)

df$NoShopping<-'No'
df$NoShopping[which(df$Shopping.Amount.at.Airport==0)]<-'Yes'
df$NoShopping<-factor(df$NoShopping)

df$NoArrivalDelay<-'No'
df$NoArrivalDelay[which(df$Arrival.Delay.in.Minutes==0)]<-'Yes'
df$NoArrivalDelay<-factor(df$NoArrivalDelay)

df$NoDepartureDelay<-'No'
df$NoDepartureDelay[which(df$Departure.Delay.in.Minutes==0)]<-'Yes'
df$NoDepartureDelay<-factor(df$NoDepartureDelay)

#----------likelihood to recommand-------
df$NPS<-'detractors'
df$NPS[which(df$Likelihood.to.recommend>=7 &df$Likelihood.to.recommend<=8)]<-'passive'
df$NPS[which(df$Likelihood.to.recommend>8)]<-'promoters'
df$NPS<-factor(df$NPS)

#-------check and update the type of data-----
str(df)
df$Destination.City<-factor(df$Destination.City)
df$Origin.City<-factor(df$Origin.City)
df$Destination.State<-factor(df$Destination.State)
df$Origin.State<-factor(df$Origin.State)
df$Flight.cancelled<-factor(df$Flight.cancelled)
summary(df)

#------------------NA-------------------
sum(is.na(df$freeText))
# I want most of free text data will be used in Text mining part, so I will genreante a new dataframe to analysis
Text<-df[which(is.na(df$freeText)==FALSE),]
# Suppose free text will not work for analyse
df<-df[,-32]
# check whether someone doesn't rate the flight
sum(is.na(df$Likelihood.to.recommend))
# delete that row of data
df<-df[-which(is.na(df$Likelihood.to.recommend)==TRUE),]

# analyse whether Flight.cancelled influences a lot on df$Likelihood.to.recommend
mean(df$Likelihood.to.recommend[which(df$Flight.cancelled=="Yes")])
#[1] 6.511211
mean(df$Likelihood.to.recommend[which(df$Flight.cancelled=="No")],na.rm = TRUE)
#[1] 7.161265
#it shows that Flight.cancelled influence a lot on the result, so we will generate a dataset without any NA
dfnoCancelled<-df[which(df$Flight.cancelled=='No'),]
# Check again what kind of NA is missed
sum(is.na(dfnoCancelled))
which(is.na(dfnoCancelled),arr.ind =TRUE)
# there is a small numbers of NA caused by na in arrival Delay in minutes.
dfnoCancelledNoArrive<-dfnoCancelled[-which(is.na(dfnoCancelled$Arrival.Delay.in.Minutes)==TRUE),]
sum(is.na(dfnoCancelledNoArrive))
# No NA right now

# save data
write.csv(dfnoCancelledNoArrive, "updated.csv")

#------------------------------Attribute analysis------------------------------
#---------------------------------NPS analysis---------------------------------
library(readr)
df <- read_csv("updated.csv")


index1 <- which(df$Departure.Delay.in.Minutes != "0" 
                & df$Arrival.Delay.in.Minutes != "0")

index2 <- which(df$Departure.Delay.in.Minutes != "0" 
                & df$Arrival.Delay.in.Minutes == "0")

index3 <- which(df$Departure.Delay.in.Minutes == "0" 
                & df$Arrival.Delay.in.Minutes != "0")

index4 <- which(df$Departure.Delay.in.Minutes == "0" 
                & df$Arrival.Delay.in.Minutes == "0")


df$punctuality <- 0
df$punctuality[index1] <- "Both delay"
df$punctuality[index2] <- "Departure delay only"
df$punctuality[index3] <- "Arrival delay only"
df$punctuality[index4] <- "Both on time"
df$punctuality <- factor(df$punctuality, ordered = TRUE,
                         levels = c("Both delay", "Departure delay only", "Arrival delay only", "Both on time"))
summary(df$punctuality)

# If need seperate dataset, otherwise ignore
#df_delay <- df[index3,] ### Both delay
#df_D_delay <- df[index4,] ### Only departure delay
#df_A_delay <- df[index5,] ### Only arrival delay
#df_ontime <- df[index6,] ### Totally on time


# Look at categorical variables individually
ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  
  facet_grid(. ~ punctuality)
# Flights arriving both on time can significantly increase the likelihood.

df$Class<-factor(df$Class,ordered = TRUE,
                 levels = c('Eco','Eco Plus','Business'))
summary(df$Class)
ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ Class)
# Eco

summary(df$Type.of.Travel)
ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ Type.of.Travel)
# Personal travel

df$Airline.Status<-factor(df$Airline.Status,ordered = TRUE,
                          levels = c('Blue','Silver','Gold','Platinum'))
summary(df$Airline.Status)  
ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ Airline.Status)
# Blue

ggplot(df) +
  geom_histogram(aes(x=Flights.Per.Year, fill=cut(Flights.Per.Year, 100)), 
                 show.legend = FALSE) +
  facet_grid(. ~ NPS)
# No difference

ggplot(df) +
  geom_histogram(aes(x=Age, fill=cut(Age, 100)), 
                 show.legend = FALSE)+
  facet_grid(. ~ NPS)
# Middle age people tend to be promoters

summary(df$Gender)
ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ Gender)
# Female

ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend)) +
  facet_grid(. ~ Total.Freq.Flyer.Accts)

ggplot(df[df$Total.Freq.Flyer.Accts<6,]) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ Total.Freq.Flyer.Accts)
# 0

ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ Price.Sensitivity)
# 1

ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ NoShopping)
# Yes

ggplot(df[df$Shopping.Amount.at.Airport<250,]) +
  geom_histogram(aes(x=Shopping.Amount.at.Airport, 
                     fill=cut(Shopping.Amount.at.Airport, 100)), 
                 show.legend = FALSE) +
  facet_grid(. ~ NPS)
# No difference

ggplot(df[df$Eating.and.Drinking.at.Airport<300,]) +
  geom_histogram(aes(x=Eating.and.Drinking.at.Airport, 
                     fill=cut(Eating.and.Drinking.at.Airport, 100)), 
                 show.legend = FALSE) +
  facet_grid(. ~ NPS)
# <60


########

ggplot(df) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) +
  facet_grid(. ~ Type.of.Travel)

df$Type_Status <- paste(df$Type.of.Travel, ",",df$Airline.Status)
ggplot(df) +
  geom_density(aes(x=Likelihood.to.recommend, color=Type_Status)) +
  facet_grid(. ~ Class)

#########
p_travel<- df%>%
  filter(Type.of.Travel=='Personal Travel')%>%
  filter(Airline.Status=='Blue')

ggplot(p_travel) +
  geom_histogram(aes(x=Likelihood.to.recommend, fill=NPS)) +
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00"))

###############################################################################

#-------------------------------Linear modeling--------------------------------

mydata <- read_csv('updated.csv')

index1 <- which(mydata$Class == 'Eco Plus')
index2 <- which(mydata$Class == 'Eco')
index3 <- which(mydata$Class == 'Business')

mydata1 <- mydata[index1,]
mydata2 <- mydata[index2,]
mydata3 <- mydata[index3,]

index4 <- which(mydata1$Loyalty > 0)
mydata4 <- mydata1[index4,]

index5 <- which(mydata2$Loyalty > 0)
mydata5 <- mydata2[index5,]

index6 <- which(mydata3$Loyalty > 0)
mydata6 <- mydata3[index6,]

#####

model1 <- lm(formula = Loyalty ~ Flights.Per.Year,data=mydata4)
picture1 <- ggplot(mydata4,aes(y = Loyalty, x = Flights.Per.Year, color = Gender)) + 
  geom_point() + geom_smooth(method='loess')
picture1

model2 <- lm(formula = Loyalty ~ Flights.Per.Year,data=mydata1)
picture2 <- ggplot(mydata1,aes(y = Loyalty, x = Flights.Per.Year, color = Gender)) + 
  geom_point() + geom_smooth(method='loess')
picture2

model3 <- lm(formula = Loyalty ~ Flights.Per.Year,data=mydata2)
picture3 <- ggplot(mydata2,aes(y = Loyalty, x = Flights.Per.Year, color = Gender)) + 
  geom_point() + geom_smooth(method='loess')
picture3

model4 <- lm(formula = Loyalty ~ Flights.Per.Year,data=mydata5)
picture4 <- ggplot(mydata5,aes(y = Loyalty, x = Flights.Per.Year, color = Gender)) + 
  geom_point() + geom_smooth(method='loess')
picture4

model6 <- lm(formula = Loyalty ~ Flights.Per.Year,data=mydata6)
picture5 <- ggplot(mydata6,aes(y = Loyalty, x = Flights.Per.Year, color = Gender)) + 
  geom_point() + geom_smooth(method='loess')
picture5


#transfer categorial data into numeric data
mydata$Airline.Status[mydata$Airline.Status == "Blue" ]  <- 1
mydata$Airline.Status[mydata$Airline.Status == "Silver" ]  <- 2
mydata$Airline.Status[mydata$Airline.Status == "Gold" ]  <- 3
mydata$Airline.Status[mydata$Airline.Status == "Platinum" ]  <- 4

mydata$Gender[mydata$Gender == 'Male'] <- 1
mydata$Gender[mydata$Gender == "Female"] <- 0

mydata$Type.of.Travel[mydata$Type.of.Travel == "Business travel"] <- 1
mydata$Type.of.Travel[mydata$Type.of.Travel == "Mileage tickets"] <- 2
mydata$Type.of.Travel[mydata$Type.of.Travel == "Personal Travel"] <- 3

mydata$Class[mydata$Class == "Eco"] <- 1
mydata$Class[mydata$Class == "Eco Plus"] <- 2
mydata$Class[mydata$Class == "Business"] <- 3

mydata$Flight.cancelled[mydata$Flight.cancelled == "No"] <- 1
mydata$Flight.cancelled[mydata$Flight.cancelled == "Yes"] <- 0 

mydata$Age <- as.numeric(mydata$Age)
mydata$Airline.Status <- as.numeric(mydata$Airline.Status)
mydata$Gender <- as.numeric(mydata$Gender)
mydata$Price.Sensitivity  <- as.numeric(mydata$Price.Sensitivity)
mydata$Flights.Per.Year <- as.numeric(mydata$Flights.Per.Year)
mydata$Type.of.Travel <- as.numeric(mydata$Type.of.Travel)
mydata$Total.Freq.Flyer.Accts <- as.numeric(mydata$Total.Freq.Flyer.Accts)
mydata$Shopping.Amount.at.Airport <- as.numeric(mydata$Shopping.Amount.at.Airport)
mydata$Eating.and.Drinking.at.Airport <- as.numeric(mydata$Eating.and.Drinking.at.Airport)
mydata$Class <- as.numeric(mydata$Class)
mydata$Arrival.Delay.in.Minutes  <- as.numeric(mydata$Arrival.Delay.in.Minutes )
mydata$Flight.cancelled  <- as.numeric(mydata$Flight.cancelled)
mydata$Likelihood.to.recommend   <- as.numeric(mydata$Likelihood.to.recommend )

cor(mydata[c('Airline.Status','Age','Gender','Price.Sensitivity','Flights.Per.Year','Loyalty','Type.of.Travel','Total.Freq.Flyer.Accts','Shopping.Amount.at.Airport','Eating.and.Drinking.at.Airport','Class','Arrival.Delay.in.Minutes','Flight.cancelled','Likelihood.to.recommend','Departure.Delay.in.Minutes')])
# type of travel > flight.per.year> Airline.status > age > loyalty > Gender > arrival.delay.in.minutes > price.Sensitivity > eating and dring > total.freq.flyer.accts > class > shoping

model5 <- lm(formula = Likelihood.to.recommend ~ Type.of.Travel + Flights.Per.Year + 
               Airline.Status + Age + Loyalty + Gender + Arrival.Delay.in.Minutes + Price.Sensitivity + Eating.and.Drinking.at.Airport + Total.Freq.Flyer.Accts + Class + Shopping.Amount.at.Airport, data =mydata)
summary(model5)

