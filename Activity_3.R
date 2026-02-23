library(dplyr)
library(ggplot2)
library(lubridate)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
climCH <- read.csv("/cloud/project/activity03/climate-change.csv")
## IN CLASS DEMO

colnames(datCO2)[4] <- "CO2"

datCO2$Entity <- as.factor(datCO2$Entity) #Careful when doing this, can mess things up

US <- datCO2 %>%
  filter(Entity == "United States")

plot(US$Year, US$CO2,
     type = "b",
     pch=19,
     xlab="Year",
     ylab="Fossil Fuel Emissions (Billions of Tons of CO2)",
     yaxt="n")
axis(2, seq(0,6000000000, by=2000000000),
     seq(0,6, by=2), las=2) #What is las, figure out

#plus symbol chains data to geometry we want - specific to ggplot
ggplot(US, aes(x=Year,y=CO2))+
  geom_point()+
  labs(x="Year",y="US Fossil Fuel CO2 Emissions (tons)")+
  theme_classic() #Gets rid of gridlines

NorthA <- datCO2 %>%
  filter(Entity == "United States" |
           Entity == "Mexico" |
           Entity == "Canada")

ggplot(NorthA,
       aes(x=Year,y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c('red','royalblue','darkgoldenrod3')) #matches argument in aes: color=color, fill=fill

#CW PROMPT 1
colnames(climCH)[4] <- 'tempAnom'

climCH$date <- ymd(climCH$Day)

North <- climCH %>%
  filter(Entity == "Northern Hemisphere")
South <- climCH %>%
  filter(Entity == "Southern Hemisphere")

climNoWorld <- climCH %>%
  filter(Entity == "Northern Hemisphere" |
           Entity == "Southern Hemisphere")

ggplot(climNoWorld,
        aes(x=date,y=tempAnom,color=Entity))+
  geom_point()+
  geom_line()+
  theme_classic()+
  labs(x="Date (Year/Month/Day)",y="Temperature Anomaly by Hemisphere (Celcius)")

plot(North$date, North$tempAnom,
     type = "b",
     pch=19,
     xlab="Date (Year/Month/Day)",
     ylab="Temp Anomaly in Northern Hemisphere (°C)",
     yaxt="n")
axis(2, seq(-2,2, by=1),
     seq(-2,2, by=1), las=2)

plot(South$date, South$tempAnom,
     type = "b",
     pch=19,
     xlab="Date (Year/Month/Day)",
     ylab="Temp Anomaly in Southern Hemisphere(°C)",
     yaxt="n")
axis(2, seq(-1,1, by=0.25),
     seq(-1,1, by=0.25), las=2)

## CW PROMPT 2
NorthASum <- NorthA %>%
  group_by(Entity) %>%
  summarise(sumCO2 = sum(CO2))
## Plot
ggplot(NorthASum, aes(x=reorder(Entity, -sumCO2),y=sumCO2,fill=Entity, sort)) +
  geom_col()+
  theme_classic()+
  labs(x="Country", y="All-time CO2 Emissions")
  

### HOMEWORK

##HW1

BRIC <- datCO2 %>%
  filter(Entity == "Brazil" |
           Entity == "Russia" |
           Entity == "India" |
           Entity == "China")

ggplot(BRIC, aes(x=Year, y=CO2, col=Entity,group=Entity))+
  geom_line()+
  labs(title="CO2 Emissions Over Time in BRIC Countries", y="CO2 Emissions")+
  theme_classic()

##HW2

CO2Year <- datCO2 %>%
  group_by(Year) %>%
  summarise(sumYear = sum(CO2,group=Year))

ggplot(CO2Year, aes(x=Year,y=sumYear))+
  geom_line(col="royalblue")+
  labs(title="Global CO2 Emissions by Year",y="CO2 Emissions")+
  theme_classic()

#Graph 2

TempAnom <- climCH %>%
  group_by(date) %>%
  summarise(meanAnom = mean(tempAnom,group=date))

ggplot(TempAnom, aes(x=date,y=meanAnom))+
  geom_line(col="darkred")+
  labs(title="Global Temperature Anomaly by Year",x="Date",y="Temperature Anomaly")+
  theme_classic()

##HW3

Renew <- read.csv("/cloud/project/activity03/renewable-share-energy.csv")
RenewYear <- Renew %>%
  group_by(Year) %>%
  summarise(GlobalRenew = mean(Renewables,group=Year))

ggplot(RenewYear, aes(x=Year,y=GlobalRenew))+
  geom_line(col="forestgreen")+
  theme_classic()+
  labs(title="Global Proportion of Energy from Renewable Sources", y="Proportion of Renewable Energy (%)")
