#-------------------------------------------------#-------------------------------------------------#
library(readabs) #package to read and clean the time-series dataset from Australia Bureau of Statistics
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(forecast)
library(zoo)
library(corrplot)
library(Hmisc)
library(car)
library(scales)
library(tseries)

#Set the environment to store the ABS time series file
Sys.setenv(R_READABS_PATH="Desktop")

#"labour_force_mont": monthly unemployment rate ; collected from Australia Bureau of Statistics (cat.no.6202.)
#"economy": quarterly data collected from Australia Bureau of Statistics (cat.no.5206.)
#"labour_force_quar": quarterly unemployment rate ; collected from International Labour Organization - ilostats: https://www.ilo.org/shinyapps/bulkexplorer15/
#Assumption (when using this dataset): 
#(1) size: dataset is large enough to reflect the real Au-labor-market
#(2) accuracy: the information provided is reliable
#(3) participants profile: people, who participate(ed) in the survey, can be represented AU workers

##Part 1. Exploratory data analysis 
#Labour market indicators
#Monthly unemployment rate (cat.no.6202.0 ; ABS): load the dataset by using read_abs function, which available in the reababs package
labour_force_mont <- read_abs("6202.0", tables = 1) #table 1 contains data that relevant to the analysis 

#First, evaluate the labour market's trend in general by reviewing the "Labour force total ; Persons" variable
#Decide to use "Seasonally Adjusted" series_type as it is accounted the seasonality of the data. 
#Besides, "Trend" series_type is not completed and "Original" series_type did not account the nature of time-series data
#filter & subset functions are used to collect the appropriate variables
Labour_market <- filter(labour_force_mont, series_type == "Seasonally Adjusted" & series == "Labour force total ;  Persons ;") 
Labour_market <- Labour_market %>% subset(select = c(date,value))
Labour_market %>%
  as.data.frame()

#labour_market.1920: narrow-down the dataset to Jan 2019-Nov2020 period to navigate the influences of Covid-19
labour_market.1920 <- tail(Labour_market,23)
#Plot the dataset
labour_market.1920 %>% 
  as.data.frame() %>%
  ggplot(aes(x = date, y = value/1000))+ #value is saved in "000" form - thousand; transforming to "000,000" form - million helps the visualisation process easier
  xlab("Time") + 
  ylab("People (million)") +
  ggtitle("Australia Labour Market 2019-20",subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics")+
  geom_line()
View(labour_force_mont)

#Un_rate: monthly unemployment rate  
#filter the series == Unemployment rate ;  Persons ;" to get the unemployment rate in total
#filter for series_type == "Seasonally Adjusted" as it helps to reflect the characteristics of the time-series data set & "trend" data is missing
Un_rate <- filter(labour_force_mont, series_type == "Seasonally Adjusted" & series == "Unemployment rate ;  Persons ;") 

#illuminate unrelevant data
Un_rate <- Un_rate %>% subset(select = c(date,value))
colnames(Un_rate) <- c("Time","Un_rate")

#Un_rate.1820: monthly unemployment rate from Jan 2018 to Nov 2020
Un_rate.1820 <- tail(Un_rate,35)

#Plot the dataset
Un_rate.1820 %>% 
  as.data.frame() %>%
  ggplot(aes(x = Time, y = Un_rate/100))+ #value is saved in Percent form - 6 decimal places; 
  scale_y_continuous(label = percent_format(accuracy = 1))+ #transforming to percentage format with unit only helps to improve the visualization quality
  xlab("Time") + 
  ylab("Percentage (%)") +
  ggtitle("Australia: Unemployment Rate 2018-20", subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics")+
  geom_line()

#Un_rate.1920: monthly unemployment rate from Jan 2019 to Nov 2020
Un_rate.1920 <- tail(Un_rate,23)
#Plot the dataset
Un_rate.1920 %>% 
  as.data.frame() %>%
  ggplot(aes(x = Time, y = Un_rate/100))+ #value is saved in Percent form - 6 decimal places; 
  scale_y_continuous(label = percent_format(accuracy = 1))+ #transforming to percentage format with unit only helps to improve the visualization quality
  xlab("Time") + 
  ylab("Percentage (%)") +
  ggtitle("Australia: Unemployment Rate 2019-20", subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics")+
  geom_line()

#Em_rate: monthly employment rate
#Review the Employment Rate by using filter function on series "Employment to population ratio ;  Persons ;"
Em_rate <- filter(labour_force_mont, series_type == "Seasonally Adjusted" & series == "Employment to population ratio ;  Persons ;") 
Em_rate <- Em_rate %>% subset(select = c(date,value))
colnames(Em_rate)<- c("Time","Em_rate")

#plot the Employment Rate from 1978 to 2020 to discover the trend
Em_rate %>% as.data.frame() %>%
  ggplot(aes(x = Time, y = Em_rate/100))+
  xlab("Year") + 
  ylab("Employment rate(%)") +
  ggtitle("Monthly Employment Rate in Australia from 1978 to 2020")+
  geom_line()


Em_rate.1920 <- tail(Em_rate,23)
#Plot the dataset
Em_rate.1920 %>% 
  as.data.frame() %>%
  ggplot(aes(x = Time, y = Em_rate/100))+ #value is saved in Percent form - 6 decimal places; 
  scale_y_continuous(label = percent_format(accuracy = 1))+ #transforming to percentage format with unit only helps to improve the visualization quality
  xlab("Time") + 
  ylab("Percentage (%)") +
  ggtitle("Australia: Employment Rate 2019-20", subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics")+
  geom_line()


#Not in labour market: people who at least 15 yrs but not looking for jobs actively
Not_Labour <- filter(labour_force_mont,series == "Not in the labour force (NILF) ;  Persons ;") 
Not_Labour <- Not_Labour %>% 
  subset(select = c(date,value)) %>% 
  as.data.frame()

#Review the number of people not in labour force from 2018 till now
Not_Labour <- tail(Not_Labour,35) #dataset contains monthly data ; 35 months from Jan 2019 to Nov 2020

summary(Not_Labour) #median: ~7 (million)
#Plot the dataset
Not_Labour %>% 
  as.data.frame() %>%
  ggplot(aes(x = date, y = value/1000))+ #value is saved in "000" form - thousand; transforming to "000,000" form - million helps the visualisation process easier
  xlab("Time") + 
  ylab("People (million)") +
  ggtitle("Australia: Not in Labour Market 2018-20",subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics")+
  geom_line()

#Pop: the number of people aged 15 years and over
Pop <- filter(labour_force_mont,series == "Civilian population aged 15 years and over ;  Persons ;") 
Pop <- Pop %>% subset(select = c(date,value))

#Review the number of population (over 15rs) from 2018 till now
Pop <- tail(Pop,35) #dataset contains monthly data ; 35 months from Jan 2019 to Nov 2020
Pop %>% 
  as.data.frame() %>%
  ggplot(aes(x = date, y = value/1000))+ #value is saved in "000" form - thousand; transforming to "000,000" form - million helps the visualisation process easier
  xlab("Time") + 
  ylab("People (million)") +
  ggtitle("Australia: Population aged 15 years and over 2018-20",subtitle = "Monthly series 6202.0 from the Australian Bureau of Statistics")+
  geom_line()

#Economy indicators: all parameters are / were recorded quarterly ; hence, need to acquire another dataset recorded labour market data quarterly in the later stage 
#Quarterly business indicators (cat.no.5206.0 ; ABS): load the dataset by using read_abs function, which available in the reababs package
economy <- read_abs("5206.0", tables = 1) #table 1 contains data that relevants to the analysis 
#Clean the economy dataset and use the "Seasonally Adjusted" data only for the calculation because (1) "trend" data is not completed and (2) "Original" data does not reflected the nature of time-series data
economy <- filter(economy, series_type == "Seasonally Adjusted") 

#Check the variables to select the right parameter 
unique(economy$series)

#GDP: Percentage change of Gross Demestic Product (GDP) in Australia
GDP <- filter(economy, series == "Gross domestic product: Current prices - Percentage Changes ;") 
GDP <- GDP %>% 
  subset(select = c(date,value)) %>%
  as.data.frame()
head(GDP)
#Convert "time" variable from character to time-series format to plot it
Time.GDP <-as.yearqtr(GDP$date, forformat = "%%Y-%m-%d" )
GDP.quar <- cbind(Time.GDP, GDP)  #create new dataset
GDP.quar[,2] <- NULL #remove the outdated date format

#Narrow-down the dataset to Jan 2019-Nov2020 period to navigate the influences of Covid-19
GDP.1920 <- tail(GDP.quar,7) #the dataset contains observations recorded quarterly
head(GDP.1920,1)
#Plot the dataset GDP from 1Q2019 to 3Q2020
GDP.1920 %>% 
  as.data.frame() %>%
  ggplot(aes(x = Time.GDP, y = value/100))+ #value saved in percentage 
  scale_y_continuous(label = percent_format(accuracy = 1))+ #transforming to percentage format with unit only helps to improve the visualization quality
  scale_x_yearqtr(format = "%Y-%q", n = 7)+
  xlab("Time") + 
  ylab("Percentage (%)") +
  ggtitle("Australia: GDP (Aggregate Demand) %Change from 1Q2019 to 3Q2020",subtitle = "Quarterly series 5206.0 from the Australian Bureau of Statistics")+
  geom_line()

#labour_force_quar: quarterly labour market dataset obtained from International Labour Organization - ilostats: https://www.ilo.org/shinyapps/bulkexplorer15/
#Decide to work on the quarterly dataset to match with business indicator, using the input with similar format
#Risks: 2 datasets, labour_force_quar and economy, might have a certain different in term of data collection and terminology's deffinition 
#Assumption: the risks are acceptable
View(labour_force_quar)
#Load Quarterly Unemployment Rate data,naming the file labour_force_quar
labour_force_quar <- filter(labour_force_quar, sex.label == "Sex: Total" & classif1.label == "Age (Aggregate bands): Total") 
#Checking the unique variable in the dataset to select the appropriate data
unique(labour_force_quar$indicator.label)

#Part_rate_quar: Participant rate quarterly
#adjusted seasonally & contained data rated by all sex and age
Part_rate_quar <- labour_force_quar %>% 
  filter(indicator.label == "Labour force participation rate by sex and age, seasonally adjusted series (%)")
Part_rate_quar <- subset(Participation_rate, select = c(time,obs_value))

#Convert "time" variable from character to time-series format to plot it
Time.Part <-as.yearqtr(Part_rate_quar$time, forformat = "%YQ%q" )
Part.quar <- cbind(Time.Part, Part_rate_quar)  #create new dataset
Part.quar[,2] <- NULL #remove the outdated date format

##Narrow-down the dataset to Jan 2019-Nov2020 period to navigate the influences of Covid-19
Part_rate_quar.1920 <- tail(Part.quar,7) #the dataset contains observations recorded quarterly
head(Part_rate_quar.1920,1)
#Plot the dataset GDP from 1Q2019 to 3Q2020
Part_rate_quar.1920 %>% 
  as.data.frame() %>%
  ggplot(aes(x = Time.Part, y = obs_value/100))+ #value saved in percentage 
  scale_y_continuous(label = percent_format(accuracy = 1))+ #transforming to percentage format with unit only helps to improve the visualization quality
  scale_x_yearqtr(format = "%Y-%q", n = 7)+
  xlab("Time") + 
  ylab("Percentage (%)") +
  ggtitle("Australia: Participation Rate Quaterly 2019-20",subtitle = "Quarterly series from International Labour Organization (ilostats)")+
  geom_line()

#Un_rate_quar: Unemployment rate quarterly
#adjusted seasonally & contained data rated by all sex and age
Un_rate_quar <- filter(labour_force_quar, indicator.label == "Unemployment rate by sex and age, seasonally adjusted series (%)")
Un_rate_quar <- subset(Un_rate_quar, select = c(time,obs_value))
head(Un_rate_quar,1)
#Convert "time" variable from character to time-series format to plot it
Time.Un <-as.yearqtr(Un_rate_quar$time, forformat = "%YQ%q" )
Un.quar <- cbind(Time.Un, Un_rate_quar)  #create new dataset
Un.quar[,2] <- NULL #remove the outdated date format

##Narrow-down the dataset to Jan 2019-Nov2020 period to navigate the influences of Covid-19
Un.quar.1920 <- tail(Un.quar,7) #the dataset contains observations recorded quarterly
#Plot the dataset GDP from 1Q2019 to 3Q2020
Un.quar.1920 %>% 
  as.data.frame() %>%
  ggplot(aes(x = Time.Un, y = obs_value/100))+ #value saved in percentage 
  scale_y_continuous(label = percent_format(accuracy = 1))+ #transforming to percentage format with unit only helps to improve the visualization quality
  scale_x_yearqtr(format = "%Y-%q", n = 7)+
  xlab("Time") + 
  ylab("Percentage (%)") +
  ggtitle("Australia: Unemployment Rate Quaterly 2019-20",subtitle = "Quarterly series from International Labour Organization (ilostats)")+
  geom_line()

#Conduct statistical testing to prove the evaluate the relationship between 2 indicators
#Fill-in all available data for the statistical testing as the sample must contain as least 30 observations
head(Un.quar,1) #variables: Time.Un, obs_value
head(GDP.quar,1) #variables: Time.GDP, value

labour_un <- merge(GDP.quar,Un.quar, by.x = "Time.GDP", by.y = "Time.Un" )
#Range: 1Q1980 to 3Q2020
colnames(labour_un) <- c("Time","GDP_change","Un_change")

labour_un.fit <- lm(Un_change~GDP_change,data=labour_un)
summary(labour_un.fit)
#Prediction model - linear regression: 6.7972 + 0.0176*(GDP_Change)
#p-value: 0.861 ; 
#indicate that there is lacking of statistical evidence to prove the relationship between GDP Change and Unemployment rate
#demand to test another business indicators, identifying the influencing factors 

#Part 2. Trend evaluation
#Time-forecasting
#Plot the whole dataset - unemployment rate quarterly, analyzing the characteristics of the time-series data
Un_rate %>% 
  as.data.frame() %>%
  ggplot(aes(x = Time, y = Un_rate/100))+ #value saved in percentage 
  scale_y_continuous(label = percent_format(accuracy = 1))+ #transforming to percentage format with unit only helps to improve the visualization quality
  xlab("Time") + 
  ylab("Percentage (%)") +
  ggtitle("Australia: Unemployment Rate Monthly Feb 1978 - Nov 2020",subtitle = "Monthly series 5206.0 from the Australian Bureau of Statistics")+
  geom_line() +
  geom_hline(yintercept = mean(Un.quar$obs_value/100),linetype="dashed",color="darkred")+
  geom_hline(yintercept = median(Un.quar$obs_value/100),linetype="dashed",color="darkred")
head(Un_rate)

#Convert the data into time-series data
#Start: 1st February 1978;
#End: 1st November 2020;
#Frequency: 12, transforming the data set into monthly unemployment rate time-series dataset

#In general, there is not a clear trend observed from the dataset ; mostly, the data fluctuated dramatically.
#the first observation's value is 6.65, recording the unemployment rate in Feb 1978
#the last observation's value is 6.83, recording the unemployment rate in Nov 2020
mean_un_rate <- apply(Un_rate[,2], 1, mean)
median_un_rate <- apply(Un_rate[,2], 2, median)
#Although changing significantly throughout time, the unemployment rate tend to keep at certain range ; 
#mean (un_rate): 6.802318
#median (un_rate): 6.252306

#The dataset is cyclic
#Convert dataset into time-series data

Un_rate.ts <- ts(Un_rate$Un_rate ,start = c(1978,2),end=c(2020,11),frequency = 12) 
Acf(Un_rate.ts)

#the Augmented Dickey-Fuller test is conducted to confirm whether the dataset is stationary
#The null hypothesis for the test is that the dataset has a unit root, a indicator of not-stationary dataset
#H(0) = 1
#The alternative hypothesis is that the data are stationary: H(1) < 1
#To reject the null hypothesis: p-value <= 0.5, indicating that the dataset is stationary
#To not reject the null hypothesis: p-value >0.5, not have a strong evidence to conclude that that the dataset is stationary

A <- adf.test(Un_rate.ts,alternative = "stationary",k=1)
print(A)
#p-value = 0.7068 ; hence, cannot accept the null-hypothesis, or, in other words, the dataset is not-stationary

#Check whether there is the white noise by using acf function to conduct auto correlation testing


#Split the train & test dataset
#train dataset: Feb 1978 to June 2020
#test dataset: Jun 2020 to Nov 2020

train.ts <- window(Un_rate.ts, start=c(1978,1),end=c(2020,8),frequency=12)
test.ts <- window(Un_rate.ts, start=c(2020,9),end=c(2020,11),frequency=12)

#Create the Mean Absolute Percentage Error (MAPE) function, measuring the prediction accuracy of the forecasting method
mape <- function(actual,pred){
  mape <- mean(abs((actual-pred)/actual)*100)
  return(mape)
}


#As the dataset is not-stationary; we can first perform the Random Walk Forecast
#Random Walk Forecasts: current value of a variable is composed of a past value + an error term 
#Y(t) = Y(t-1) + error(t), 
#or, in other words, the best prediction for the next month's unemployment rate is the previous value
#do not take the pattern of the dataset into consideration

randomwalk <- rwf(train.ts,h=3) %>% 
  as.data.frame()
randomwalk
randomwalk <- randomwalk$`Point Forecast` %>% 
  as.data.frame() 

test.ts <- cbind(test.ts,randomwalk) %>% as.data.frame()
colnames(test.ts) <- c("Un_rate","RWF")

#Conduct Mean Absolute Percentage Error, measuring the size of the error of the prediction 
mape.rwf <- mape(test.ts$Un_rate,test.ts$RWF)
mape.rwf
#MAPE: 1.439693

##Simple Exponential Smoothing: extension of the naive
#Not have same pattern dataset
ses.ts <- ses(train.ts, h=3) %>% 
  as.data.frame()

#Point Forecast: 7.435448 - same for Jul 2020 to Nov 2020
##alpha value (0.9999) is close to 1 -> forecasts are closter to the most recent observations
ses.ts <- ses.ts$`Point Forecast` %>% 
  as.data.frame()

test.ts <- cbind(test.ts,ses.ts) 
colnames(test.ts) <- c("Un_rate","RWF","ses")

mape.ses <- mape(test.ts$Un_rate,test.ts$ses)
mape.ses
#MAPE: 1.438719 ; increasing the level of accuracy compare to naive model

#Holt's Trend Method: extension of the simple exponential smoothing

holt_model  <- holt(train.ts,h=3)
holt_model <- as.data.frame(holt_model)
summary(holt_model) 
#the model forecast the unemployment rate increase continuously by time with MAPE on training set equal to 2.162925
#evaluate the model performance
holt <- holt_model$`Point Forecast` %>%
  as.data.frame() 
test.ts <- cbind(test.ts,holt) 
colnames(test.ts) <- c("Un_rate","RWF","ses","holt")
holt
mape.holt <- mape(test.ts$Un_rate,test.ts$holt)
mape.holt
#MAPE: 4.224371 ; decreasing the level of accuracy compare to both Random Walk and Simple Exponential Smoothing models
#MAPE of train data < MAPE of test data, indicating the overfiting problem



#Arima model
#as the condition to perform Arima forecast is that the dataset has to be stationary
#transform the dataset into stationary to apply Arima model in the later stage
Un_ratediff1 <- diff(Un_rate$Un_rate,differences = 1)
adf.test(Un_ratediff1,alternative = "stationary",k=1)
#new p-value=0.01 <0.05, rejecting the null hypothesis and accepting the alternative hypotheses, which indicates that the dataset is stationary 
#convert the variable into time-series dataset
#as the Un_ratediff1 is the difference between value at t(t) and value at t(t-1), the dataset will start from March 1978 instead of February 1978
Un_rated_diff.ts <- ts(Un_ratediff1,start = c(1978,3),end=c(2020,11),frequency = 12) 

#Split the train & test dataset
#train dataset: Feb 1978 to June 2020
#test dataset: Jun 2020 to Nov 2020

train_diff.ts <- window(Un_rated_diff.ts, start=c(1978,3),end=c(2020,8),frequency=12)
test_diff.ts <- window(Un_rated_diff.ts, start=c(2020,9),end=c(2020,11),frequency=12)

#applying auto.arima function to create the Arima model
#auto.arima function: search for a range of p and q values after fixing d by Kwiatkowski-Phillips-Schmidt-Schin (KPSS) test, choosing the model having lowest AIC score

arima_model <- auto.arima(train_diff.ts)
fore_arima <- forecast::forecast(arima_model,h=3) %>%
  as.data.frame()

#Assign the obtained data into the arima.ts dataset
arima.ts <- fore_arima$`Point Forecast` %>%
  as.data.frame()
colnames(arima.ts) <- "arima"

#transform the data from different to unemployment rate
aug2020 <- tail(train.ts,1)
sep2020.pred <- aug2020+arima.ts[1,]
oct2020.pred <- sep2020.pred+arima.ts[2,]
nov2020.pred <- oct2020.pred+arima.ts[3,]

abc <- cbind(sep2020.pred,oct2020.pred,nov2020.pred) %>%
  as.data.frame()
arima.trans <- t(abc) %>% data.frame()
rownames(arima.trans) <- NULL
#Check MAPE
test.ts <- cbind(test.ts,arima.trans) %>% as.data.frame()
colnames(test.ts) <- c("Un_rate","RWF","ses","holt","arima.trains")
mape.arima <- mape(test.ts$Un_rate,test.ts$arima.trains)
mape.arima
#MAPE: 5.26583

#table the MAPE
mape.ts <- cbind(mape.rwf, mape.ses, mape.holt ,mape.arima)
mape.ts

#Plot the prediction for data in December 2020 to March 2021
ses.ts.Mar21 <- ses(Un_rate.ts, h=4)
holt_model.Mar21  <- holt(Un_rate.ts,h=4)
arima_model.Mar21 <- auto.arima(Un_rate.ts)
fore_arima.Mar21 <- forecast::forecast(arima_model.Mar21,h=4)
autoplot(window(Un_rate.ts,start=2019))+
  autolayer(ses.ts.Mar21,series = "Simple Exponential Smoothing",PI = FALSE)+
  autolayer(holt_model.Mar21,series = "Holt's Trend Method",PI = FALSE)+
  autolayer(fore_arima.Mar21,series = "Arima",PI = FALSE)+
  xlab("Time")+
  ylab("Percentage (%)")+
  ggtitle("Australia: Forecast Monthly Unemployment Rate December 2020 to March 2021", subtitle = "Time-Series Forecast")+
  guides(colour=guide_legend(title = "Forecast"))
#create table to store forecast points
pred.ses <- ses.ts.Mar21 %>% as.data.frame() 
pred.ses <- pred.ses$`Point Forecast`%>%
  as.data.frame()
pred.holt <- holt_model.Mar21 %>% as.data.frame()
pred.holt <- pred.holt$`Point Forecast`%>%
  as.data.frame()
pred.arima <- fore_arima.Mar21 %>% as.data.frame()
pred.arima <- pred.arima$`Point Forecast`%>%
  as.data.frame()
For_table <- cbind(pred.ses,pred.holt,pred.arima)
For_table

#Regression Analysis
#check the unique variables contained in the economy_ad dataset to transform, aiming to do regression analysis
#There are 36 variables that can categorize into 3 types of unit, including (1) chain volume estimate, (2) growth rates, and (3) index number
#Decide to use the growth rate and drop other types of units for avoiding duplicity 

economy_ad <- subset(economy, select = c(date,series,value,unit))
economy_ad <- filter(economy_ad, unit == "Percent")
unique(economy_ad$series) #the number of unique series dropped to 16 ; 1 variable, which is GDP change was created

#pre-processing to create dataframe contained 16 variables

#GDP_Per_Capita

GDP_per_capita <- filter(economy_ad, series == "GDP per capita: Chain volume measures - Percentage changes ;")
GDP_per_capita[,c(1,2,4)] <- list(NULL)
colnames(GDP_per_capita) <- "GDP_per_capita"

#Gross_value_added
Gross_value_added <- filter(economy_ad, series == "Gross value added market sector: Chain volume measures - Percentage changes ;")
Gross_value_added[,c(1,2,4)] <- list(NULL)
colnames(Gross_value_added) <- "Gross_value_added"

#NDP
NDP <- filter(economy_ad, series == "Net domestic product: Chain volume measures - Percentage changes ;")
NDP[,c(1,2,4)] <- list(NULL)
colnames(NDP) <- "NDP"

#GDI
GDI <- filter(economy_ad, series == "Real gross domestic income: Chain volume measures - Percentage changes ;")
GDI[,c(1,2,4)] <- list(NULL)
colnames(GDI) <- "GDI"

#GNI
GNI <- filter(economy_ad, series == "Real gross national income: Chain volume measures - Percentage changes ;")
GNI[,c(1,2,4)] <- list(NULL)
colnames(GNI) <- "GNI"

#RNNDI: Real_Net_national_disposable_income
RNNDI <- filter(economy_ad, series == "Real net national disposable income: Chain volume measures - Percentage changes ;")
RNNDI[,c(1,2,4)] <- list(NULL)
colnames(RNNDI) <- "RNNDI"

#RNNDIPE: Real_Net_national_disposable_income_per_capita
RNNDIPE <- filter(economy_ad, series == "Real net national disposable income per capita: Chain volume measures - Percentage changes ;")
RNNDIPE[,c(1,2,4)] <- list(NULL)
colnames(RNNDIPE) <- "RNNDIPE"

#GDP_Price: GDP current prices
GDP_Price <- filter(economy_ad, series == "Gross domestic product: Current prices - Percentage Changes ;")
GDP_Price[,c(1,2,4)] <- list(NULL)
colnames(GDP_Price) <- "GDP_Price"

#Hours_worked
Hours_worked <- filter(economy_ad, series == "Hours worked: Index - Percentage changes ;")
Hours_worked[,c(1,2,4)] <- list(NULL)
colnames(Hours_worked) <- "Hours_worked"

#Hours_worked_m: Hours worked market sector
Hours_worked_m <- filter(economy_ad, series == "Hours worked market sector: Index - Percentage changes ;")
Hours_worked_m[,c(1,2,4)] <- list(NULL)
colnames(Hours_worked_m) <- "Hours_worked_m"

#GDPPH:GDP per hour worked
GDPPH <- filter(economy_ad, series == "GDP per hour worked: Index - Percentage changes ;")
GDPPH[,c(1,2,4)] <- list(NULL)
colnames(GDPPH) <- "GDPPH"

#GVAPHM: Gross value added per hour worked market sector
GVAPHM <- filter(economy_ad,series == "Gross value added per hour worked market sector: Index - Percentage changes ;")
GVAPHM[,c(1,2,4)] <- list(NULL)
colnames(GVAPHM) <- "GVAPHM"

#GDP_Index
GDP_Index <- filter(economy_ad, series == "Gross domestic product: Index - Percentage changes ;")
GDP_Index[,c(1,2,4)] <- list(NULL)
colnames(GDP_Index) <- "GDP_Index"

#Domestic_demand
Domestic_demand <- filter(economy_ad, series == "Domestic final demand: Index - Percentage changes ;")
Domestic_demand[,c(1,2,4)] <- list(NULL)
colnames(Domestic_demand) <- "Domestic_demand"

#tot: terms of trade
tot <- filter(economy_ad, series == "Terms of trade: Index - Percentage changes ;")
tot[,c(1,2,4)] <- list(NULL)
colnames(tot) <- "tot"

#Merge all variables into the economy_ad1
economy_ad1 <- cbind(GDP, GDP_per_capita, Gross_value_added, NDP,GDI,GNI,RNNDI,RNNDIPE,GDP_Price,Hours_worked,Hours_worked_m,GDPPH,GVAPHM,GDP_Index,Domestic_demand)
#Omit missing data
economy_ad1 <- na.omit(economy_ad1)

#Checking the statistical significant relationship among Unemployment rate and other indicators
Time.econ <- as.yearqtr(economy_ad1$date,forformat = "%Y-%m-%d")
econ.fit <- cbind(Time.econ, economy_ad1)  #create new dataset
econ.fit[,2] <- NULL #remove the outdated date format

#join 2 datasets by using merge function 
market.fit <- merge(Un.quar,econ.fit,by.x = "Time.Un",by.y = "Time.econ")
colnames(market.fit) <- c("Time","Unrate","GDPchange","GDPpercap","Grossvalueadded","NDP","GDI","GNI","RNNDI","RNNDIPE","GDPprice","Hoursworked","hoursworkedm","GDPPH","GVAPHM","GDPindex","Domesticdemand")

#Business indicator
Biz_indi <- read_abs("5676.0", tables = 20) #table 20 contains Wages and salaries by state, current prices, percentage change from previous quarter
#There are 9 variables recorded in the dataset, including 8 variables corresponding to 8 states, which are New South Wales, Victoria, Queensland, South Australia, Western Australia, Tasmania, Northern Territory, Australian Capital Territory, and 1 variable - Total of all State
unique(Biz_indi$series)

#Use the Seasonally Adjusted variables
Biz_indi.state <- filter(Biz_indi, series_type == "Seasonally Adjusted") 
Biz_indi.state <- Biz_indi.state %>% subset(select = c(date,value,series))
Biz_indi.state %>% as.data.frame() %>%
  View()

#Wage.NSW: Quarterly estimates of wage in Victoria
Wage.NSW <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  New South Wales ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.NSW[,3] <- list(NULL)
colnames(Wage.NSW) <- c("Time","Wage.NSW")

#Wage.Victoria: Quarterly estimates of wage in Victoria
Wage.Victoria <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  Victoria ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.Victoria[,c(1,3)] <- list(NULL)
colnames(Wage.Victoria) <- "Wage.Victoria"

#Wage.Queensland: Quarterly estimates of wage in Queensland
Wage.Queensland <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  Queensland ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.Queensland[,c(1,3)] <- list(NULL)
colnames(Wage.Queensland) <- "Wage.Queensland"

#Wage.SAustralia: Quarterly estimates of wage in South Australia
Wage.SAustralia <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  South Australia ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.SAustralia[,c(1,3)] <- list(NULL)
colnames(Wage.SAustralia) <- "Wage.SAustralia"

#Wage.WAustralia: Quarterly estimates of wage in Western Australia
Wage.WAustralia <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  Western Australia ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.WAustralia[,c(1,3)] <- list(NULL)
colnames(Wage.WAustralia) <- "Wage.WAustralia"

#Wage.Tasmania: Quarterly estimates of wage in Tasmania
Wage.Tasmania <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  Tasmania ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.Tasmania[,c(1,3)] <- list(NULL)
colnames(Wage.Tasmania) <- "Wage.Tasmania"

#Wage.Norter: Quarterly estimates of wage in Northern Territory
Wage.Norter <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  Northern Territory ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.Norter[,c(1,3)] <- list(NULL)
colnames(Wage.Norter) <- "Wage.Norter"

#Wage.Ater: Quarterly estimates of wage in Australian Capital Territory,
Wage.Ater <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  Australian Capital Territory ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.Ater[,c(1,3)] <- list(NULL)
colnames(Wage.Ater) <- "Wage.Ater"

#Wage.total: Quarterly estimates of wage in Australian Capital Territory,
Wage.total <- filter(Biz_indi.state, series == "Wages quarterly percentage change ;  Total (State) ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE) ;")
Wage.total[,c(1,3)] <- list(NULL)
colnames(Wage.total) <- "Wage.total"


#Merge Wage.State dataset with market.fit
Wage.State <- cbind(Wage.NSW,Wage.Victoria,Wage.Queensland,Wage.SAustralia,Wage.WAustralia,Wage.Tasmania,Wage.Norter,Wage.Ater,Wage.total)

#Checking the statistical significant relationship among Unemployment rate and other indicators
Time.wage <- as.yearqtr(Wage.State$Time,forformat = "%Y-%m-%d")
Wage.State <- cbind(Time.wage, Wage.State)  #create new dataset
Wage.State[,2] <- NULL #remove the outdated date format
head(Wage.State,1)
#join 2 datasets by using merge function 

market.biz.fit <- merge(market.fit,Wage.State,by.x = "Time",by.y = "Time.wage")
cor.mar.fit <- subset(market.biz.fit, select = -c(Time))
cor.test <- rcorr(as.matrix(cor.mar.fit))

#Insignificant correlations are leaved blank
#Test not only correlation but also statistical significant relationship among variables ; variables that have insignificant statistical relationship will be feature blank
corrplot(cor.test$r, type="lower", order="hclust", p.mat = cor.test$P, sig.level = 0.05, insig = "blank")

#variables that have strong statistical relationship with unemployment rate (p-value < 0.05): GDPchange, GDP price,GDPindex, Domesticdemand, Wage.WAustralia
#GDPchange, GDP price,GDPindex are all about the aggregate demand ; 
#choose GDP change to fit the model 
#Fit model with GDPchange, Domesticdemand, and Wage.WAustralia 

#Split training & Test dataset
#After reduce missing data ; the length of dataset is reduced to 2Q2001 to 3Q2020

smp_size <- 0.8*nrow(cor.mar.fit) #Split the data by 80:20
index.n <- sample(nrow(cor.mar.fit),smp_size)
data.train.n = cor.mar.fit[index.n,]
data.test.n = cor.mar.fit[-index.n,]

#GDPchange+Domesticdemand+Wage.total+Wage.WAustralia
#Build predictive model

fit.market <- lm(Unrate~.,data=data.train.n)
summary(fit.market)
#R-squared when using all variables is there is 0.8442;

fit.market <- lm(Unrate~GDPchange+Domesticdemand+Wage.total+Wage.WAustralia,data=data.train.n)
summary(fit.market)
#Surprisingly, GDPchange & Wage.total appear as not have strong statistical relationship with unemployment rate (p-value = 0.846487 & 0.075910, coresspondingnly)
#possible explanation:
#GDPchange: Australia's Government expenses (stimulus package) play a role to aid the labour market during pandemic; therefore, the level of expense should also take into consideration
#Wage.total: it might be affected by "Wage.WAustralia" - multicollinarity problem

#R-squared (0.3797) is extremely low; 
#To accept the prediction model, there is a requirement to improve R-squared to at least > 0.5
#Note: R-squared should be fluctuated regarding to the train & test datasets, which were generated randomly
#R-squared value < 0.3 this value is generally considered a None or Very weak effect size,
#R-squared value 0.3 < r < 0.5 this value is generally considered a weak or low effect size,
#R-squared value 0.5 <= r < 0.7 this value is generally considered a Moderate effect size,
#R-squared value r > 0.7 this value is generally considered strong effect size
#Reference: Moore, D. S., Notz, W. I, & Flinger, M. A. (2013). The basic practice of statistics (6th ed.). New York, NY: W. H. Freeman and Company. Page (138)

#Keep current variables & plug-in other variables to improve R-squared
fit.market <- lm(Unrate~GDPchange+Domesticdemand+Wage.total+Wage.WAustralia+Wage.Queensland+Wage.NSW+Wage.Victoria+Wage.Tasmania+Wage.Ater+Wage.Norter+Wage.SAustralia+GVAPHM+GDPPH+Grossvalueadded+NDP+RNNDI+hoursworkedm+GDI+GNI,data=data.train.n)
#R-squared improves to 0.5472 after adding above variables
#Diagnose the multicollinearity essentials by using vif function from car package
vif(fit.market)
#Variable with a VIF value (above 5 or 10) should be removed from the model (James,2014)
#Reference:
#James, Gareth, Daniela Witten, Trevor Hastie, and Robert Tibshirani. 2014. An Introduction to Statistical Learning: With Applications in R. Springer Publishing Company, Incorporated.

fit.market <- lm(Unrate~
                   GDPPH+
                   Domesticdemand+
                   Wage.total+
                  
                   Wage.Queensland+
                   Wage.Victoria+
                   Wage.WAustralia+
                   Wage.SAustralia+
                   Wage.Tasmania+
                   Wage.Norter+
                   Wage.Ater+
                   NDP+
                   GVAPHM+
                   RNNDI,
                 data=data.train.n)

summary(fit.market)
vif(fit.market)
#the best R-squared can get after testing available variables on current model: 0.4922 ;
#VIF of all are lower than 5 except Wage.Total - 8.245884
#R-squared can be improved  when keep adding new variables
#For instace, R-squared reaches 0.5107 when adding Wage.NSW, however, Wage.Total goes to 26.297340
#Adjusted R-squared was still pretty low (0.3546) ; adjusted R-squared could be a better indicator to look at. It'll be increase when useful variables are added to the model
#Besides, only Domesticdemand variable illustrates strong statistical relationship (p-value < 0.05)
#All variables that did not provide meaningful interpretation if their p-vale lower than 0.05
#Moreover, all added variables related to State's salary are duplicated with Wage.total variable
#Wage.total plays crucial roles to the model
#Hence, decide to adjust standard R-squared to from expected R-squared >= 0.5 to 0.3<expected R-squared<0.5 ; aiming to get p-value <0.5 & VIF < 10
#Domesticdemand & Wage.total would be choosen

fit.market <- lm(Unrate~Domesticdemand+Wage.total, data=data.train.n)
summary(fit.market) 
#R-squared: 0.3794 & Adjusted R-squared: 0.3584
vif(fit.market) #vif of both variables : 1.103631
#Test the accuracy of the model by looking at Mean Square Error (MSE)

pred.new <- predict(fit.market,newdata=data.test.n) #feed the input from fit model on data.train dataset
y_test.new <- data.test.n[,"Unrate"]
pred_err.new <- sqrt(mean((pred.new - y_test.new)^2))
pred_err.new #RMSE: 0.7092026

#Formula: Quarterly Unemployment rate (%) = 6.16691+ -0.92121 *(%change of domestic demand) + -0.17724 * (%change of wage total)
#Standard deviation of residuals: 0.7092026

#For model improvement, real-time data collected is required to generate more meaningful insights
#The decision to use 2 quarterly datasets from difference resources is made due to the shortage of data
#Some indicators were not recorded fully ; methods to treat missing data such as adding mean/median of those variables into those NAs places could be conducted
#However, there is also a risk that those added values could not reflect the characteristics of missing value ; the context, when data is available - 2000s towards, is different with 1980s & 1990s

#Apply prediction model to calculate the unemployment rate 3Q2020 & 1Q2020
#Wages quarterly percentage change ;  Total (State) ;  Total (Industry) ;  Current Price ;  TOTAL (SCP_SCOPE): 2.4
#Domestic final demand: Index - Percentage changes ; 3Q2020: -0.1

#Normal case: total wage & domestic domestic demand growth with the same pace within the previous quarter 
#Good case: total wage will be increased 4.15 (current rate - 2.4 + new policy suggested to increase 1.75% national minimum wage) & domestic demand will not be decreased - %change = 0
#Worse case: total wage will be increased 1.75 (not growth but was pushed by the government policy) & domestic demand will not growth

#Normal case
domesticdemand_3Q2020 <- tail(Domestic_demand,1)
Wage.total_3Q2020 <- tail(Wage.total,1)

pred.unrate.4Q2020 <- 6.16691 + -0.92121 *domesticdemand_3Q2020 + -0.17724*(Wage.total_3Q2020)
pred.unrate.4Q2020.high <- pred.unrate.3Q2020+pred_err.new
pred.unrate.4Q2020.low <- pred.unrate.3Q2020-pred_err.new
unrate.pred.normal <- cbind(pred.unrate.4Q2020,pred.unrate.4Q2020.high,pred.unrate.4Q2020.low)
colnames(unrate.pred.normal) <- c("normal","high","low")

#worst case

worst.pred.unrate.4Q2020 <- 6.16691 + -0.92121 *0 + -0.17724*1.75
worst.pred.unrate.4Q2020.high <- worst.pred.unrate.4Q2020+pred_err.new
worst.pred.unrate.4Q2020.low <- worst.pred.unrate.4Q2020-pred_err.new
unrate.pred.worst <- cbind(worst.pred.unrate.4Q2020,worst.pred.unrate.4Q2020.high,worst.pred.unrate.4Q2020.low)
colnames(unrate.pred.worst) <- c("normal","high","low")

#good case

good.pred.unrate.4Q2020 <- 6.16691 + -0.92121 *0 + -0.17724*4.15
good.pred.unrate.4Q2020.high <- good.pred.unrate.4Q2020+pred_err.new
good.pred.unrate.4Q2020.low <- good.pred.unrate.4Q2020-pred_err.new
unrate.pred.good <- cbind(good.pred.unrate.4Q2020,good.pred.unrate.4Q2020.high,good.pred.unrate.4Q2020.low)
colnames(unrate.pred.good) <- c("normal","high","low")

#table
pred.table <- rbind(unrate.pred.good,unrate.pred.normal,unrate.pred.worst)
rownames(pred.table) <- c("good_case","normal_case","worse_case")
pred.table

#-------------------------------------------------#-------------------------------------------------#