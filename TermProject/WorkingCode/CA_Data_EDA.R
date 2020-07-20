#CA Covid Data
library(tidyverse)
library(tswge)
library(ggthemes)

#import data - CA covid information was obtained from CA.gov database effective 7/15/2020
#*****delete extra paths after final data put on Github*****
CA <- read.csv("E:\\NEW.2\\Data Science\\Time Series\\Project\\CA_COVID_7.16.20.csv")
CA <- read.csv("E:\\Data Science\\Data Science\\Time Series\\Project\\CA_COVID_7.16.20.csv")
head(CA)

CA <- read.csv("G:\\NEW.2\\Data Science\\Time Series\\Project\\CA_COVID_7.16.20.csv")


#Format Date
CA$date <- as.Date(CA$date, format = "%m/%d/%Y")


## Plots of COVID quantitative measures over time

#Pos percentage for each day vs 14 day rolling average positivity rate. *This is what CA is using in their official reports
ggplot(data=CA) + 
  geom_line(data=CA,aes(x=date, y=pospercent_14dayavg), color="blue") + 
  geom_line(data=CA,aes(x=date, y=newpospercent), color="green3") +
  ggtitle("Daily Positivity Rate (green) and 14 Day Avg Pos Rate (blue)") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Percent")+ theme_hc()

#14 day rolling average obviously smooths the data, could forecast with assumption of stationarity with average from april onward 

#14 day rolling average positivity rate only
ggplot(data=CA, aes(x=date, y=pospercent_14dayavg, group=1)) + 
  geom_line(color="steelblue") + ggtitle("14 Day Average Positivity Rate") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Percent")+ theme_hc()

#New Cases
ggplot(data=CA, aes(x=date, y=newcountconfirmed, group=1)) + 
  geom_line(color="gold") + ggtitle("New Confirmed COVID-19 Cases in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()

#New tested vs new cases
ggplot(data=CA) + 
  geom_line(data=CA,aes(x=date, y=newtested, group=1), color="blue3") + 
  geom_line(data=CA,aes(x=date, y=newcountconfirmed, group=1), color="gold") +
  ggtitle("") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")

#ICU patients w/ COVID - this is current "booking" or number of patients in the hospital on each day
ggplot(data=CA, aes(x=date, y=icu_covid_confirmed_patients, group=1)) + 
  geom_line(color="red2") + ggtitle("ICU Patients with COVID-19 in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()

#Hospitalizations of Patients with COVID-19 in CA - this is current "booking" or number of patients in the hospital on each day
ggplot(data=CA, aes(x=date, y=hospitalized_covid_confirmed_patients, group=1)) + 
  geom_line(color="orange") + ggtitle("Hospitalizations of Patients with COVID-19 in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()

#Hospitalized (blue) and ICU (red) Patients with COVID-19 in CA
ggplot(data=CA) + 
  geom_line(data=CA,aes(x=date, y=hospitalized_covid_confirmed_patients, group=1), color="blue3") + 
  geom_line(data=CA,aes(x=date, y=icu_covid_confirmed_patients, group=1), color="red3") +
  ggtitle("Hospitalized (blue) and ICU (red) Patients with COVID-19 in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()


#Daily Deaths
ggplot(data=CA, aes(x=date, y=newcountdeaths, group=1)) + 
  geom_line(color="darkred") + ggtitle("Daily COVID-19 Related Deaths in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()



#Plots - Cumulative Totals

#Cumulative total Deaths
ggplot(data=CA, aes(x=date, y=totalcountdeaths, group=1)) + 
  geom_line(color="darkred") + ggtitle("Cumulative Total COVID-19 Related Deaths in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()

#Cumulative total Cases
ggplot(data=CA, aes(x=date, y=totalcountconfirmed, group=1)) + 
  geom_line(color="gold") + ggtitle("Cumulative Total COVID-19 Confirmed Cases in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()

#Cumulative total Tests
ggplot(data=CA, aes(x=date, y=testedtotal, group=1)) + 
  geom_line(color="green2") + ggtitle("Cumulative Total COVID-19 Tests in CA") + 
  scale_x_date(date_labels = "%b") + xlab("") + ylab("Count")+ theme_hc()



#Parzen Window Spectral Density Plots - only daily data (non cumulative) will likely show notable frequencies
#Using non cumulative daily data might be better for 7 day forecast if there are legitimate cyclical differences through the week
parzen.wge(CA$pospercent_14dayavg)
parzen.wge(CA$newtested)
parzen.wge(CA$newcountconfirmed)
parzen.wge(CA$newcountdeaths)

#Cumulatives might be easier to forecast (long term) as they lack the comparatively large fluctuations present in daily data?
#Running totals - these lack any notable frequency, which support probable easier forecasting over longer time period
parzen.wge(CA$testedtotal)
parzen.wge(CA$totalcountconfirmed)
parzen.wge(CA$totalcountdeaths)




#Model estimation

#Stationary Model: Estimating parameters for positivity rate - 14 day avg; only using data from April 3 onward
pr14 <- CA$pospercent_14dayavg[17:119]

parzen.wge(pr14)

#Estimate p and q
aic5.wge(pr14)
aic5.wge(pr14, type = "bic") #AR1 lowest bic
#Estimate phi and theta
pr14est <- est.ar.wge(pr14, p=1, type = "burg")
pr14est
#one week forecast of AR1 model
fore.arma.wge(pr14, phi = pr14est$phi, n.ahead = 7, lastn = FALSE, limits = FALSE)
#3 month Forecast of AR1 model
fore.arma.wge(pr14, phi = pr14est$phi, n.ahead = 90, lastn = FALSE, limits = FALSE)
#The 3 month stationary forecast does not represent a realistic representation of expectations; eventually the forecast must come down to near zero









###########################DNU
pr14est2 <- est.ar.wge(pr14, p=5, type = "burg")
fore.arma.wge(pr14, phi = pr14est2$phi, n.ahead = 90, lastn = FALSE, limits = FALSE)


#Difference the data once
pr14d1 <- artrans.wge(pr14, phi.tr = 1)
plotts.sample.wge(pr14d1)
aic5.wge(pr14d1, type = "bic")
pr14d1est <- est.ar.wge(pr14d1, p=5, type = "burg")
fore.aruma.wge(pr14, phi = pr14d1est$phi, d=1, n.ahead = 90, lastn = FALSE, limits = FALSE)

fore.aruma.wge(pr14, d=2, n.ahead = 90, lastn = FALSE, limits = FALSE)


#Differencing of cumulative totals series


