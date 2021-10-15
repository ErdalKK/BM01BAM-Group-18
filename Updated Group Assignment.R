library (ggplot2)
library (stargazer)
library (reshape2)
library (plyr)
library (tidyverse)
library(dplyr)

# install.packages("lubridate")
library(lubridate)

# install.packages("plm")
library(plm)

dir <- "~/Downloads/ASP/Group Assignment/"

dirData <- paste0(dir, "Data/")
dirProg <- paste0(dir, "Programs/")
dirRslt <- paste0(dir, "Figures/")

covid_data <- read.csv(file=paste0(dirData,"covid-data.csv"),
                         stringsAsFactors=TRUE)
closure_levels <- read.csv(file=paste0(dirData,"closure levels.csv"),
                        stringsAsFactors=TRUE)
### Cleaning Process ###
closure_levels$Day<-as.Date(closure_levels$Day, "%d/%m/%Y")

df.closure_levels<-closure_levels[closure_levels$Day >= "2020-03-10" & closure_levels$Day <= "2020-12-31",]

covid_data$date<-as.Date(covid_data$date, "%d/%m/%Y")

df.covid_data<-covid_data[covid_data$date >= "2020-03-10" & covid_data$date <= "2020-12-31",]

colnames(df.covid_data)[colnames(df.covid_data) == "iso_code"]             <- "Code"
colnames(df.closure_levels)[colnames(df.closure_levels) == "Day"]             <- "date"


df.covid<-merge(df.covid_data, df.closure_levels, by = c("Code", "date"))

# create subset for EU countries
df.covid <-subset(df.covid, df.covid$Code=="BEL"| df.covid$Code=="AUT"| df.covid$Code=="BGR" |df.covid$Code=="HRV"| df.covid$Code=="CYP"| df.covid$Code=="CZE"| df.covid$Code=="DNK"| df.covid$Code=="EST"| df.covid$Code=="FIN"| df.covid$Code=="FRA"| df.covid$Code=="DEU"| df.covid$Code=="GRC"| df.covid$Code=="HUN"| df.covid$Code=="IRL"| df.covid$Code=="ITA"| df.covid$Code=="LVA"| df.covid$Code=="LTU"| df.covid$Code=="LUX"| df.covid$Code=="MLT"| df.covid$Code=="NLD"| df.covid$Code=="POL"| df.covid$Code=="PRT"| df.covid$Code=="ROU"| df.covid$Code=="SVK"| df.covid$Code=="SVN"| df.covid$Code=="ESP"| df.covid$Code=="SWE")
rm(covid_data)

df.covid$Entity<- NULL
df.covid$continent<- NULL
df.covid$Code<- NULL

# We want to have the country first, then date
df.covid<-df.covid[,c(2,1,3,5,6,7,8,9)]


df.covid[, "new_tests"][is.na(df.covid[, "new_tests"])] <- 0

# check for complete cases -> Balanced dataset
df.covid <- df.covid[ complete.cases( df.covid ),]

# adjust for change in level measures 
df.covid$workplace_closures[which(df.covid$workplace_closures == 3)]<-2 

df.covid$stay_home_requirements[which(df.covid$stay_home_requirements == 3)]<-2 

df.covid$restriction_gatherings[which(df.covid$restriction_gatherings == 3)]<-2 
df.covid$restriction_gatherings[which(df.covid$restriction_gatherings == 4)]<-3

df.covid$international_travel_controls[which(df.covid$international_travel_controls == 2)]<-1
df.covid$international_travel_controls[which(df.covid$international_travel_controls == 3)]<-2 
df.covid$international_travel_controls[which(df.covid$international_travel_controls == 4)]<-3


# convert measures as factors

df.covid$workplace_closures<-as.factor(as.numeric(df.covid$workplace_closures))
df.covid$stay_home_requirements<-as.factor(as.numeric(df.covid$stay_home_requirements))
df.covid$restriction_gatherings<-as.factor(as.numeric(df.covid$restriction_gatherings))
df.covid$international_travel_controls<-as.factor(as.numeric(df.covid$international_travel_controls))

rm(closure_levels)
rm(covid_data)
rm(df.covid_data)
rm(df.closure_levels)

# summary stat
stargazer(df.covid, type="text")

# Need to lead variables by 7 and 14 days
install.packages("dplyr")   
library("dplyr")  

df.covid <- df.covid %>%                           
  group_by(location) %>%
  dplyr::mutate(new_cases_lead_7_days = dplyr::lead(new_cases, n = 7, default = NA)) %>% 
  as.data.frame()
df.covid <- df.covid %>%                           
  group_by(location) %>%
  dplyr::mutate(new_cases_lead_14_days = dplyr::lead(new_cases, n = 14, default = NA)) %>% 
  as.data.frame()
df.covid <- df.covid %>%                           
  group_by(location) %>%
  dplyr::mutate(new_tests_lead_7_days = dplyr::lead(new_tests, n = 7, default = NA)) %>% 
  as.data.frame()
df.covid <- df.covid %>%                           
  group_by(location) %>%
  dplyr::mutate(new_tests_lead_14_days = dplyr::lead(new_tests, n = 14, default = NA)) %>% 
  as.data.frame()
df.covid

#Create a subset for 7 days and 14 days
df.covid.7.lags <- df.covid[c(-10, -12)]
df.covid.14.lags <- df.covid[c(-9, -11)]

#Having complete cases for both 7 days and 14 days subsets
df.covid.7.lags <- df.covid.7.lags[ complete.cases( df.covid.7.lags),]
df.covid.14.lags <- df.covid.14.lags[ complete.cases( df.covid.14.lags),]

#View summary statistics results for 7 and 14 days
stargazer(df.covid.7.lags, type="text")
stargazer(df.covid.14.lags, type="text")

#### Run pooled model#####
mdlA.1 <- new_cases_lead_7_days ~   workplace_closures + restriction_gatherings + stay_home_requirements + international_travel_controls 
mdlA.2 <- new_cases_lead_7_days ~   workplace_closures + restriction_gatherings + stay_home_requirements + international_travel_controls + new_tests_lead_7_days
mdlB.1 <- new_cases_lead_14_days ~   workplace_closures + restriction_gatherings + stay_home_requirements + international_travel_controls 
mdlB.2 <- new_cases_lead_14_days ~   workplace_closures + restriction_gatherings + stay_home_requirements + international_travel_controls + new_tests_lead_14_days


#------------------
# Estimation of the pooled model
#------------------
rsltPoolA.1 <- lm (mdlA.1, data=df.covid.7.lags)

rsltPoolA.2 <- lm (mdlA.2, data=df.covid.7.lags)

rsltPoolB.1 <- lm (mdlB.1, data=df.covid.14.lags)

rsltPoolB.2 <- lm (mdlB.2, data=df.covid.14.lags)

stargazer(rsltPoolA.1, rsltPoolA.2, rsltPoolB.1, rsltPoolB.2,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type = "text")

#Fixed-effects model
rsltFE.A.1 <- 
  plm(mdlA.1, data = df.covid.7.lags, 
      index=c("location", "date"), model = "within")

rsltFE.A.2 <- 
  plm(mdlA.2, data = df.covid.7.lags, 
      index=c("location", "date"), model = "within")

rsltFE.B.1 <- 
  plm(mdlB.1, data = df.covid.14.lags, 
      index=c("location", "date"), model = "within")

rsltFE.B.2 <- 
  plm(mdlB.2, data = df.covid.14.lags, 
      index=c("location", "date"), model = "within")

stargazer(rsltFE.A.1, rsltFE.A.2, rsltFE.B.1, rsltFE.B.2, 
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

#F-test to evaulate if fixed-effects model or pooled regression model is suitable
pFtest(rsltFE.A.2, rsltPoolA.2)
pFtest(rsltFE.B.2, rsltPoolB.2)

#Random-effects model
rsltRE.A.1 <- 
  plm(mdlA.1, data = df.covid.7.lags, 
      index=c("location", "date"), model = "random")

rsltRE.A.2 <- 
  plm(mdlA.2, data = df.covid.7.lags, 
      index=c("location", "date"), model = "random")

rsltRE.B.1 <- 
  plm(mdlB.1, data = df.covid.14.lags, 
      index=c("location", "date"), model = "random")

rsltRE.B.2 <- 
  plm(mdlB.2, data = df.covid.14.lags, 
      index=c("location", "date"), model = "random")

stargazer(rsltRE.A.1, rsltRE.A.2, rsltRE.B.1, rsltRE.B.2,
          align=TRUE, no.space=TRUE, intercept.bottom = FALSE, type="text")

#Hausman test to determine if fixed-effects model or random-effects model is suitable
phtest(rsltFE.A.2, rsltRE.A.2)
phtest(rsltFE.B.2, rsltRE.B.2)
