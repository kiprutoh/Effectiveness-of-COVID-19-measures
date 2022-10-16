library(CausalImpact)
library(quantmod)
librayry(lubridate)
library(ggplot2)

casP=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/Algeria.txt", header=TRUE,sep="\t")
casP$Date=as.Date(casP$Date)
head(casP)
attach(casP)



CasesP=zoo(cbind(New_cases,New_deaths),Date)
head(CasesP)

pre.period=as.Date(c("2020-2-25","2020-3-23")) #lockdowns
post.period=as.Date(c("2020-3-24", "2020-5-14"))

##determining the impact of the lockdown
impactL=CausalImpact(CasesP,pre.period, post.period)
plot(impactL)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL)
summary(impactL, "report")

##opening lockdowns

pre.periodA=as.Date(c("2020-3-23","2020-5-14")) #lockdowns
post.periodA=as.Date(c("2020-5-15", "2020-6-25"))

##determining the impact of the opening the lockdown up to the end of the year
impactLA=CausalImpact(CasesP,pre.periodA, post.periodA)
plot(impactLA)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactLA)
summary(impactLA, "report")



###closing schools
pre.period1A=as.Date(c("2020-2-25","2020-3-23")) #schools (partial reopening)
post.period1A=as.Date(c("2020-3-24", "2020-10-21"))
impactL1A=CausalImpact(CasesP,pre.period1A, post.period1A)
plot(impactL1A)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL1A)
summary(impactL1A, "report")

####opening schools
pre.period1=as.Date(c("2020-3-23","2020-10-21")) #schools (partial reopening)
post.period1=as.Date(c("2020-10-22", "2021-05-21"))
impactL1=CausalImpact(CasesP,pre.period1, post.period1)
plot(impactL1)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL1)
summary(impactL1, "report")



###closing airports
pre.period2=as.Date(c("2020-2-25","2020-5-21")) #airports closure
post.period2=as.Date(c("2020-5-22", "2020-6-12"))
impactL2=CausalImpact(CasesP,pre.period2, post.period2)
plot(impactL2)+
scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL2)
summary(impactL2, "report")


###opening airports
pre.period2=as.Date(c("2020-5-21","2020-6-12")) #airports closure
post.period2=as.Date(c("2020-6-13", "2020-7-03"))
impactL2=CausalImpact(CasesP,pre.period2, post.period2)
plot(impactL2)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL2)
summary(impactL2, "report")




#####Angola


casP=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/Angola.txt", header=TRUE,sep="\t")
casP$Date=as.Date(casP$Date)
head(casP)
attach(casP)



CasesP=zoo(cbind(New_cases,New_deaths),Date)
head(CasesP)

pre.period=as.Date(c("2020-3-21","2020-5-27")) #lockdowns
post.period=as.Date(c("2020-5-28", "2020-9-1"))

##determining the impact of the lockdown
impactL=CausalImpact(CasesP,pre.period, post.period)
plot(impactL)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL)
summary(impactL, "report")

##opening lockdowns

pre.periodA=as.Date(c("2020-5-27","2020-9-1")) #lockdowns
post.periodA=as.Date(c("2020-9-2", "2020-12-4"))

##determining the impact of the opening the lockdown up to the end of the year
impactLA=CausalImpact(CasesP,pre.periodA, post.periodA)
plot(impactLA)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactLA)
summary(impactLA, "report")



###closing schools
pre.period1A=as.Date(c("2020-3-21","2020-5-27")) #schools (partial reopening)
post.period1A=as.Date(c("2020-5-28", "2020-10-5"))
impactL1A=CausalImpact(CasesP,pre.period1A, post.period1A)
plot(impactL1A)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL1A)
summary(impactL1A, "report")

####opening schools
pre.period1=as.Date(c("2020-5-27","2020-10-5")) #schools (partial reopening)
post.period1=as.Date(c("2020-10-6", "2021-02-8"))
impactL1=CausalImpact(CasesP,pre.period1, post.period1)
plot(impactL1)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL1)
summary(impactL1, "report")



###closing airports
pre.period2=as.Date(c("2020-3-15","2020-3-20")) #airports closure
post.period2=as.Date(c("2020-3-21", "2020-9-14"))
impactL2=CausalImpact(CasesP,pre.period2, post.period2)
plot(impactL2)+
scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL2)
summary(impactL2, "report")


###opening airports
pre.period2=as.Date(c("2020-3-20","2020-9-14")) #airports closure
post.period2=as.Date(c("2020-9-15", "2021-3-10"))
impactL2=CausalImpact(CasesP,pre.period2, post.period2)
plot(impactL2)+scale_x_date(date_breaks = "1 month",date_labels = "%b-\n%Y")
summary(impactL2)
summary(impactL2, "report")



