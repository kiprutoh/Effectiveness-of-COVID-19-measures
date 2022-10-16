
###Loading the data

##Algeria
##lockdowns
AlgeriaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/AlgeriaLockdown.txt",header=T,sep='\t')
head(AlgeriaLD)
##schools
AlgeriaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/AlgeriaSchools.txt",header=T,sep='\t')
head(AlgeriaSCH)




###Angola
AngolaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/AngolaLockdown.txt",header=T,sep='\t')
head(AngolaLD)
##schools
AngolaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/AngolaSchools.txt",header=T,sep='\t')
head(AngolaSCH)


#####################################################

####analysis of the change points

######################################################
###lockdowns
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(AlgeriaLD[, 5])  # Fit all models at once
fit_envcpt$summary

plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(AlgeriaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(AlgeriaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric

library(changepoint.np)
changepoint.np::cpt.np(AlgeriaLD[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(AlgeriaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)


###################Schools
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(AlgeriaSCH[, 5])  # Fit all models at once
fit_envcpt$summary

plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(AlgeriaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(AlgeriaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric

library(changepoint.np)
changepoint.np::cpt.np(AlgeriaSCH[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(AlgeriaSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)




#####Angola

###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(AngolaLD[, 5])  # Fit all models at once
fit_envcpt$summary

plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(AngolaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(AngolaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))

plot(fit_changepoint)

###using changepoint non-parametric

library(changepoint.np)
changepoint.np::cpt.np(AngolaLD[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(AngolaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(AngolaSCH[, 5])  # Fit all models at once
fit_envcpt$summary

plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(AngolaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(AngolaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))

plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(AngolaSCH[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(AngolaSCH[, 5], d = 1000)
plot(fit_bcp)




########################################

####Benin

#####################################

##lockdowns
BeninLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/BeninLockdown.txt",header=T,sep='\t')
##schools
BeninSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/BeninSchools.txt",header=T,sep='\t')



###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(BeninLD[, 5])  # Fit all models at once
fit_envcpt$summary

plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(BeninLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(BeninLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))

plot(fit_changepoint)

###using changepoint non-parametric

library(changepoint.np)
changepoint.np::cpt.np(BeninLD[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(BeninLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(BeninSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(BeninSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(BeninSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))

plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(BeninSCH[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(BeninSCH[, 5], d = 1000)
plot(fit_bcp)




########################################

####Botswana

#####################################

##lockdowns
BotswanaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/BotswanaLockdown.txt",header=T,sep='\t')
##schools
BotswanaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/BotswanaSchools.txt",header=T,sep='\t')



###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(BotswanaLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(BotswanaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(BotswanaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))

plot(fit_changepoint)

###using changepoint non-parametric

library(changepoint.np)
changepoint.np::cpt.np(BotswanaLD[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(BotswanaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(BotswanaSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est



library(cpm)## another package
fit_cpm = processStream(BotswanaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints



library(changepoint)# a different approach
fit_changepoint = cpt.mean(BotswanaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))

plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(BotswanaSCH[, 5])@cpts


###Bayesian approach
library(bcp)
fit_bcp = bcp(BotswanaSCH[, 5], d = 1000)
plot(fit_bcp)




##################################################################
###Bukina Faso
BukinaFLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/BuknaFasoLockdown.txt",header=T,sep='\t')
head(BukinaFLD)
##schools
BukinaFSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/BuknaFasoSchools.txt",header=T,sep='\t')
head(BukinaFSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(BukinaFLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(BukinaFLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(BukinaFLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(BukinaFLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(BukinaFLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(BukinaFSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(BukinaFSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(BukinaFSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(BukinaFSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(BukinaFSCH[, 5], d = 1000)
plot(fit_bcp)








##################################################################
###Carbo Verde
CarboVLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CaboVerdeLockdown.txt",header=T,sep='\t')
head(CarboVLD)
##schools
CarboVSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CaboVerdeSchools.txt",header=T,sep='\t')
head(CarboVSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CarboVLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CarboVLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CarboVLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(CarboVLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CarboVLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CarboVSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CarboVSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CarboVSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(CarboVSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CarboVSCH[, 5], d = 1000)
plot(fit_bcp)









##################################################################
###Cameroon
CameroonLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CameroonLockdown.txt",header=T,sep='\t')
head(CameroonLD)
##schools
CameroonSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CameroonSchools.txt",header=T,sep='\t')
head(CameroonSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CameroonLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CameroonLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CameroonLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(CameroonLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CameroonLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CameroonSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CameroonSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CameroonSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(CameroonSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CameroonSCH[, 5], d = 1000)
plot(fit_bcp)







##################################################################
###CAR
CARLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CARLockdown.txt",header=T,sep='\t')
head(CARLD)
##schools
CARSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CARSchools.txt",header=T,sep='\t')
head(CARSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CARLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CARLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CARLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(CARLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CARLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CARSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CARSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CARSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(CARSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CARSCH[, 5], d = 1000)
plot(fit_bcp)




#######

###Chad

#####################schools 
##schools
ChadSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/ChadSchools.txt",header=T,sep='\t')
head(ChadSCH)


##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(ChadSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(ChadSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(ChadSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(ChadSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(ChadSCH[, 5], d = 1000)
plot(fit_bcp)




#######

###Comoros

#####################schools 
##schools
ComorosSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/ComorosSchools.txt",header=T,sep='\t')
head(ComorosSCH)


##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(ComorosSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(ComorosSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(ComorosSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(ComorosSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(ComorosSCH[, 5], d = 1000)
plot(fit_bcp)








##################################################################
###Congo
CongoLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CongoLockdown.txt",header=T,sep='\t')
head(CongoLD)
##schools
CongSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CongoSchools.txt",header=T,sep='\t')
head(CongSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CongoLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CongoLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CongoLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(CongoLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CongoLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CongSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CongSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CongSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(CongSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CongSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)








##################################################################
###Cote d Ivoire
CotedLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CotedILockdown.txt",header=T,sep='\t')
head(CotedLD)
##schools
CotedSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/CotedISchools.txt",header=T,sep='\t')
head(CotedSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CotedLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CotedLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CotedLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(CotedLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CotedLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(CotedSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(CotedSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(CotedSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(CotedSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(CotedSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)







##################################################################
###DRC
DRCLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/DRCLockdown.txt",header=T,sep='\t')
head(DRCLD)
##schools
DRCSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/DRCShools.txt",header=T,sep='\t')
head(DRCSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(DRCLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(DRCLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(DRCLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(DRCLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(DRCLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(DRCSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(DRCSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(DRCSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(DRCSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(DRCSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)






##################################################################
###Equatorial Guinea
EQGNLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/EqGuneaLockdown.txt",header=T,sep='\t')
head(EQGNLD)
##schools
EQGNSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/EqGuneaSchools.txt",header=T,sep='\t')
head(EQGNSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(EQGNLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(EQGNLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(EQGNLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(EQGNLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(EQGNLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(EQGNSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(EQGNSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(EQGNSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(EQGNSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(EQGNSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)






##################################################################
###Eritrea
EritLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/ErtLockdown.txt",header=T,sep='\t')
head(EritLD)
##schools
EritSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/ErtShools.txt",header=T,sep='\t')
head(EritSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(EritLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(EritLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(EritLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(EritLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(EritLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(EritSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(EritSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(EritSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(EritSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(EritSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





##################################################################
###Eswatini
EritLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/ErtLockdown.txt",header=T,sep='\t')
head(EritLD)
##schools
EswatSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/EswatiniSchools.txt",header=T,sep='\t')
head(EswatSCH)


###################schools
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(EswatSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(EswatSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(EswatSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(EswatSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(EswatSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)




##################################################################
###Ethiopia
EritLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/ErtLockdown.txt",header=T,sep='\t')
head(EritLD)
##schools
ETSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/EthiopiaSchools.txt",header=T,sep='\t')
head(ETSCH)


###################schools
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(ETSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(ETSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(ETSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(ETSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(ETSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)









##################################################################
###Gabon
GabonLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GabonLockdown.txt",header=T,sep='\t')
head(GabonLD)
##schools
GabonSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GabonSchools.txt",header=T,sep='\t')
head(GabonSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GabonLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GabonLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GabonLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(GabonLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GabonLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GabonSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GabonSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GabonSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(GabonSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GabonSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





##################################################################
###Gambia
GambiaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GambiaLockdown.txt",header=T,sep='\t')
head(GambiaLD)
##schools
GambiaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GambiaSchools.txt",header=T,sep='\t')
head(GambiaSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GambiaLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GambiaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GambiaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(GambiaLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GambiaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GambiaSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GambiaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GambiaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(GambiaSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GambiaSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)










##################################################################
###Ghana
GhanaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GhanaLockdown.txt",header=T,sep='\t')
head(GhanaLD)
##schools
GhanaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GhanaSchools.txt",header=T,sep='\t')
head(GhanaSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GhanaLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GhanaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GhanaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(GhanaLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GhanaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GhanaSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GhanaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GhanaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(GhanaSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GhanaSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





##################################################################
###Guinea-Bissau
GuineaBLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GuineaBLockdown.txt",header=T,sep='\t')
head(GuineaBLD)
##schools
GuineaBSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/GuineaBSchools.txt",header=T,sep='\t')
head(GuineaBSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GuineaBLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GuineaBLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GuineaBLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(GuineaBLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GuineaBLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(GuineaBSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(GuineaBSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(GuineaBSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(GuineaBSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(GuineaBSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)







##################################################################
###Kenya
KenyaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/KenyaLockdown.txt",header=T,sep='\t')
head(KenyaLD)
##schools
KenyaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/KenyaSchools.txt",header=T,sep='\t')
head(KenyaSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(KenyaLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(KenyaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(KenyaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(KenyaLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(KenyaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(KenyaSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(KenyaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(KenyaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(KenyaSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(KenyaSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)




##################################################################
###Lesotho
LesothoLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/LesothoLockdown.txt",header=T,sep='\t')
head(LesothoLD)
##schools
LesothoSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/LesothoSchools.txt",header=T,sep='\t')
head(LesothoSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(LesothoLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(LesothoLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(LesothoLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(LesothoLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(LesothoLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(LesothoSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(LesothoSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(LesothoSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(LesothoSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(LesothoSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)







##################################################################
###Liberia
LiberiaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/LiberiaLockdown.txt",header=T,sep='\t')
head(LiberiaLD)
##schools
LiberiaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/LiberiaSchools.txt",header=T,sep='\t')
head(LiberiaSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(LiberiaLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(LiberiaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(LiberiaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(LiberiaLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(LiberiaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(LiberiaSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(LiberiaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(LiberiaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(LiberiaSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(LiberiaSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





##################################################################
###Madagascar
MadagLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/madagLokdown.txt",header=T,sep='\t')
head(MadagLD)
##schools
MadagSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/madagchools.txt",header=T,sep='\t')
head(MadagSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MadagLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MadagLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MadagLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(MadagLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MadagLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MadagSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MadagSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MadagSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(MadagSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MadagSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





##################################################################
###Malawi
##schools
MalawiSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/malawiSchools.txt",header=T,sep='\t')
head(MalawiSCH)



#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MalawiSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MalawiSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MalawiSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(MalawiSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MalawiSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)






##################################################################
###Mali
MaliLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/MaliLockdown.txt",header=T,sep='\t')
head(MaliLD)
##schools
MaliSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/MaliSchools.txt",header=T,sep='\t')
head(MaliSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MaliLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MaliLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MaliLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(MaliLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MaliLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MaliSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MaliSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MaliSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(MaliSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MaliSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)





##################################################################
###Mauritania
MauritaLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/MauritaniaLockdown.txt",header=T,sep='\t')
head(MauritaLD)
##schools
MauritaSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/MauritaniaSchools.txt",header=T,sep='\t')
head(MauritaSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MauritaLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MauritaLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MauritaLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(MauritaLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MauritaLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MauritaSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MauritaSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MauritaSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(MauritaSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MauritaSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)




##################################################################
###Mauritius
MauritLD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/MauritiusLockdown.txt",header=T,sep='\t')
head(MauritLD)
##schools
MauritSCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/MauritiusSchools.txt",header=T,sep='\t')
head(MauritSCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MauritLD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MauritLD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MauritLD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(MauritLD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MauritLD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(MauritSCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(MauritSCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(MauritSCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(MauritSCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(MauritSCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



##################################################################
###South Africa
SALD=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/SALockdown.txt",header=T,sep='\t')
head(SALD)
##schools
SASCH=read.table("F:/Epi Modelling COVID-19/Data/Analytics/Janette/Contries/SASchools.txt",header=T,sep='\t')
head(SASCH)


###################lockdown
#######################
##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(SALD[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(SALD[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(SALD[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
library(changepoint.np)
changepoint.np::cpt.np(SALD[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(SALD[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)



#####################schools 

##different approaches
library(EnvCpt) #can detect change points in mean and variance (not separately), slopes (?trends?), and AR(1)/AR(2), as well as conveniently fitting various models without change points. It automatically infers the number of change points. 
fit_envcpt = envcpt(SASCH[, 5])  # Fit all models at once
fit_envcpt$summary
plot(fit_envcpt)
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

library(cpm)## another package
fit_cpm = processStream(SASCH[, 5], cpmType = "Student")  # Multiple change points
fit_cpm$changePoints

library(changepoint)# a different approach
fit_changepoint = cpt.mean(SASCH[, 5])
# Return estimates
c(ints = param.est(fit_changepoint)$mean,
  cp = cpts(fit_changepoint))
plot(fit_changepoint)

###using changepoint non-parametric
changepoint.np::cpt.np(SASCH[, 5])@cpts

###Bayesian approach
library(bcp)
fit_bcp = bcp(SASCH[, 5], d = 1000)
plot(fit_bcp)
summary(fit_bcp)


#####
citation(EnvCpt)

cm <- tryCatch(citation("bcp"),
               error = function(e) {
                 warning("Recommended package 'mgcv' is not installed properly")
                 stop(e$message) })
cm # short entries (2-3 lines each)
print(cm, bibtex = TRUE) # each showing its bibtex code



