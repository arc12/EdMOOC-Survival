## ****************************************************
## Created: Adam Cooper, Cetis, Dec 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Basic survival analysis based on last access date
## ****************************************************

## Chunks intended for use with knitter in the Rmd file.
## Can be used independently of knitr but NB that some config parameters are set in the Rmd

## @knitr INIT
library("survival")
source("./dbConnect.R")
source("./helpers.R")
source("./parametricHelpers")

##
## a couple of procedures to decorate the current plot
##
drawWeekLines<-function(end.Index, duration, drawStart=FALSE){
   #these are relative to start course=day0
   if(drawStart){      
      abline(v=0, col="blue" )
   }
   #and weekly intervals from course start
   abline(v=7*seq(1,duration+end.window), col="blue", lty=3 )
   # show official course end date
   abline(v=end.Index-course.open.dIndex, col="red")
}
#argument is a vector of days relative to the x origin - i.e. course.open.
# Names are used as labels. Use single character
drawAnnotationLines<-function(annotations){
   if(length(annotations)>0){
      for(i in 1:length(annotations)){
         x<-annotations[i]
         abline(v=x, col="green")
         points(x,0,pch=19, col="green", cex=3)
         text(x,0,names(x))
      }
   }
}

dayFactor<-86400 #seconds in 1 day
weekFactor<-7*dayFactor #seconds in 1 week
course.end.dates<-course.open.date+course.durations*weekFactor
cut.dates<-course.end.dates+end.window*weekFactor
#NB SQL returns truncated date values - see below - for use as date indexes
cut.dIndices<-cut.dates/dayFactor
course.open.dIndex<-course.open.date/dayFactor
course.end.dIndices<-course.end.dates/dayFactor
names(course.end.dIndices)<-courseIDs

#last_access_time is a UNIX datetime number
#rounds to nearest day. Multiply by 86400 to get a number to convert to a unix date
sql<-"SELECT u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from **gen.users u
 JOIN **gen.course_grades cg ON u.anon_user_id = cg.anon_user_id
 where access_group_id = 4"
# there are actually a number of people whose last access precedes the course start but there is a clear discontinuity at course start so use this as the start of the data. The pre-start attrition should be investigated separately, if required
sql.censored<-paste(sql, "AND cg.achievement_level = 'none' AND last_access_time >= ##")
sql.uncensored<-paste(sql, "AND cg.achievement_level = 'none' AND last_access_time >= ", course.open.date, "AND last_access_time < ##")
#count learners whose achievement was not none. These are excluded from the survival analysis
sql.achieved<-"SELECT  u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from **gen.users u
                  JOIN **gen.course_grades cg ON u.anon_user_id = cg.anon_user_id
                     where access_group_id = 4 AND cg.achievement_level != 'none'"

#this simply gets the full last-access data, which will not be used for "proper" analysis
# but which puts our focus-period into context
sql.unrestricted<-paste(sql, "AND last_access_time > ",  course.open.date-28*dayFactor)

## ........ get the data
db<-conn()
censoredList<- list.limit.SELECT(db, courseIDs, sql.censored, cut.dates, echo=T, schemaPrefix="vpodata_")
uncensoredList<- list.limit.SELECT(db, courseIDs, sql.uncensored, cut.dates, echo=T, schemaPrefix="vpodata_")
achievedList<-list.SELECT(db,courseIDs,sql.achieved,echo=T,schemaPrefix="vpodata_")
unrestrictedList<- list.SELECT(db, courseIDs, sql.unrestricted,  echo=T, schemaPrefix="vpodata_")
dbDisconnect(db)

##
## @knitr KM_UNRESTRICTED
##
# compute survival estimeates for the unrestricted/uncensored data. includes SoA people
unrestricted.KM.List<-list()
for(i in 1:length(courseIDs)){
   courseID <- courseIDs[i]
   unrestricted<-unrestrictedList[[courseID]]
   my.times <-unrestricted$date_index-course.open.dIndex #"death" times are >=1 (days)
   my.events <-rep(1, length(unrestricted$date_index))
   my.surv <- Surv(my.times,my.events)
   #KM fit
   my.KMest <- survfit(my.surv~1, conf.int=0.95)#type="kaplan-meier" is default
   unrestricted.KM.List[[courseID]]<-my.KMest
}
#use this for plotting per-course results of the above treatment
plot.unrestricted<-function(courseID){
   KMest<-unrestricted.KM.List[[courseID]]
   max.d<-max(KMest$time)
   plot(KMest, main=courseNames[courseID], sub=paste("max=",max.d), xlab="t/days", ylab="Proportion Surviving to time=t, S(t)")
   drawWeekLines(course.end.dIndices[courseID], max.d, TRUE)
   legend(x=max.d-2, y=1.0,  xjust=1, bg="white", legend=c("course start","course end"), fill=c("blue","red"), cex=0.7)
}

##
## @knitr KM_ACHIEVED
##
# compute achiievement-time estimeates for those with achievement != "none"
achieved.KM.List<-list()
for(i in 1:length(courseIDs)){
   courseID <- courseIDs[i]
   achieved<-achievedList[[courseID]]
   my.times <-achieved$date_index-course.open.dIndex #"death" times are >=1 (days)
   my.events <-rep(1, length(achieved$date_index))
   my.surv <- Surv(my.times,my.events)
   #KM fit
   my.KMest <- survfit(my.surv~1, conf.int=0.95)#type="kaplan-meier" is default
   achieved.KM.List[[courseID]]<-my.KMest
}
#use this for plotting per-course results of the above treatment
plot.achieved<-function(courseID){
   KMest<-achieved.KM.List[[courseID]]
   max.d<-max(KMest$time)
   plot(KMest, main=courseNames[courseID], sub=paste("max=",max.d), xlab="t/days", ylab="Proportion Not Yet Achieved by Time=t, S(t)")
   drawWeekLines(course.end.dIndices[courseID], max.d, TRUE)
   legend(x=max.d-2, y=1.0,  xjust=1, bg="white", legend=c("course start","course end"), fill=c("blue","red"), cex=0.7)
}

##
## @knitr PROPORTIONS
##
## plot proportions in/out of the survival analysis
props<-cbind(achievers = sapply(achievedList, function(x){length(x[,1])}),
             "non-survivors"=sapply(uncensoredList, function(x){length(x[,1])}),
             censored=sapply(censoredList, function(x){length(x[,1])}))
# add those who were lost prior to start date.
props<-cbind(props,"pre-start loss"= sapply(unrestrictedList, function(x){length(x[,1])})-rowSums(props))
#
props<-props[,c(1,4,3,2)]#re-order
cols<-rainbow(4)
barplot(t(100*props/rowSums(props)), main="Proportions of Learners", col=cols, las=2)
#mtext("(with access after start date)")
legend(x=4.2,y=95,legend=rev(colnames(props)),fill=rev(cols), cex=0.7, bg="white")

##
## @knitr KM_RESTRICTED
##
## compute K-M survival estimates with chosen start and finish dates (i.e. with R-censoring)

KMestList<-list()

for(i in 1:length(courseIDs)){
   
   courseID <- courseIDs[i]
   cut.dIndex <- cut.dIndices[i]
   course.end.dIndex<-course.end.dIndices[i]
   duration<-course.durations[i]
   
   censored<-censoredList[[courseID]]
   uncensored<-uncensoredList[[courseID]]
   
   my.times <-c(uncensored$date_index, rep(cut.dIndex, length(censored$date_index)))-course.open.dIndex+1 #"death" times are >=1 (days)
   my.events <-c(rep(1, length(uncensored$date_index)), rep(0, length(censored$date_index)))
   my.surv <- Surv(my.times,my.events)
   
   #KM fit
   my.KMest <- survfit(my.surv~1, conf.int=0.95)#type="kaplan-meier" is default
   KMestList[[courseID]]<-my.KMest
   plot(my.KMest, main=paste("Kaplan-Meier",courseID, sep=" - "), xlim=c(min(my.times), max(my.times)), xlab="Days Since Course Start")
   drawWeekLines(course.end.dIndex, duration)
   drawAnnotationLines(annotationList[[courseID]])
   # if this is linear then decay is exponential
   plot(my.KMest, main=paste("Cumulative Hazard Function (KM)",courseID, sep=" - "), sub="plot t vs -log(S)", xlim=c(min(my.times), max(my.times)), fun="cumhaz", xlab="Days Since Course Start")
   drawWeekLines(course.end.dIndex, duration)
   drawAnnotationLines(annotationList[[courseID]])
   
   #extract time and Survivor Function for manipulation
   fit.times<-my.KMest$time
   fit.survival<-my.KMest$surv
   
   # fit the parametric expoential decay using "survreg"
   expon<-survreg(formula = my.surv ~ 1, dist="exponential")
   print(summary(expon))
   
}

# last access by day of week
for(i in 1:length(courseIDs)){
   courseID <- courseIDs[i]
   uncensored<-uncensoredList[[courseID]]
   la.dow<-table(1+(uncensored$date_index-course.open.dIndex) %% 7)
   plot(la.dow, main=paste("last access by day of week", courseID,sep=" - "), sub="day 1 is the first day of each course week")
}





##
## some segmented fitting, ad hoc per course
##

# intro seems to have a change of shape at day 22
KMest<-KMestList[["intro"]]
KM.times<-KMest$time
KM.survival<-KMest$surv
split.1<-match(22, KM.times) #expected to return same index as matched value
#split.2<-match(56, KM.times)
split.times<-KM.times[1:split.1]
split.survival<-KM.survival[1:split.1]
plot(split.times, -log(split.survival), type="S", main="Test Exponential")
plot(log(split.times), log(-log(split.survival)), type="S", main="Intro days s1-22 Test Weibull")
plot(log(split.times), log(1/split.survival-1), type="S", main="Test log-logistic")

#I think a less negative Loglik indicates a better fit
#Weibull Survival function S(t)=exp(-\lambda t^\gamma)
expon<-survreg(formula = my.surv ~ 1, subset=(KMest$time<=22), dist="exponential")
weibull<-survreg(formula = my.surv ~ 1, subset=(KMest$time<=22), dist="weibull")
loglogistic<-survreg(formula = my.surv ~ 1, subset=(KMest$time<=22), dist="loglogistic")

#test statistic to compare alternative distributions to null hypothesis of exponential
D<- -2*expon$loglik[2] + 2*weibull$loglik[2]
#p.value - i.e. credibility of null hypothesis relative to alternative, given data
# = the probability of getting the observed given the null hypothesis
p.weibull<-1-pchisq(D, weibull$df-expon$df)

Dll<- -2*expon$loglik[2] + 2*loglogistic$loglik[2]
p.ll<-1-pchisq(Dll, loglogistic$df-expon$df)

save(file="Basic.RData", list=c("censoredList","uncensoredList", "KMestList", "unrestricted.KM.List"))

#    #transform and plot the survivor function to see if it looks like ...
#  plot(fit.times, -log(fit.survival), type="S", main="Test Exponential")
# #Weibull distribution. If a straight line then it is of Weibull type
# plot(log(fit.times), log(-log(fit.survival)), type="S", main="Test Weibull")
# # I'm confused by what the scale/shape parameters are in survreg, how differ tfrom rweibull and how relate to k, lambda in various accounts
# 
# 
# # alternatively log-logistic
# plot(log(fit.times), log(1/fit.survival-1), type="S", main="Test log-logistic")
# 
# 
# # fit the parametric distributions using "survreg"
# expon<-survreg(formula = my.surv ~ 1, dist="exponential")
# weibull<-survreg(formula = my.surv ~ 1, dist="weibull")
# loglogistic<-survreg(formula = my.surv ~ 1, dist="loglogistic")


## ***Made available using the The MIT License (MIT)***
#The MIT License (MIT)
#Copyright (c) 2013 Adam Cooper, University of Bolton
#
#Permission is hereby granted, free of charge, to any person obtaining a copy of
#this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#    
#    The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************