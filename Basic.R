library("survival")
source("./dbConnect.R")
source("./helpers.R")

course.open.date<-as.numeric(as.POSIXct("2013-01-28"))
courseIDs<-c("aiplan","astro","crit","edc","equine","intro")
#in weeks relative to course.open.date, same order as courseIDs
course.durations<-c(5,5,5,5,5,7)
#number of weeks after course duration before right-censoring
# i.e. people who access after this are right censored but people with last access before are "deaths"
end.window<-4

#these are used to label key dates. The names are used as the labels. Single char best
annotationList<-list(aiplan=c(A=29,B=36, E=44),
                     astro=c(A=29, B=51),
                     crit=c(A=29,B=43),
                     edc=c(A=22),
                     equine=c(A=8, B=22,C=58),
                     intro=c(A=22, B=61))


dayFactor<-86400 #seconds in 1 day
weekFactor<-7*dayFactor #seconds in 1 week
course.end.dates<-course.open.date+course.durations*weekFactor
cut.dates<-course.end.dates+end.window*weekFactor
#NB SQL returns truncated date values - see below - for use as date indexes
cut.dIndices<-cut.dates/dayFactor
course.open.dIndex<-course.open.date/dayFactor
course.end.dIndices<-course.end.dates/dayFactor

#last_access_time is a UNIX datetime number
#rounds to nearest day. Multiply by 86400 to get a number to convert to a unix date
sql<-"SELECT u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from **gen.users u
 JOIN **gen.course_grades cg ON u.anon_user_id = cg.anon_user_id
 where access_group_id = 4 AND cg.achievement_level = 'none'
AND"
# there are actually a number of people whose last access precedes the course start but there is a clear discontinuity at course start so use this as the start of the data. The pre-start attrition should be investigated separately, if required
sql.censored<-paste(sql, "last_access_time >= ##")
sql.uncensored<-paste(sql, "last_access_time >= ", course.open.date, "AND last_access_time < ##")

db<-conn()
censoredList<- list.limit.SELECT(db, courseIDs, sql.censored, cut.dates, echo=T, schemaPrefix="vpodata_")
uncensoredList<- list.limit.SELECT(db, courseIDs, sql.uncensored, cut.dates, echo=T, schemaPrefix="vpodata_")

dbDisconnect(db)

drawWeekLines<-function(end.Index, duration){
   #these are relative to start course=day0
   #and weekly intervals from course start
   abline(v=7*seq(1,duration+end.window), col="blue", lty=3 )
   # show official course end date
   abline(v=end.Index-course.open.dIndex, col="red")
}

#argument is a vector of days relative to the course.open.
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

# supply with the results of surveg(... dist="weibull") and it returns the parameters lambda and gamma for Weibull distribution survival fn: S(t)=exp(-\lambda t^\gamma)
weibull.pars<-function(weib.survreg){
   pars=list(lambda=as.numeric(exp(-weib.survreg$coefficient/weib.survreg$scale)),
             gamma=1/weib.survreg$scale)
   return(pars)
}

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
   
   # last access by day of week
   la.dow<-table(1+(uncensored$date_index-course.open.dIndex) %% 7)
   plot(la.dow, main=paste("last access by day of week", courseID,sep=" - "), sub="day 1 is the first day of each course week")
}


##
##
x<-seq(0,4, 0.01)
plot(x, exp(-x), type="l")
lines(x,exp(-x^1.2), col="red")
lines(x,exp(-x^0.8), col="green")
ye<-exp(-x)
yw12<-exp(-x^1.2)
yw08<-exp(-x^0.8)
plot(x, diff(ye), type="l")
plot(x[-1], diff(ye), type="l")
plot(x[-1], diff(yw12), type="l")
lines(x[-1], diff(yw08), type="l", col="green")
plot(x[-1], diff(ye), type="l")
lines(x[-1], diff(yw12), col="red")
lines(x[-1], diff(yw08), type="l", col="green")
plot(x[-1], diff(ye)/ye, type="l")
plot(x[-1], diff(ye)/ye[-1], type="l")
lines(x[-1], diff(yw08)/yw08, type="l", col="green")
lines(x[-1], diff(yw08)/yw08[-1], type="l", col="green")
plot(x[-1], diff(yw08)/yw08[-1], type="l", col="green")
plot(x[-1], diff(yw12)/yw12[-1], type="l", col="green")
plot(x[-1], diff(yw08)/yw08[-1], type="l", col="green")
plot(x[-1], diff(yw12)/yw12[-1], type="l", col="green")
##
##


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
expon<-survreg(formula = my.surv ~ 1, subset=(my.surv[,"time"]<=22), dist="exponential")
weibull<-survreg(formula = my.surv ~ 1, subset=(my.surv[,"time"]<=22), dist="weibull")
loglogistic<-survreg(formula = my.surv ~ 1, subset=(my.surv[,"time"]<=22), dist="loglogistic")

#test statistic to compare alternative distributions to null hypothesis of exponential
D<- -2*expon$loglik[2] + 2*weibull$loglik[2]
#p.value - i.e. credibility of null hypothesis relative to alternative, given data
# = the probability of getting the observed given the null hypothesis
p.weibull<-1-pchisq(D, weibull$df-expon$df)

Dll<- -2*expon$loglik[2] + 2*loglogistic$loglik[2]
p.ll<-1-pchisq(Dll, loglogistic$df-expon$df)

save(file="Basic.RData", list=c("censoredList","uncensoredList", "KMestList"))

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