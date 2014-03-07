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
source("./parametricHelpers.R")

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

all.regions<-c('EUROPE', 'NORTH AMERICA', 'SOUTH AMERICA', 'AFRICA',
               'ASIA', 'MIDDLE EAST', 'CENTRAL AMERICA', 'UK AND IRELAND',
               'CHINA', 'AUSTRALIA AND NZ', 'INDIA')

dayFactor<-86400 #seconds in 1 day
weekFactor<-7*dayFactor #seconds in 1 week
course.end.dates<-course.open.date+course.durations*weekFactor
cut.dates<-course.end.dates+end.window*weekFactor
#NB SQL returns truncated date values - see below - for use as date indexes
cut.dIndices<-cut.dates/dayFactor
course.open.dIndex<-course.open.date/dayFactor
course.end.dIndices<-course.end.dates/dayFactor
names(course.end.dIndices)<-courseIDs

## ****** refactor to avoid having separated censored/uncensored data at this point
##          accident of history.... would be neater just to set status based on date
#last_access_time is a UNIX datetime number
#rounds to nearest day. Multiply by 86400 to get a number to convert to a unix date
sql<-"SELECT c.ip_continent continent, c.ip_country country, u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from **gen.users u
   JOIN **gen.uoe_ip_country c ON u.anon_user_id = c.anon_user_id
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

## region is a derived attribute.
for(i in 1:length(courseIDs)){
   uncensoredList[[i]]<-cbind(uncensoredList[[i]], region=apply(uncensoredList[[i]],1,map.toRegion))
   censoredList[[i]]<-cbind(censoredList[[i]], region=apply(censoredList[[i]],1,map.toRegion))
}

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
   drawWeekLines(course.end.dIndices[courseID], max.d/7, TRUE)
   legend(x=max.d-2, y=1.0,  xjust=1, bg="white", legend=c("course start","course end"), fill=c("blue","red"), cex=0.7)
}

##
## @knitr KM_ACHIEVED
##
# compute achiievement-time estimeates for those with achievement != "none"
# basically same as KM_UNRESTRICTED - refactor!
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
   drawWeekLines(course.end.dIndices[courseID], max.d/7, TRUE)
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
restricted.Surv.List<-list()#Surv object containing time and event data for the "restricted" range
restricted.KM.List<-list()#Kaplan-Meier fit of Surv

for(i in 1:length(courseIDs)){
   
   courseID <- courseIDs[i]
   cut.dIndex <- cut.dIndices[i]
   course.end.dIndex<-course.end.dIndices[i]
   duration<-course.durations[i]
   
   censored<-censoredList[[courseID]]
   uncensored<-uncensoredList[[courseID]]
   
   data<-data.frame(time=c(uncensored$date_index, rep(cut.dIndex, length(censored$date_index)))-course.open.dIndex+1,#"death" times are >=1 (days)
                    event=c(rep(1, length(uncensored$date_index)), rep(0, length(censored$date_index))))
   my.surv <- Surv(data$time,data$event)
   restricted.Surv.List[[courseID]]<-my.surv
   
   #KM fit
   my.KMest <- survfit(my.surv~1, conf.int=0.95)#type="kaplan-meier" is default
   restricted.KM.List[[courseID]]<-my.KMest   
}
# produces two plots: S and cumulative hazard by default.
plot.restricted<-function(courseID, cumhaz=TRUE){
   my.KMest<-restricted.KM.List[[courseID]]
   xlims<-c(min(my.KMest$time), max(my.KMest$time))
   plot(my.KMest, main=paste("Kaplan-Meier",courseID, sep=" - "), xlim=xlims, xlab="Days Since Course Start")
   drawWeekLines(course.end.dIndices[courseID], course.durations[courseID])
   drawAnnotationLines(annotationList[[courseID]])
   if(cumhaz){
   # if this is linear then decay is exponential
   plot(my.KMest, main=paste("Cumulative Hazard Function (KM)",courseID, sep=" - "), sub="plot t vs -log(S)", xlim=xlims, fun="cumhaz", xlab="Days Since Course Start")
   drawWeekLines(course.end.dIndices[courseID], course.durations[courseID])
   drawAnnotationLines(annotationList[[courseID]])
   }
}

# produces the hazard function plot
plot.H.restricted<-function(courseID, cumhaz=TRUE){
   my.KMest<-restricted.KM.List[[courseID]]
   t<-my.KMest$time
   S<-my.KMest$surv
   #omit last time value
   tH<-t[-length(S)]
   H<-diff(S)/S[-length(S)]   
   xlims<-c(min(t), max(t))   
   plot(tH, H, type="l", main=paste("Kaplan-Meier",courseID, sep=" - "), xlim=xlims, xlab="Days Since Course Start", ylab="H(t)/%")
   
   drawWeekLines(course.end.dIndices[courseID], course.durations[courseID])
   drawAnnotationLines(annotationList[[courseID]])
}


##
## @knitr FITTING
##
# - fit the specified date range to exponential, Weibull, and Log-Logistic Formulae
#     the first argument is a survival object from survival package
#     the date range is expressed as day number.
#        start is the day when S=1.0 so the fit uses events with time>startDay
#     returns a list of:
#        - the return values from survreg,
#        - parameters transformed as per parametricHelpers (and desc in Typical Curves.Rmd)
#        - test statistics comparing Weibull and L-L to a null hypothesis of the exponential
# fitExp.restricted<-function(courseID){
#    my.surv<-restricted.Surv.List[[courseID]]
#    # fit the parametric expoential decay using "survreg"
#    expon<-survreg(formula = my.surv ~ 1, dist="exponential")
#    print(summary(expon))
#    print(nice.pars(expon))
# }
fit1<-function(surv, startDay, endDay){
   #change status of people after the revised endDay to be "0" i.e. censored.
   #Subset<-(surv[,"time"]>startDay) & (surv[,"time"]<=endDay)
   surv[surv[,"time"]>endDay,"status"]<-0
   surv[surv[,"time"]>endDay,"time"]<-endDay
   #I think a less negative Loglik indicates a better fit
   expon<-survreg(formula = surv ~ 1, dist="exponential")
   weibull<-survreg(formula = surv ~ 1, dist="weibull")
   loglogistic<-survreg(formula = surv ~ 1, dist="loglogistic")
   
   ## removed because Chi^2 distribution assumption only valid for exp vs Weibull AFAIK
   #test statistic to compare alternative distributions to null hypothesis of exponential
   #p statistics for the credibility of the exponential form (the null hypothesis) relative to the
   # alternative parametric. A low value suggests the exponential is not to be accepted
   # (relative to the alternative)
#    D.weibull<- -2*expon$loglik[2] + 2*weibull$loglik[2]
#    #p.value - i.e. credibility of null hypothesis relative to alternative, given data
#    p.weibull<-1-pchisq(D.weibull, weibull$df-expon$df)
#    
#    D.loglogistic<- -2*expon$loglik[2] + 2*loglogistic$loglik[2]
#    p.loglogistic<-1-pchisq(D.loglogistic, loglogistic$df-expon$df)
#    
#    #ll better than weib?
#    D.ll.w<- -2*weibull$loglik[2] + 2*loglogistic$loglik[2]
#    p.ll.w<- 1-pchisq(D.ll.w, loglogistic$df-weibull$df)
   
   #get the name of the best distribution, ignoring preference for simplicity of exp
   loglik<-c(exponential=expon$loglik[2], weibull=weibull$loglik[2], loglogistic=loglogistic$loglik[2])
   best.dist<-names(loglik[order(loglik, decreasing=T)[1]])
   
   res.fits<-list(exponential=expon,
                  weibull=weibull,
                  loglogistic=loglogistic)
   res.pars<-list(exponential=nice.pars(expon),
                  weibull=nice.pars(weibull),
                  loglogistic=nice.pars(loglogistic))
#    res.p<-list(exponential=NA,
#                   weibull=p.weibull,
#                   loglogistic=p.loglogistic,
#                   ll.vs.w=p.ll.w)
   
   results<-list(fits=res.fits, pars=res.pars, best.dist=best.dist)
   return(results)
}
#argument is fit parameters, start and end dates as for fit1
#last param allows for a dotted line to be extended past the end date
fit1.S.lines<-function(f.pars, start, end, extend=NA, cols=rainbow(3)){
   t<-seq(start,end,1)
   lines(x=t,y=S.generic(t,f.pars$exponential), col=cols[1])
   lines(x=t,y=S.generic(t,f.pars$weibull), col=cols[2])
   lines(x=t,y=S.generic(t,f.pars$loglogistic), col=cols[3])
   if(!is.na(extend)){      
      t<-seq(end,extend,1)
      lines(x=t,y=S.generic(t,f.pars$exponential), col=cols[1],lty=2)
      lines(x=t,y=S.generic(t,f.pars$weibull), col=cols[2],lty=2)
      lines(x=t,y=S.generic(t,f.pars$loglogistic), col=cols[3],lty=2)
   }
   leg<-c(parse(text=paste("'Exponential,'~lambda==",f.pars$exponential$lambda,sep="")),
          parse(text=paste("'Weibull,'~lambda==",round(f.pars$weibull$lambda,3),
                           "~gamma==", f.pars$weibull$gamma,sep="")),
          parse(text=paste("'Log-Logistic,'~rho==",round(f.pars$loglogistic$rho,3),
                           "~gamma==", f.pars$loglogistic$gamma,sep=""))
   )
   legend("topright", legend=leg, fill=cols, cex=0.8, title="Fitted Curves", bg="white")
}
# parametric fn lines for instantaneous hazard
fit1.H.lines<-function(f.pars, start, end, extend=NA){
   t<-seq(start,end-1,1)
   cols<-rainbow(3)
   lines(x=t,y=H.generic(t,f.pars$exponential), col=cols[1])
   lines(x=t,y=H.generic(t,f.pars$weibull), col=cols[2])
   lines(x=t,y=H.generic(t,f.pars$loglogistic), col=cols[3])
   if(!is.na(extend)){      
      t<-seq(end-1,extend-1,1)
      lines(x=t,y=H.generic(t,f.pars$exponential), col=cols[1],lty=2)
      lines(x=t,y=H.generic(t,f.pars$weibull), col=cols[2],lty=2)
      lines(x=t,y=H.generic(t,f.pars$loglogistic), col=cols[3],lty=2)
   }
   leg<-c(parse(text=paste("'Exponential,'~lambda==",f.pars$exponential$lambda,sep="")),
          parse(text=paste("'Weibull,'~lambda==",round(f.pars$weibull$lambda,3),
                           "~gamma==", f.pars$weibull$gamma,sep="")),
          parse(text=paste("'Log-Logistic,'~rho==",round(f$pars$loglogistic$rho,3),
                           "~gamma==", f.pars$loglogistic$gamma,sep=""))
   )
   legend("topright", legend=leg, fill=cols, cex=0.8, title="Fitted Curves", bg="white")
}

## - convenience to fit and plot for one course
#     sub.end is index of end within fit1.end
fitAndPlot<-function(courseID, sub.end=1){
   start<-fit1.start[[courseID]]
   end<-fit1.end[[courseID]][sub.end]
   #replot the survival curve with additional lines for fits
   plot.restricted(courseID,cumhaz=FALSE)
   mtext(paste("with parametric forms fitted to day",end),cex=1.2)
   f<-fit1(restricted.Surv.List[[courseID]],start,end)
   fit1.S.lines(f$pars,start,end, extend=7*(course.durations[courseID]+end.window))
   mtext(paste("The best-fit parametric form (ignoring significance):",nice.distname(f$best.dist)), side=1, line=-3, cex=1.0)
   return(f)
}

##
## @knitr REGIONS
##
reg.surv.list<-list()
reg.data.list<-list()
for(i in 1:length(courseIDs)){
   courseID <- courseIDs[i]   
   cut.dIndex <- cut.dIndices[i]
   censored<-censoredList[[courseID]]
   uncensored<-uncensoredList[[courseID]]
   
   data<-data.frame(time=c(uncensored$date_index, rep(cut.dIndex, length(censored$date_index)))-course.open.dIndex+1,#"death" times are >=1 (days)
                    event=c(rep(1, length(uncensored$date_index)), rep(0, length(censored$date_index))),
                    region=c(as.character(uncensored$region),as.character(censored$region)))
   
   ##KM for regions
   #remove regions with <100 people
   reg.nos<-summary(data$region)
   reg.use<-names(reg.nos)[reg.nos>=100]
   data<-data[data[,"region"] %in% reg.use,]
   reg.surv <- Surv(data$time,data$event)
   reg.KM<-survfit(reg.surv~data$region, conf.int=0.95)
   reg.data.list[[courseID]]<-data
   reg.surv.list[[courseID]]<-reg.surv
   
   cols<-c("black","grey",rainbow(length(reg.use)-2))
   plot(reg.KM, col=cols, lwd=2, main=paste("Kaplan-Meier Fits by Region",courseID,sep=" - "), xlab="days since course start", ylab="Surviving Fraction")
   legend("topright", legend=reg.use, fill=cols)
}
# simple wrappper to do survdiff comparisons between 2 or more regions using log-rank test
# set regions arg to be a vector of region names or leave NA for full diff
# regions passed in the arg that are not present are omitted.
# the exclude argument changes the region list to be treated as those regions to be excluded. The all.regions list is the full set, from which these exclusions are made. Regions missing from the data (e.g. if too few people) are still automatically excluded.
#returns a list containing the survdiff object and the p value from chi^2 distribution
regionDiffs<-function(courseID, regions=NA, exclude=F){
   reg.surv<-reg.surv.list[[courseID]]
   data<-reg.data.list[[courseID]]
   if(exclude){
      if(is.na(regions[1])){
         regions<-all.regions #catch logical but unlikely arg combo
      }else{
         regions<-all.regions[!(all.regions %in% regions)]
      }
   }
   if(is.na(regions[1])){
      sdiff<-survdiff(reg.surv~data$region)
      p=pchisq(sdiff$chisq,length(levels(data$region))-1, lower.tail=F)
   }else{
      regions<-regions[regions %in% levels(data$region)]
      sdiff<-survdiff(reg.surv~data$region, subset=data$region %in% regions)
      p<-pchisq(sdiff$chisq,length(regions)-1, lower.tail=F)
   }
   return(list(survdiff=sdiff,p=p))
}


##
## @knitr LA_DOW
##

# last access by day of week
for(i in 1:length(courseIDs)){
   courseID <- courseIDs[i]
   uncensored<-uncensoredList[[courseID]]
   la.dow<-table(1+(uncensored$date_index-course.open.dIndex) %% 7)
   plot(la.dow, main=paste("last access by day of week", courseID,sep=" - "), sub="day 1 is the first day of each course week")
}


## @knitr SAVE

save(file="Basic.RData", list=c("censoredList","uncensoredList", "restricted.KM.List","restricted.Surv.List", "unrestricted.KM.List", "achieved.KM.List"))

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