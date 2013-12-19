## ****************************************************
## Created: Adam Cooper, Cetis, Dec 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Compute/plot parametric survival fns
## ****************************************************

##
##

source("./parametricHelpers")

# Set the same time interval for all plots. Units = days but use 0.1 day as plotting interval
plot.interval<-0.1
t<-seq(0,35, plot.interval)

##
## standard parameterised plots.
## ....... would benefit from some refactoring
##
# plot an exponential and one or more Weibull Survival Function curves
plotS.ExpWeib<-function(par.lambda, par.gamma, main="Survival Function"){
   plot(t, exp(-par.lambda*t), type="l", xlab="time/days", ylab="S(t)",
        main=bquote(atop(.(main),lambda==.(par.lambda))))
   cols<-rainbow(length(par.gamma))
   for(i in 1:length(par.gamma)){
      lines(t,exp(-par.lambda*t^par.gamma[i]), col=cols[i])
   }
   leg<-c("Exponential", parse(text=paste("'Weibull,'~gamma==", par.gamma,sep="")))
   legend("topright", legend=leg, fill=c("black", cols), cex=0.8)
}

# plot an exponential and one or more Weibull Survival Function curves
# with the Weibull lambda values adjusted to give a cross-over at the half-life
plotS.ExpWeib.Half<-function(par.lambda, par.gamma, main="Survival Function (half-life crossing)"){
   plot(t, exp(-par.lambda*t), type="l", xlab="time/days", ylab="S(t)",
        main=main)
   cols<-rainbow(length(par.gamma))
   t.half<-log(2)/par.lambda
   par.lambda.W<-par.lambda * t.half^(1-par.gamma)
   for(i in 1:length(par.gamma)){
      lines(t,exp(-par.lambda.W[i]*t^par.gamma[i]), col=cols[i])
   }
   leg<-c("Exponential", parse(text=paste("'Weibull,'~lambda==",round(par.lambda.W,3),"~gamma==", par.gamma,sep="")))
   legend("topright", legend=leg, fill=c("black", cols), cex=0.8)
}


# plot an exponential (gradient 0 line) and one or more Weibull Hazard Function curves
plotH.ExpWeib<-function(par.lambda, par.gamma, main="Hazard Function"){
   Hexp<-rep(-100*par.lambda, length(t)-1)#omit last time value
   tH<-t[-length(Hexp)]
   plot(tH, Hexp , type="l", xlab="time/days", ylab="H(t)/%",
        main=bquote(atop(.(main),lambda==.(par.lambda))), ylim=c(-2*100*par.lambda,0.0))
   cols<-rainbow(length(par.gamma))
   for(i in 1:length(par.gamma)){
      Sweib<-exp(-par.lambda*t^par.gamma[i])
      #NB: since the time interval in t is plot.interval
      Hweib<-(100/plot.interval)*diff(Sweib)/Sweib[-length(Sweib)]      
      lines(tH,Hweib, col=cols[i])
   }
   
   leg<-c("Exponential", parse(text=paste("'Weibull,'~gamma==", gamma,sep="")))
   legend("topright", legend=leg, fill=c("black", cols), cex=0.8)
}

plotH.ExpWeib.Half<-function(par.lambda, par.gamma, main="Hazard Function (half-life crossing)"){
   Hexp<-rep(-100*par.lambda, length(t)-1)#omit last time value
   tH<-t[-length(Hexp)]
   plot(tH, Hexp , type="l", xlab="time/days", ylab="H(t)/%",
        main=main, ylim=c(-2*100*par.lambda,0.0))
   cols<-rainbow(length(par.gamma))
   t.half<-log(2)/par.lambda
   par.lambda.W<-par.lambda * t.half^(1-par.gamma)
   for(i in 1:length(par.gamma)){
      Sweib<-exp(-par.lambda.W[i]*t^par.gamma[i])
      #NB: since the time interval in t is plot.interval
      Hweib<-(100/plot.interval)*diff(Sweib)/Sweib[-length(Sweib)]      
      lines(tH,Hweib, col=cols[i])
   }   
   leg<-c("Exponential", parse(text=paste("'Weibull,'~lambda==",round(par.lambda.W,3),"~gamma==", par.gamma,sep="")))
   legend("topright", legend=leg, fill=c("black", cols), cex=0.8)
}



##
##


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