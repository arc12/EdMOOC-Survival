## ****************************************************
## Created: Adam Cooper, Cetis, Dec 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Various functions of utility value for survival analysis
## ****************************************************



# This provides the chosen parameterisation forms and transformation from survreg fitting parameters
# The S functions may appear to be a bit pointless but it makes for clarity in the face of >1 parameterisation in common use

##
## - take the distribution name as used by survreg and make a nicely capitalised version for display
##
nice.distname<-function(n){
   niceNames<-c(exponential="Exponential",
                weibull="Weibull",
                loglogistic="Log-Logistic")
   niceName<-niceNames[n]
   if(is.na(niceName))niceName<-n
   
   return(niceName)
}

##
## - transform the parameters returned by survreg into my preferred forms
##
# supply with the results of survreg. dist must be from: exponential|weibull|loglogistic
# returns with a list of named parameters, according to the distribution
# .. exponential: lambda in  S(t)=exp(-\lambda t)
# .. weibull: labda and gamma in S(t)=exp(-\lambda t^\gamma)
# .. loglogistic: rho and gamma in S(t)=[{1+({\rho t})^\gamma}]^{-1}
nice.pars<-function(obj.survreg){
   pars<-list()
   if(obj.survreg$dist == "exponential"){
      pars<-list(lambda=as.numeric(exp(-obj.survreg$coefficient)))
   }else if (obj.survreg$dist == "weibull"){
      pars<-list(lambda=as.numeric(exp(-obj.survreg$coefficient/obj.survreg$scale)),
                gamma=1/obj.survreg$scale)
   }else if (obj.survreg$dist == "loglogistic"){
      pars<-list(rho=exp(-obj.survreg$coefficient),
                gamma=1/obj.survreg$scale)
   }
   pars$dist<-obj.survreg$dist
   return(pars)
}


# # supply with the results of surveg(... dist="weibull") and it returns the parameters lambda and gamma for Weibull distribution survival fn: S(t)=exp(-\lambda t^\gamma)
# weibull.pars<-function(weib.survreg){
#    pars=list(lambda=as.numeric(exp(-weib.survreg$coefficient/weib.survreg$scale)),
#              gamma=1/weib.survreg$scale)
#    return(pars)
# }
# 
# # supply with the results of surveg(... dist="loglogistic") and it returns the parameters rho and gamma for survival fn: S(t)=[{1+({\rho t})^\gamma}]^{-1}
# loglogistic.pars<-function(ll.survreg){
#    pars=list(rho=exp(-loglogistic$coefficient),
#              gamma=1/loglogistic$scale)
#    return(pars)
# }

#return the value of a the parametric survival function with time=T
#pars is the return list from nice.pars, which includes the distribution type
S.generic<-function(T,pars){
   S<-0
   if(pars$dist == "exponential"){
      S<-S.Exp(T,pars$lambda)
   }else if (pars$dist == "weibull"){
      S<-S.Weib(T,pars$lambda,pars$gamma)
   }else if (pars$dist == "loglogistic"){
      S<-S.LL(T,pars$rho,pars$gamma)
   }
   return(S)
}

# Survival function for exponential - trivial
S.Exp<-function(T,lambda){
   S<- exp(-lambda*T)
   return(S)
}

# Survival function for Weibull
S.Weib<-function(T,lambda, gamma){
   S<- exp(-lambda*T^gamma)
   return(S)
}

# Survival Function for log-logistic: $$S(t)=[{1+({\rho t})^\gamma}]^{-1}$$
S.LL<-function(T, rho, gamma){
   S<-1/(1+(T*rho)^gamma)
   return(S)
}

##
## - parametric Hazard Functions, function args as for S.
##       This is instantaneous hazard rate, not cumulative
##
H.generic<-function(T,pars){
   S<-0
   if(pars$dist == "exponential"){
      H<-H.Exp(T,pars$lambda)
   }else if (pars$dist == "weibull"){
      H<-H.Weib(T,pars$lambda,pars$gamma)
   }else if (pars$dist == "loglogistic"){
      H<-H.LL(T,pars$rho,pars$gamma)
   }
   return(H)
}
H.Exp<-function(T,lambda){
   return(rep(-lambda,length(T)))
}
H.Weib<-function(T,lambda, gamma){
   H<- - lambda * gamma * T^(gamma-1)
   return(H)
}
H.LL<-function(T, rho, gamma){
   H<--(gamma * rho * (rho*T)^(gamma-1))/(1+(rho*T)^gamma)
   return(H)
}



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