## ****************************************************
## Created: Adam Cooper, Cetis, Dec 2013
## This source code was produced for The University of
## Edinburgh DEI as part of their MOOC initiative.
## Various functions of utility value for survival analysis
## ****************************************************



# This provides the chosen parameterisation forms and transformation from survreg fitting parameters
# The S functions may appear to be a bit pointless but it makes for clarity in the face of >1 parameterisation in common use


# supply with the results of surveg(... dist="weibull") and it returns the parameters lambda and gamma for Weibull distribution survival fn: S(t)=exp(-\lambda t^\gamma)
weibull.pars<-function(weib.survreg){
   pars=list(lambda=as.numeric(exp(-weib.survreg$coefficient/weib.survreg$scale)),
             gamma=1/weib.survreg$scale)
   return(pars)
}

# supply with the results of surveg(... dist="weibull") and it returns the parameters rho and gamma for survival fn: S(t)=[{1+({\rho t})^\gamma}]^{-1}
loglogistic.pars<-function(ll.survreg){
   pars=list(rho=exp(-loglogistic$coefficient),
             gamma=1/loglogistic$scale)
   return(pars)
}

# Survival function for Weibull
S.Weib<-function(T,lambda, gamma){
   S<- exp(-lambda*t^gamma)
   return(S)
}

# Survival Function for log-logistic
S.ll<-function(T, rho, gamma){
   S<-1/(1+(T*rho)^gamma)
   return(S)
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