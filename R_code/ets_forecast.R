library(forecast)
source("constants.R")



rm(list=ls())
###########################################################################################
############# Data preparation ####################################################
data=read.csv("Ten-Year-Demand.csv")
data.train=data[1:L,'x']
data.test=data[97:120,'x']

#################################################################################
##########################################################################################################################################


###############################################################################################################################################################################################################
initial_trend=function(series,slen=12){ #calculate b_{t/slen}, yearly trend
  ys=floor(length(series)/12) #years in training data
  YSAVG=1:ys
  for(yy in 1:ys){
    YSAVG[yy]=sum(series[(12*(yy-1)+1):(12*yy)])/12
  }
  year_trend=sum(diff(YSAVG,1)/(ys-1)) #initial year trend
  
  P=matrix(0,nrow=ys,ncol=12) #proportion matrix over years, #yr by #month
  for(ii in 1:(ys*12)){
    yy=ceiling(ii/12)
    propYearAvg=series[ii]/YSAVG[yy]
    mm=ii%%12+(ii%%12==0)*12
    P[yy,mm]=propYearAvg
  }
  meanP=colMeans(P) #initial proportions (or seasonal trends)
  return(list(year_trend=year_trend,meanP=meanP))
}


TES=function(series,slen=12,alpha_month=0.5,beta=0.5,alpha_quarter=0.5,n_preds=12){
  initials=initial_trend(series,slen)
  year_trend=initials$year_trend
  meanP=initials$meanP
  
  s=1:(length(series)+n_preds)
  results=s #fitted series
  results[1]=series[1]
  residual=1:length(series)
  residual[1]=0
  s[1]=results[1]
  for(ii in 2:length(series)){#fitting training data
    yy=ceiling(ii/12)
    mm=ii%%12+(ii%%12==0)*12
    results[ii]=(s[ii-1])*meanP[mm]
    residual[ii]=results[ii]-series[ii]
    if(ii%%3==1){
      s[ii]=alpha_quarter*series[ii]/meanP[mm]+(1-alpha_quarter)*(s[ii-1])
    }
    else{
      s[ii]=alpha_month*series[ii]/meanP[mm]+(1-alpha_month)*(s[ii-1]+(ii%%12==0)*year_trend)  
    }
    
    if(ii%%12==1){
      print(year_trend)
      year_trend=beta*(s[ii]-s[ii-1])+(1-beta)*year_trend
    }
  }
  if(n_preds>0){
    for(hh in 1:n_preds){ #predicting
      ii=length(series)+hh
      yy=ceiling(ii/12)
      mm=ii%%12+(ii%%12==0)*12
      s[ii]=s[ii-1]
      results[ii]=s[ii]*meanP[mm]
    }
  }
  return(list(results=results,meanP=meanP,year_trend=year_trend,lastS=s[ii],res=residual,resSD=sd(residual)))
}


