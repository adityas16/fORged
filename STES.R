source("constants.R")

#Initiaize hyperparameters
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

get_month =function(month_number){
  month_number%%12+(month_number%%12==0)*12
}

#Fit training data given a set of hyperparameters
STES=function(series,hyper_parameters){
  slen=12
  alpha_month=hyper_parameters$alpha_month
  alpha_quarter=hyper_parameters$alpha_quarter
  beta=hyper_parameters$beta
  initials=initial_trend(series,slen)
  year_trend=initials$year_trend
  meanP=initials$meanP
  
  s=1:(length(series)+1)
  fitted=1:(length(series)) #fitted series
  fitted[1]=series[1]
  residual=1:length(series)
  residual[1]=0
  s[1]=fitted[1]
  for(ii in 2:length(series)){#fitting training data
    yy=ceiling(ii/12)
    mm=get_month(ii)
    fitted[ii]=(s[ii-1])*meanP[mm]
    residual[ii]=fitted[ii]-series[ii]
    if(ii%%3==1){
      s[ii]=alpha_quarter*series[ii]/meanP[mm]+(1-alpha_quarter)*(s[ii-1])
    }
    else{
      s[ii]=alpha_month*series[ii]/meanP[mm]+(1-alpha_month)*(s[ii-1]+(get_month(ii)==12)*year_trend)  
    }
    
    if(ii%%12==1){
      year_trend=beta*(s[ii]-s[ii-1])+(1-beta)*year_trend
    }
  }
  
  ii=length(series)+1
  yy=ceiling(ii/12)
  mm=get_month(ii)
  s[ii]=s[ii-1]
  predicted_mu=s[ii]*meanP[mm]
  relevant_months = get_month(2:length(series))==mm
  relevant_residuals = residual[2:length(residual)][relevant_months]
  relevant_residuals = relevant_residuals - mean(relevant_residuals)
  ssr = sum(relevant_residuals^2)
  predicted_sigma = sqrt(ssr/(sum(relevant_months)-1))
  return(list(fitted=fitted,meanP=meanP,year_trend=year_trend,lastS=s[ii],res=residual,prediction=c(mu=predicted_mu,sigma=predicted_sigma)))
}



