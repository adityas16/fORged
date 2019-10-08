library(forecast)

setwd("C:\\Users\\adity\\Dropbox\\Research\\INFORMS\\INFORMS_Shared")
#setwd("/Users/yutingyuan/Documents/Contests/")

library(ggplot2)
library(lubridate)
library(zoo)
library(plyr)
library(reshape2)

library(forecast)
library(tseries)
library(TSA)
library(pracma)


rm(list=ls())
###########################################################################################
############# Data preparation ####################################################
data=read.csv("Ten-Year-Demand.csv")


data.train=data[1:96,'x']
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


TES=function(series,slen=12,alpha=0.5,beta=0.5,gamma=0.5,n_preds=12){
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
      s[ii]=gamma*series[ii]/meanP[mm]+(1-gamma)*(s[ii-1])
    }
    else{
      s[ii]=alpha*series[ii]/meanP[mm]+(1-alpha)*(s[ii-1]+(ii%%12==0)*year_trend)  
    }
    
    if(ii%%12==1){
      print(year_trend)
      year_trend=beta*(s[ii]-s[ii-1])+(1-beta)*year_trend
    }
    #meanP[mm]=gamma*series[ii]/s[ii]+(1-gamma)*meanP[mm]
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

##########################################################################################################################################
##########################################################################################################################################

################################# Test test test, no update with new observation ###############################################################################################
series=data[1:96,'x']
pred=TES(series,12,alpha=0.5,beta=0.5,gamma=0.5,n_preds=24) #fit+predict
true=data[1:120,'x']
plot(true)
lines(pred$results)
error1=abs(true[97:120]-pred$results[97:120])
rmse=sqrt(sum(t(error1)%*%error1)/24) #rmse 2.03
pred$resSD
plot(pred$res)
# rmse=data.frame(alpha=as.numeric(),beta=as.numeric(),gamma=as.numeric(),rmse=as.numeric())
# for(aa in 1:9){
#   for(bb in 1:9){
#     for(gg in 1:9){
#       fitting=TES(series,12,alpha=aa/10,beta=bb/10,gamma=gg/10,n_preds=0) #fit+predict
#       pred=fitting$x
#       true=data[1:length(series),'x']
#       error1=abs(true-pred)
#       se=sqrt(sum(t(error1)%*%error1)/length(series))
#       print(se)
#       rmse=rbind(rmse,data.frame(alpha=aa/10,beta=bb/10,gamma=gg/10,rmse=se))
#     }
#   }
# }
# best=rmse[which(rmse$rmse==min(rmse$rmse)),]
# fitting=TES(series,12,alpha=best$alpha,beta=best$beta,gamma=best$gamma,n_preds=0)

##########################################################################################################################################
########################## Approach 1: Update alpha, beta, gamma, year_trend, meanP, with new observation ###########################################################################################
nn=24
predictions=1:nn
newdata=data[1:96,'x']
for(pp in 1:nn){ #predicting
  rmse=data.frame(alpha=as.numeric(),beta=as.numeric(),gamma=as.numeric(),rmse=as.numeric())
  for(aa in 1:9){
    for(bb in 1:9){
      for(gg in 1:9){
        fitting=TES(newdata,12,alpha=aa/10,beta=bb/10,gamma=gg/10,n_preds=0) #fit+predict
        pred=fitting$results
        true=data[1:length(newdata),'x']
        error1=abs(true-pred)
        se=sqrt(sum(t(error1)%*%error1)/length(newdata))
        #print(se)
        rmse=rbind(rmse,data.frame(alpha=aa/10,beta=bb/10,gamma=gg/10,rmse=se))
      }
    }
  }
  best=rmse[which(rmse$rmse==min(rmse$rmse)),]
  fitting=TES(newdata,12,alpha=best$alpha,beta=best$beta,gamma=best$gamma,n_preds=1)
  predictions[pp]=fitting$results[length(newdata)+1]
  newdata=c(newdata,data[length(newdata)+1,'x'])
}

error=data.test[,'x']-predictions
sqrt(sum(t(error)%*%error)/24)
############################################################################################################################################################

##########################################################################################################################################
########################## Approach 2: (1) Find the best alpha, beta, gamma via testing ###########################################################################################
nn=24
predictions=1:nn
newdata=data[1:96,'x']
rmse=data.frame(alpha=as.numeric(),beta=as.numeric(),gamma=as.numeric(),rmse=as.numeric())
for(aa in 1:9){
  for(bb in 1:9){
    for(gg in 1:9){
      fitting=TES(newdata,12,alpha=aa/10,beta=bb/10,gamma=gg/10,n_preds=24) #fit+predict
      pred=fitting$results
      true=data.test
      error1=abs(true-pred[(length(pred)-23):length(pred)])
      se=sqrt(sum(t(error1)%*%error1)/length(true))
      rmse=rbind(rmse,data.frame(alpha=aa/10,beta=bb/10,gamma=gg/10,rmse=se))
    }
  }
}
plot(rmse$alpha,rmse$rmse)
best=rmse[which(rmse$rmse==min(rmse$rmse)),]
best #alpha, beta, gamma, rmse: 0.3,0.1,0.5, 1.822738

ggplot(rmse, aes(alpha,beta)) + geom_point(aes(size = rmse))

########################## Approach 2: (2) Predict the next, update year_trend, meanP with new observation ###########################################################################################
nn=24
predictions=data.frame(pred=as.numeric(1:nn),sd=as.numeric(1:nn))
newdata=data.train
for(pp in 1:nn){ #predicting
  fitting=TES(newdata,12,alpha=best$alpha,beta=best$beta,gamma=best$gamma,n_preds=1)
  predictions[pp,]=c(fitting$results[length(newdata)+1],fitting$resSD)
  newdata=c(newdata,data[length(newdata)+1,'x'])
}

error=data.test-predictions[,'pred']
sqrt(sum(t(error)%*%error)/24) #1.943

plot(data[,'x'])
lines(fitting$results)
##########################################################################################################################################


############################## Inventory control: 2004-2005 ##########################################################
control=data.frame(month=as.numeric(97:120),pp_demand=predictions$pred,pp_demand_sd=predictions$sd,true_demand=data.test,
                   starting_inventory=as.numeric(1:24),replenishment=as.numeric(1:24),ending_inventory=as.numeric(1:24),
                   next_inventory_goal=as.numeric(1:24))
colnames(control)
#starting: made up from excel sheet
control[1,'starting_inventory']=81
control[1,'ending_inventory']=-8.88
control[1,'replenishment']=control[1,'ending_inventory']-control[1,'starting_inventory']+control[1,'true_demand']

for(mm in 97:120){
  index=mm-96
  control[index,'ending_inventory']=control[index,'starting_inventory']+control[index,'replenishment']-control[index,'true_demand']
  if(mm<120){
  control[index,'next_inventory_goal']=qnorm(3/4)*control[index+1,'pp_demand_sd']+control[index+1,'pp_demand'] #make replenishment decisions at the end of each period, after backorders are fulfilled
  control[index+1,'replenishment']=max(0,control[index,'next_inventory_goal']-max(0,control[index,'ending_inventory']))
  control[index+1,'starting_inventory']=max(0,control[index,'ending_inventory'])
  }
}

control$cost=0
for(mm in 97:120){
  index=mm-96
  control[index,'cost']=(control[index,'ending_inventory']>0)*control[index,'ending_inventory']+(control[index,'ending_inventory']<0)*3*(-control[index,'ending_inventory'])
}
sum(control[2:24,'cost'])/24 #2.36

############################## Inventory control: 1996-2005 ##########################################################
control=data.frame(month=as.numeric(1:120),pp_demand=predictions$pred,pp_demand_sd=predictions$sd,true_demand=data.test,
                   starting_inventory=as.numeric(1:120),replenishment=as.numeric(1:120),ending_inventory=as.numeric(1:120),
                   next_inventory_goal=as.numeric(1:120))
colnames(control)

control[1:96,'pp_demand']=fitting$results[1:96]
control[1:96,'pp_demand_sd']=sd(fitting$res)
control[1:96,'true_demand']=data.train
control[1,'starting_inventory']=60
control[1,'replenishment']=0
control[1,'ending_inventory']

for(index in 1:120){
  control[index,'ending_inventory']=control[index,'starting_inventory']+control[index,'replenishment']-control[index,'true_demand']
  if(index<120){
    control[index,'next_inventory_goal']=qnorm(3/4)*control[index+1,'pp_demand_sd']+control[index+1,'pp_demand'] #make replenishment decisions at the end of each period, after backorders are fulfilled
    control[index+1,'replenishment']=max(0,control[index,'next_inventory_goal']-max(0,control[index,'ending_inventory']))
    control[index+1,'starting_inventory']=max(0,control[index,'ending_inventory'])
  }
}

control$cost=0
for(index in 1:120){
  control[index,'cost']=(control[index,'ending_inventory']>0)*control[index,'ending_inventory']+(control[index,'ending_inventory']<0)*3*(-control[index,'ending_inventory'])
}
sum(control$cost)/120 #3.438

