source("STES.r")
##########################################################################################################################################
##########################  (1) Find the best alpha_month, beta, alpha_quarter via testing ###########################################################################################

estimate_hyper_parameters = function(training_data){
rmse=data.frame(alpha_month=as.numeric(),beta=as.numeric(),alpha_quarter=as.numeric(),rmse=as.numeric())
for(aa in 1:9){
  for(bb in 1:9){
    for(gg in 1:9){
      fitting=STES(training_data,list(alpha_month=aa/10,beta=bb/10,alpha_quarter=gg/10)) #fit+predict
      pred=fitting$fitted
      error1=abs(training_data-pred)
      se=sqrt(sum(t(error1)%*%error1)/length(training_data))
      rmse=rbind(rmse,data.frame(alpha_month=aa/10,beta=bb/10,alpha_quarter=gg/10,rmse=se))
    }
  }
}
best=rmse[which(rmse$rmse==min(rmse$rmse)),][1,]
best
#alpha_month beta alpha_quarter     rmse
#332         0.5  0.1           0.8 2.851543
}



