
data.train=data[1:96,'x']
#data.train=data$x

##########################################################################################################################################
##########################  (1) Find the best alpha_month, beta, alpha_quarter via testing ###########################################################################################

estimate_hyper_parameters = function(training_data){
rmse=data.frame(alpha_month=as.numeric(),beta=as.numeric(),alpha_quarter=as.numeric(),rmse=as.numeric())
for(aa in 1:9){
  for(bb in 1:9){
    for(gg in 1:9){
      fitting=STES(training_data,12,alpha_month=aa/10,beta=bb/10,alpha_quarter=gg/10) #fit+predict
      pred=fitting$fitted
      true=data.train
      error1=abs(true-pred)
      se=sqrt(sum(t(error1)%*%error1)/length(true))
      rmse=rbind(rmse,data.frame(alpha_month=aa/10,beta=bb/10,alpha_quarter=gg/10,rmse=se))
    }
  }
}
best=rmse[which(rmse$rmse==min(rmse$rmse)),][1,]
best
#alpha_month beta alpha_quarter     rmse
#332         0.5  0.1           0.8 2.851543
}




########################## Approach 2: (2) Predict the next, update year_trend, meanP with new observation ###########################################################################################

generate_all_STES_forecast = function(demand,hyper_parameters)
nn=24
predictions=data.frame(pred=as.numeric(1:nn),sd=as.numeric(1:nn))
newdata=data.train
for(pp in 1:nn){ #predicting
  fitting=STES(newdata,12,alpha_month=hyper_parameters$alpha_month,beta=hyper_parameters$beta,alpha_quarter=hyper_parameters$alpha_quarter)
  predictions[pp,]=c(fitting$prediction["mu"],fitting$prediction["sigma"])
  newdata=c(newdata,data[length(newdata)+1,'x'])
}

error=data.test-predictions[,'pred']
sqrt(sum(t(error)%*%error)/24) 
predictions=cbind(predictions,data.test)
predictions$error = predictions$pred - predictions$data.test
plot(predictions$error)
#2.23. Inbuilt ETS = 2.46
#Without yearly trend: 2.15
#With trend = 2.33
#With quarter weights:  2.05305











rmse = function(actual,predicted){
  sqrt(mean((actual-predicted)^2))
}
fit = TES(data.train,12,alpha_month=best$alpha_month,beta=best$beta,alpha_quarter=best$alpha_quarter,n_preds=0)
a=data.frame(fit$results,data.train)
a$diff = data.train - fit$results
a$error = abs(a$diff)
mean(a$error)
plot(a$diff)
rmse(data.train , fit$results)
#2.87
a$month=1:nrow(a)
a$month_num = (a$month-1)%%12
to_csv(a)



