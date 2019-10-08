library(ggplot2)


data.train=data[1:96,'x']
#data.train=data$x

##########################################################################################################################################
##########################  (1) Find the best alpha_month, beta, alpha_quarter via testing ###########################################################################################

rmse=data.frame(alpha_month=as.numeric(),beta=as.numeric(),alpha_quarter=as.numeric(),rmse=as.numeric())
for(aa in 1:9){
  for(bb in 1:9){
    for(gg in 1:9){
      fitting=TES(data.train,12,alpha_month=aa/10,beta=bb/10,alpha_quarter=gg/10,n_preds=0) #fit+predict
      pred=fitting$results
      true=data.train
      error1=abs(true-pred)
      se=sqrt(sum(t(error1)%*%error1)/length(true))
      rmse=rbind(rmse,data.frame(alpha_month=aa/10,beta=bb/10,alpha_quarter=gg/10,rmse=se))
    }
  }
}
ggplot(rmse, aes(alpha_month,beta)) + geom_point(aes(size = rmse))

plot(rmse$alpha_month,rmse$rmse)
best=rmse[which(rmse$rmse==min(rmse$rmse)),][1,]
best #alpha_month, beta, alpha_quarter, rmse: 0.3,0.1,0.5, 1.822738
#alpha_month beta alpha_quarter   rmse
#559   0.7  0.9   0.1 2.944





########################## Approach 2: (2) Predict the next, update year_trend, meanP with new observation ###########################################################################################

nn=24
predictions=data.frame(pred=as.numeric(1:nn),sd=as.numeric(1:nn))
newdata=data.train
for(pp in 1:nn){ #predicting
  fitting=TES(newdata,12,alpha_month=best$alpha_month,beta=best$beta,alpha_quarter=best$alpha_quarter,n_preds=1)
  predictions[pp,]=c(fitting$results[length(newdata)+1],fitting$resSD)
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
#With quarter weights: 2.05











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



