source("parameter_estimation.R")
source("inventory_control.R")
source("constants.R")


#Read demand from input file
demand=read.csv("Twelve-Year-Demand.csv")
demand.train=demand[TRAINING_MONTHS,'x']
demand.test=demand[TEST_MONTHS,'x']

#Estiamte hyper parameters needed by STES
hyper_parameters = estimate_hyper_parameters(demand.train)


#Use estimated hyper parameters along with demand data till period t to predict demand for period t+1
predictions=data.frame(pred=as.numeric(1:nn),sd=as.numeric(1:nn))
newdata=demand.train
for(pp in 1:24){
  fit=STES(newdata,hyper_parameters)
  predictions[pp,]=c(fit$prediction["mu"],fit$prediction["sigma"])
  #Adding observed data after predicting demand for the period
  newdata=c(newdata,demand[length(newdata)+1,'x'])
}

#Run inventory control using demand forecasts and actual demand
inventory_state = inventory_control(predictions,data.test)


#Write results to output files
write.table(inventory_state,"inventory_state.csv",col.names = T,sep=",",row.names = F)
#Write aggregate stats to a text file