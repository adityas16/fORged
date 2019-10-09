rm(list=ls())
source("constants.R")
source("parameter_estimation.R")
source("inventory_control.R")


#Read demand from input file
demand.train=read.csv("Ten-Year-Demand.csv")[,'x']
demand.test=read.csv("Test-Demand.csv")[,'x']
demand=c(demand.test,demand.train)

#Estiamte hyper parameters needed by STES
hyper_parameters = estimate_hyper_parameters(demand.train)


#Use estimated hyper parameters along with demand data till period t to predict demand for period t+1
predictions=data.frame(pred=as.numeric(1:NUM_TEST_PERIODS),sd=as.numeric(1:NUM_TEST_PERIODS))
newdata=demand.train
for(pp in 1:24){
  fit=STES(newdata,hyper_parameters)
  predictions[pp,]=c(fit$prediction["mu"],fit$prediction["sigma"])
  #Adding observed data after predicting demand for the period
  newdata=demand[1:(LAST_TRAINING_MONTH+pp)]
}

#Run inventory control using demand forecasts and actual demand
inventory_state = inventory_control(predictions,demand.test)


#Write results to output files
write.table(inventory_state,"inventory_state.csv",col.names = T,sep=",",row.names = F)

aggregate_stats =data.frame(c('overall','holding','backorder'),c(sum(inventory_state$total_cost),sum(inventory_state$holding_cost),sum(inventory_state$backorder_cost)))
colnames(aggregate_stats) = c('cost','total')
aggregate_stats$average = aggregate_stats$total/NUM_TEST_PERIODS
write.table(aggregate_stats,"aggregate_stats.csv",col.names = T,sep=",",row.names = F)
