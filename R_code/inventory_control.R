
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

