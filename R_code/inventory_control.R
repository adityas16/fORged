
############################## Inventory control: 2004-2005 ##########################################################
inventory_control = function(predictions,true_demand){
control=data.frame(month=as.numeric(TEST_MONTHS), 
                   beginning_inventory=as.numeric(1:24),order_quantity=as.numeric(1:24),ending_inventory=as.numeric(1:24),
                   holding_cost=as.numeric(1:24),backorder_cost=as.numeric(1:24),total_cost=as.numeric(1:24),
                   pp_demand=predictions$pred,pp_demand_sd=predictions$sd,true_demand=true_demand,
                   next_inventory_goal=as.numeric(1:24))
colnames(control)
#starting: made up from excel sheet
incoming_order_quantity =0
prev_end_inventory = 60

index=1
for(mm in TEST_MONTHS){
  control[index,]$beginning_inventory = prev_end_inventory + incoming_order_quantity
  control[index,]$ending_inventory = control[index,]$beginning_inventory - control[index,]$true_demand
  
  if(mm < TEST_MONTHS[length(TEST_MONTHS)]){
    control[index,]$next_inventory_goal = 0.6744898 *control[index+1,]$pp_demand_sd + control[index+1,]$pp_demand
  }
  else{
    control[index,]$next_inventory_goal = 73
  }
  control[index,'order_quantity']=max(0,control[index,'next_inventory_goal']-control[index,'ending_inventory'])
  
  prev_end_inventory = control[index,]$ending_inventory
  incoming_order_quantity = control[index,]$order_quantity
  index = index + 1
}

control$holding_cost = (control$ending_inventory>0)*control$ending_inventory
control$backorder_cost = (control$ending_inventory<0)*3*(-control$ending_inventory)
control$total_cost= control$holding_cost + control$backorder_cost

mean(tail(control$total_cost,-1))

control[,1:7]
}

inventory_control(predictions,data.test)
