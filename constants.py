mean_demand = 98
BIN_WIDTH =0.5
DEMAND_LOWER_BOUND= mean_demand - 30
DEMAND_UPPER_BOUND= mean_demand + 30

INVENTORY_LEVEL_LOWER_BOUND= mean_demand - 30
INVENTORY_LEVEL_UPPER_BOUND= mean_demand + 30


INVENTORY_POSITION_LOWER_BOUND= INVENTORY_LEVEL_LOWER_BOUND + mean_demand
INVENTORY_POSITION_UPPER_BOUND= INVENTORY_LEVEL_UPPER_BOUND + mean_demand


STARTING_INVENTORY=73
START_PERIOD=97
LAST_PERIOD = 120


#FILE IO
OUTPUT_FILE = "output.csv"