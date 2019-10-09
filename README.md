# fORged

Requirements:
R version 3.4.4 or later


To run the code:
main.R is the script that needs to be run (which will in turn source 4 other scripts in the folder)
From the terminal or command line, change directory to the folder containing the main.R script
Place the file containing the demand for 2006 and 2007 in the folder and name it as 'Test-Demand.csv'. The format of this file must be the same as was provided in 'Ten-Year-Demand.csv'.
Run the command : <Path to R installation>/bin/Rscript main.R for linux or <Path to R installation>\bin\Rscript.exe main.R for windows
The output will be written to two files: 
  (1) inventory_state.csv contains the state of the inventory at the beginning and end of the month, quantity ordered and the costs for each of the 24 months from 121-144
  (2) aggregate_stats.csv contains the 
 Samples of all the input file and the 2 output files have been included in the 'samples' folder

Possible errors:

In file(filename, "r", encoding = encoding) :
  cannot open file 'constants.R': No such file or directory
Execution halted

Solution:
Before running the main.r, change directory into the folder containing it.
