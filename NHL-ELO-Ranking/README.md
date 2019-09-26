Hello! Please first read the slides for a more comprehensive description for this project.

Recommended viewing steps:
* If you are interested in seeing the process for cleaning, merging and manipulating the original data (player_information, player_season_statistics, etc) then view the file 'Data_Cleaning_Process.R'

* Otherwise, simply load 'hockey_cleaned_age.rdata' and continue to 'Creating_Models.R'

* For the modeling process, I've included steps for creating the data matrix by hand and by for-loop. In the case of this project, I used both OLS and the Bradley-Terry package in R, but other models are applicable as well, like logistic regression. 

* If you just want to skip that as well and just see the coefficients, then load 'hockey_zscores.rdata' or 'linear_hockey.rdata' into the file 'Plotting.R' to see various plots 

* If there are any questions or concerns feel free to contact me here or via twitter @temipanarin 

Additional Information:

* The dictionary describes the different columns within the (original uncleaned) player information files.
* All data comes from eliteprospects.com 
