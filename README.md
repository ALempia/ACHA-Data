# ACHA-Data
This repository stores and updates the data used the web app (*Link coming SOON!*). 

A github action runs [Daily_script.R](https://github.com/ALempia/ACHA-Data/blob/main/Daily_script.R) each day at 7am EDT. It updates the master schedules for all 5 divisions, uses the schedule to update team ratings. Power Rankings update each Wednesday, as does the file [weekend.csv](https://github.com/ALempia/ACHA-Data/blob/main/Data/weekend.csv), which contains the weekend schedule (Th-Su) for all divisions. 

[functions.R](https://github.com/ALempia/ACHA-Data/blob/main/functions.R) contains the meat of the code used in Daily_script. 
