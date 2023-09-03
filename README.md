# ACHA-Data
This repository stores and updates the data used in the web app [ACHA Analytics](https://andersl.shinyapps.io/ACHA_Analytics/). 

[Schedule_Update.yml](https://github.com/ALempia/ACHA-Data/blob/main/.github/workflows/Schedule_Update.yml) runs [Schedule_Update.R](https://github.com/ALempia/ACHA-Data/blob/main/Schedule_Update.R) each day at 6:00am EDT. It updates the master schedules for all 5 divisions.

[Ratings_Update.yml](https://github.com/ALempia/ACHA-Data/blob/main/.github/workflows/Ratings_Update.yml) runs [Ratings_Update.R](https://github.com/ALempia/ACHA-Data/blob/main/Ratings_Update.R) each day at 6:30am EDT. It uses the schedule to update team ratings for all 5 divisions.

[Power_Update.yml](https://github.com/ALempia/ACHA-Data/blob/main/.github/workflows/Power_Update.yml) runs [Power_Update.R](https://github.com/ALempia/ACHA-Data/blob/main/Power_Update.R) each Wednesday at 7:00am EDT. It updates the Power Rankings for all 5 divisions.

[Weekend_Schedule.yml](https://github.com/ALempia/ACHA-Data/blob/main/.github/workflows/Weekend_Schedule.yml) runs [Weekend_Schedule.R](https://github.com/ALempia/ACHA-Data/blob/main/Weekend_Schedule.R) each Wednesday at 7:30am EDT. It uses the schedule to update the full Th-Su schedule.

[functions.R](https://github.com/ALempia/ACHA-Data/blob/main/functions.R) contains the meat of the code used. 

[app-functions.R](https://github.com/ALempia/ACHA-Data/blob/main/app-functions.R) contains the meat of the code used in the app, the code for which can be found in [app.R](https://github.com/ALempia/ACHA-Data/blob/main/app.R)
