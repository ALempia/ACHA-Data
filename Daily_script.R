source("functions.R")
require(lubridate)

date <- Sys.Date()
day <- wday(date, week_start = 1)

lgs <- c("M1", "W1", "M2", "M3", "W2")

# DAILY UPDATES
lgs %>% map(function(x) edit_sched_ratings(lg = x, date = date))

# Weekly (Wednesday) Updates
if(day == 3){
  get_W_Su(date)
  
  lgs %>% map(function(x) update_Power(lg = x, date = date))
}
