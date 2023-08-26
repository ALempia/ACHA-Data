source("functions.R")

date <- Sys.Date()

lgs <- c("M1", "W1", "M2", "M3", "W2")

lgs %>% map(function(x) update_Power(lg = x, date = date))