source("functions.R")

f <- function(lg){
  p <- read_rds(paste0("Data/prior/prior_", lg, ".rds"))
  sched <- read_data(lg, 24)
  
  m <- get_ratings(sched, p, date = Sys.Date())
  power <- power_Rank(m, date = Sys.Date())
  
  path1 <- paste0("Data/schedule/schedule_", lg, ".csv")
  path2 <- paste0("Data/ratings/ratings_", lg, ".rds")
  path3 <- paste0("Data/power/power_", lg, ".csv")
  write_csv(sched, path1)
  write_rds(m, path2)
  write_csv(power, path3)
}

map(.x = c("M1", "M2", "M3", "W1", "W2"), f)


