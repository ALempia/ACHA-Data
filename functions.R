require(httr)
require(stringr)
require(tidyverse)
require(jsonlite)
require(bpr)

pull_url <-function(code){
  require(httr)
  require(stringr)
  require(tidyverse)
  require(jsonlite)
  
  url <- paste0("https://lscluster.hockeytech.com/feed/index.php?feed=statviewfeed&view=schedule&team=-1&season=",
                code, 
                "&month=-1&location=homeaway&key=e6867b36742a0c9d&client_code=acha&site_id=2&league_id=1&division_id=-1&lang=en")
  
  page_json <- httr::GET(url) %>%
    httr::content(as = "text") %>% 
    stringr::str_remove("^\\(\\[") %>% 
    stringr::str_remove("\\]\\)$") %>% 
    jsonlite::parse_json() 
  
  page_data <- page_json %>% purrr::pluck("sections", 1, "data") %>% 
    tibble::tibble() %>% tidyr::unnest_wider(1) %>% 
    tidyr::unnest_wider(c("prop", "row"), names_sep = "_")
  
  data <- page_data %>% select(row_game_id, row_date_with_day, row_home_goal_count, row_visiting_goal_count, row_game_status,        
                               row_home_team_city, row_visiting_team_city)
  
  colnames(data) <- c("game_id", "date_w_day", "homeG", "awayG", "status", "home", "away")
  
  if(code >= 21){
    data <- data %>% mutate(
      phase = case_when(
        code <= 23 | code >= 34 ~ "Regular", 
        code == 32 ~ "Regional",
        TRUE ~ "National"
      )
    )
  }
  else{
    data <- data %>% mutate(
      phase = case_when(
        code <= 11 ~ "Regular",
        code == 12 ~ "Regional",
        TRUE ~ "National"
      )
    )
  }
  data
}

read_data <- function(lg = c("M1", "M2", "M3", "W1", "W2"), season, include_manual = F){
  require(bpr)
  
  codes <- NULL
  codes <- case_when(
    lg == "M1" & season == 23 ~ c(21, 24, 0, 0),
    lg == "M2" & season == 23 ~ c(21, 32, 26, 27),
    lg == "M3" & season == 23 ~ c(21, 28, 29, 0),
    lg == "W1" & season == 23 ~ c(22, 25, 0, 0),
    lg == "W2" & season == 23 ~ c(23, 30, 31, 0),
    lg == "M1" & season == 22 ~ c(10, 15, 0, 0),
    lg == "M2" & season == 22 ~ c(10, 12, 13, 14),
    lg == "M3" & season == 22 ~ c(10, 16, 18, 0),
    lg == "W1" & season == 22 ~ c(11, 17, 0, 0),
    lg == "W2" & season == 22 ~ c(11, 19, 20, 0),
    str_detect(lg, "M") & season == 24 ~ c(34, 0, 0, 0),
    str_detect(lg, "W") & season == 24 ~ c(35, 0, 0, 0),
    TRUE ~ c(0, 0, 0, 0)
  )
  codes <- codes[codes != 0]
  
  data <- codes %>% map(pull_url) %>% bind_rows()
  
  if(season >= 24){
    data <- data %>% mutate(
      home = str_replace(home, "D", ""), away = str_replace(away, "D", "")
    )
  }
  
    data <- data %>% 
      separate(col = "date_w_day", into = c("Day", "Mo", "Date"), sep = " ") %>%
      select(-Day) %>% mutate(
        Mo = match(Mo, month.abb), Date = as.numeric(Date),
        Year = ifelse(Mo >= 8, 2000 + season - 1, 2000 + season),
        prop_date = paste(Year, Mo, Date, sep = "-")
      ) %>% select(-c(Date, Mo, Year)) %>% 
      filter(
        phase != "Regular" | 
          (str_detect(home, lg) & str_detect(away, lg))
      ) %>% 
      mutate(
        home = sub(".*? ", "", home),
        away = sub(".*? ", "", away),
        home = str_replace_all(home, c(" " = "_", "-" = "_")),
        away = str_replace_all(away, c(" " = "_", "-" = "_")),
       
      ) %>% filter(
        home != "TBD" & away != "TBD"
      )

  return(data)
}

get_ratings <- function(A, p, date){
  # Turn off prior at new year.
  if(date > as.Date("2023-12-31")){
    # Make numeric
    A <- A %>% filter(str_detect(status,"Final") & prop_date <= date) %>% mutate(
      homeG = as.numeric(homeG), awayG = as.numeric(awayG)
    )
    
    # Exclude post-regulation
    for(i in unique(c(A$home, A$away))){
      A[i] <- case_when(
        A$home == i ~ 1, 
        A$away == i ~ -1,
        TRUE ~ 0
      )
    }
    
    # Maximum of 12 scored
    A <- A %>% mutate(
      homeG = case_when(
        homeG > 12 ~ 12,
        (status == "Final OT" | status == "Final SO") & homeG > awayG ~ homeG - 1,
        TRUE ~ homeG
      ),
      awayG = case_when(
        awayG >12 ~ 12,
        (status == "Final OT" | status == "Final SO") & awayG > homeG ~ awayG - 1,
        TRUE ~ awayG
      )
    )
    
    # Scoring target vector(s)
    SH <- A$homeG; SA <- A$awayG
    
    # Mean goals for power rankings
    mean_Hgoals <- mean(A$homeG)
    mean_Agoals <- mean(A$awayG)
    
    # Home Ice vector
    HomeIce <- c(rep(1, sum(A$phase == "Regular")), rep(0, length(SH) - sum(A$phase == "Regular")), 
                 rep(-1, sum(A$phase == "Regular")), rep(0, length(SA) - sum(A$phase == "Regular")))
    
    # Create matrix
    A <- A %>% select(-c(game_id:prop_date))
    
    # Teams
    nteams <- ncol(A)
    
    # Full sparse matrix
    X <- as.matrix(cbind(HomeIce, 
                         rbind(A, matrix(0, length(SH), ncol(A), dimnames = 
                                           list(NULL, colnames(A)))),
                         rbind(matrix(0,length(SH), ncol(A), dimnames = 
                                        list(NULL, colnames(A))), A)))
    POISmatrix <- cbind(c(SH, SA), X) %>% data.frame()
    # Fit
    pois_mod <- glm(family = "poisson", data = POISmatrix, V1 ~ .)
    
    # Extract coefficients
    coefs <- pois_mod$coefficients 
    coefs[is.na(coefs)] <- 0
    homeEF <- coefs[2]; intercept <- coefs[1]
    sds <- sqrt(diag(vcov(pois_mod)))
    
    # Sort values
    team_ratings <- data.frame(
      Team = c(colnames(A)),
      Off_coef = coefs[3:(nteams + 2)],
      Def_coef = coefs[(nteams + 3):(2*nteams + 2)],
      sd_o = sds[3:(nteams + 2)],
      sd_d = sds[(nteams + 3):(2*nteams + 2)],
      row.names = NULL
    )
    
    # Set mean to 0
    adj_off <- 0 - mean(team_ratings$Off_coef)
    adj_def <- 0 - mean(team_ratings$Def_coef)
    
    team_ratings <- team_ratings %>% mutate(
      Off_adjed = Off_coef + adj_off, Def_adjed = Def_coef + adj_def
    )
    
    # guesstimate at baseline error
    team_ratings$sd_o[is.na(team_ratings$sd_o)] <- mean(team_ratings$sd_o, na.rm = T)
    team_ratings$sd_d[is.na(team_ratings$sd_d)] <- mean(team_ratings$sd_d, na.rm = T)
    
    # exit terms
    exit <- list()
    exit[["ratings"]] <- team_ratings
    colnames(exit[["ratings"]]) <- colnames(team_ratings)
    exit[["intercept"]] <- intercept
    exit[["homeEF"]] <- homeEF
    exit[["int_sd"]] <- sds[1]
    exit[["homeEF_sd"]] <- sds[2]
    exit[["mean_hG"]] <- mean_Hgoals
    exit[["mean_aG"]] <- mean_Agoals
    exit[["n"]] <- nrow(A)
    exit
  }
  
  # For first Semester
  else{
    # All games (for all teams)
    Z <- A %>% mutate(
      homeG = as.numeric(homeG), awayG = as.numeric(awayG)
    )
    
    # All unique teams
    Z_teams <- unique(c(Z$home, Z$away))
    
    # Completed games (Training set)
    A <- A %>% filter(str_detect(status,"Final") & prop_date <= date) %>% mutate(
      homeG = as.numeric(homeG), awayG = as.numeric(awayG)
    )
    
    # If season hasn't started
    if(nrow(A) == 0){
      # Set Up
      ratings <- data.frame(
        Team = Z_teams, Off_adjed = 0, Def_adjed = 0
      )
      
      # Set values according to Prior
      for(i in 1:nrow(ratings)){
        team_i <- ratings$Team[i]
        ratings$Off_adjed[i] <- ifelse(
          team_i %in% p$ratings$Team,
          0.8*p$ratings$Off_adjed[which(p$ratings$Team == team_i)],
          0)
        ratings$Def_adjed[i] <- ifelse(
          team_i %in% p$ratings$Team,
          0.8*p$ratings$Def_adjed[which(p$ratings$Team == team_i)],
          0)
      }
      
      # End it
      exit <- list(); 
      exit[["ratings"]] <- ratings
      exit[["intercept"]] <- p$intercept
      exit[["homeEF"]] <- p$homeEF
      exit[["mean_hG"]] <- p$mean_hG
      exit[["mean_aG"]] <- p$mean_aG
      return(exit)
    }
    
    else{
    # Teams in training set
    teams <- unique(c(A$home, A$away))
    # excluded baseline team
    last <- teams[length(teams)]
    # teams w/ prior
    returning_teams <- teams[teams %in% p$ratings$Team]
    # teams to set to 0
    new_teams <- teams[!(teams %in% returning_teams)]
    # Teams w/o games played
    excluded_teams <- Z_teams[!(Z_teams %in% teams)]
    
    # Teams needing prior (games played)
    prior <- data.frame(
      Team = c(teams, paste0(teams, ".1")),
      value = 0, var = 0
    ) 
    
    # set prior to 0 or 80% of rating & variance 
    for(i in 1:nrow(prior)){
      team_i <- prior$Team[i]
      prior$value[i] <- ifelse(str_remove(team_i, ".1") %in% new_teams, 0,
        ifelse(str_remove(team_i, ".1") %in% returning_teams & !str_detect(team_i, ".1"), 
          0.8*p$ratings$Off_adjed[which(p$ratings$Team == team_i)],
          0.8*p$ratings$Def_adjed[which(p$ratings$Team == str_remove(team_i, ".1"))]
      ))
      prior$var[i] <- ifelse(str_remove(team_i, ".1") %in% new_teams, 0.3^2,
        ifelse(str_remove(team_i, ".1") %in% returning_teams & !str_detect(team_i, ".1"), 
          (p$ratings$sd_o[which(p$ratings$Team == team_i)])^2,
          (p$ratings$sd_d[which(p$ratings$Team == str_remove(team_i, ".1"))])^2
      ))
    }
    
    # Order prior by appearance in dataset
    prior <- prior[match(c(teams, paste0(teams, ".1")), prior$Team),]
    
    # Intercept and Home Ice values
    more <- data.frame(Team = c("Intercept", "HomeIce"), 
                       value = c(p$intercept, p$homeEF), 
                       var = c(p$int_sd^2, p$homeEF_sd^2))
    
    prior <- bind_rows(more, prior)
    
    # create training set
    for(i in teams){
      A[i] <- case_when(
        A$home == i ~ 1, 
        A$away == i ~ -1,
        TRUE ~ 0
      )
    }
    
    A <- A %>% mutate(
      homeG = case_when(
        homeG > 12 ~ 12,
        (status == "Final OT" | status == "Final SO") & homeG > awayG ~ homeG - 1,
        TRUE ~ homeG
      ),
      awayG = case_when(
        awayG >12 ~ 12,
        (status == "Final OT" | status == "Final SO") & awayG > homeG ~ awayG - 1,
        TRUE ~ awayG
      )
    )
    
    SH <- A$homeG; SA <- A$awayG
    
    mean_Hgoals <- mean(A$homeG)
    mean_Agoals <- mean(A$awayG)
    
    HomeIce <- c(rep(1, sum(A$phase == "Regular")), rep(0, length(SH) - sum(A$phase == "Regular")), 
                 rep(-1, sum(A$phase == "Regular")), rep(0, length(SA) - sum(A$phase == "Regular")))
    
    A <- A %>% select(-c(game_id:prop_date))
    
    # Remove last team
    A <- A[,1:ncol(A) - 1]
    
    off_offset <- prior$value[(nrow(prior)/2 + 1)]
    def_offset <- prior$value[nrow(prior)]
    
    # Re-set prior to be relative to last team
    prior$value[3:(nrow(prior)/2 + 1)] <- prior$value[3:(nrow(prior)/2 + 1)] - prior$value[(nrow(prior)/2 + 1)]
    prior$value[(nrow(prior)/2 + 2):nrow(prior)] <- prior$value[(nrow(prior)/2 + 2):nrow(prior)] - 
      prior$value[nrow(prior)]
    

    
    # cut out last team from prior
    prior <- prior %>% filter(Team != last & Team != paste0(last, ".1"))
    
    # Number of teams
    nteams <- ncol(A)
    
    X <- as.matrix(cbind(HomeIce, 
                         rbind(A, matrix(0, length(SH), ncol(A), dimnames = 
                                           list(NULL, colnames(A)))),
                         rbind(matrix(0,length(SH), ncol(A), dimnames = 
                                        list(NULL, colnames(A))), A)))
    POISmatrix <- cbind(c(SH, SA), X) %>% data.frame() 
    
    print(dim(POISmatrix)); print(nteams); print(nrow(prior))
    
    # Formula and Fit
    fmla <- as.formula(paste("V1 ~ ", paste(colnames(POISmatrix)[2:ncol(POISmatrix)], collapse= "+")))
    
    fit <- sample_bpr(fmla, data = POISmatrix,
                      iter = 1000, 
                      pars = list(method = "MH"),
                      prior = list(type = "gaussian", b = prior$value, 
                                   B = diag(2*nteams+2)*prior$var))
    
    # Pull coefficients
    coefs <- summary(fit)$coefficients[,1] %>% as.numeric()
    ratings <- data.frame(Team = prior$Team[3:(nteams+2)], 
                          off = coefs[3:(nteams+2)], def = coefs[(nteams+3):(2+nteams*2)])
    rownames(ratings) <- NULL
    if(length(excluded_teams > 0)){
    # Re-reference the mean
    ratings <- bind_rows(ratings, data.frame(Team = last, off = 0, def = 0)) %>% mutate(
      Off_adjed = off + off_offset, Def_adjed = def + def_offset
    )
    }
    else{
      ratings <- bind_rows(ratings, data.frame(Team = last, off = 0, def = 0)) %>% mutate(
        Off_adjed = off - mean(off), Def_adjed = def - mean(off)
      )
    }
    #ADD MISSING TEAMS USING PRIOR -- Unjoined teams are new and set to average
    missing_ratings <- data.frame(Team = excluded_teams)
    missing_ratings <- missing_ratings %>% left_join(p$ratings, by = "Team") %>% 
      select(Team, Off_adjed, Def_adjed) %>% mutate(Off_adjed = 0.8*Off_adjed, Def_adjed = 0.8*Def_adjed)
    missing_ratings[is.na(missing_ratings)] <- 0
    
    # Combine
    ratings <- bind_rows(ratings, missing_ratings)
    # Adjust Mean
    ratings <- ratings %>% mutate(
      Off_adjed = Off_adjed - mean(Off_adjed), Def_adjed = Def_adjed - mean(Def_adjed)
    )
    
    # Update values of relevant numbers
    a_H <- p$n * p$mean_hG; b <- p$n
    a_A <- p$n * p$mean_aG
    n <- nrow(A)
    
    exit <- list()
    exit[["ratings"]] <- ratings
    exit[["intercept"]] <- coefs[1]
    exit[["homeEF"]] <- coefs[2]
    exit[["mean_hG"]] <- (b/(b + n))*(a_H/b) + (n/(n + b))*mean_Hgoals
    exit[["mean_aG"]] <- (b/(b + n))*(a_A/b) + (n/(n + b))*mean_Agoals
    exit
    }
  }
}

power_Rank <- function(ratings_list, date){
  require(tidyverse)
  require(stringr)
  
  ratings <- ratings_list[["ratings"]]
  homeG <- ratings_list[["mean_hG"]]
  awayG <- ratings_list[["mean_aG"]]
  
  # Set up Power Ranking to be saved: Date, numerical ratings + publishable values
  ratings %>% mutate(
    Date = date,
    Off_rating = exp(Off_adjed + log(homeG)), Def_rating = exp(Def_adjed + log(awayG)),
    `Power Rating` = round(Off_rating - Def_rating, 3),
    Rank = rank(-1*`Power Rating`, ties.method = "min"),
    `Offense Rank` = rank(-1*Off_adjed, ties.method = "min"),
    `Defense Rank` = rank(Def_adjed, ties.method = "min"), 
    Team = str_replace_all(Team, "_", " ")
  ) %>% select(Date, Rank, Team, `Power Rating`, `Offense Rank`, `Defense Rank`, 
               Off_rating, Def_rating) %>% arrange(Rank)
}

update_schedule <- function(lg, date){
  require(tidyverse)
  # Get Schedule
  live <- read_data(lg, 24) %>% mutate(game_id = as.numeric(game_id))
  
 # missing <- read_csv(paste0("Data/missing/missing_", lg, ".csv"))
  
  # for(i in 1:nrow(missing)){
  #   j <- which(live$game_id == missing$game_id[i])
  #   live$homeG[j] <- missing$homeG[i]
  #   live$awayG[j] <- missing$awayG[i]
  #   live$status[j] <- missing$status[i]
  # }
  
  # Update Schedule
  write_csv(live, paste0("Data/schedule/schedule_", lg, ".csv"))
}

update_ratings <- function(lg, date){
  # Get Schedule
  live <- read_data(lg, 24) %>% mutate(game_id = as.numeric(game_id))
  
  # Update Ratings
  p <- read_rds(paste0("Data/prior/prior_", lg, ".rds"))
  m <- get_ratings(live, p, date = date)
  write_rds(m, paste0("Data/ratings/ratings_", lg, ".rds"))
}

update_Power <- function(lg, date){
  require(tidyverse)
  stored <- read_csv(paste0("Data/power/power_", lg, ".csv"))
  ratings <- read_rds(paste0("Data/ratings/ratings_", lg, ".rds"))
  power <- power_Rank(ratings, date)
  new_power <- bind_rows(stored, power)
  path <- paste0("Data/power/power_", lg, ".csv")
  write_csv(new_power, path)
}

get_W_Su <- function(date){
  require(tidyverse)
  require(lubridate)
  # Get End Date (Next Sunday)
  sunday <- date + days(4)
  
  # Get data, filter days, add day col, add league col
  f <- function(lg){
    read_csv(paste0("Data/schedule/schedule_", lg, ".csv")) %>% 
      filter(prop_date > date & prop_date <= sunday) %>% 
      mutate(`Lg.` = lg, Day = wday(prop_date, label = T, week_start = 1)) %>% 
      select(`Lg.`, Day, home, away) %>% transmute(`Lg.` = `Lg.`, Day = Day, Home = home, Away = away)
  }
  weekend <- map(.x = c("M1", "W1", "M2", "M3", "W2"), f) %>% bind_rows()
  write_csv(weekend, "Data/weekend.csv")
}





