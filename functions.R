require(httr)
require(stringr)
require(tidyverse)
require(jsonlite)
require(bpr)
require(extraDistr)
require(lubridate)

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
        code == 32 | code == 44 ~ "Regional",
        code <= 23 | code >= 34 ~ "Regular",
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
    str_detect(lg, "M") & season == 24 ~ c(34, 44, 0, 0),
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
        home != "TBD" & away != "TBD" & !str_detect(home, " Seed") & !str_detect(away, " Seed")
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

get_gh_full <- function(lg, m, s, cur_date){
  # Get & Clean
  lgs <- c("M1", "W1", "M2", "W2", "M3")
  all_leagues <- c("Men's Division 1", "Women's Division 1", "Men's Division 2", 
                   "Women's Division 2", "Men's Division 3")
  # lg <- lgs[which(league == all_leagues)]

    rtgs <- m
    sched <- s %>% 
      filter(prop_date <= cur_date & str_detect(status, "Final")) %>% 
      mutate(
        homeG = as.numeric(homeG), awayG = as.numeric(awayG),
        reg_homeG = ifelse(
          (status == "Final OT" | status == "Final SO") & homeG > awayG, homeG - 1, homeG
        ),
        reg_awayG = ifelse(
          (status == "Final OT" | status == "Final SO") & awayG > homeG, awayG - 1, awayG
        ),
        homePts = case_when(
          homeG > awayG & (status == "Final OT" | status == "Final SO") ~ 2,
          homeG > awayG & !(status == "Final OT" | status == "Final SO") ~ 3,
          homeG < awayG & (status == "Final OT" | status == "Final SO") ~ 1,
          homeG < awayG & !(status == "Final OT" | status == "Final SO") ~ 0,
          TRUE ~ 1.5
        )
      )
    
    sched$xHG <- 0; sched$xAG <- 0; sched$xHpts <- 0; sched$xApts <- 0; sched$xHGD <- 0
    sched$hwin <- 0; sched$awin <- 0
    
    for(i in 1:nrow(sched)){
      gp_i <- game_probs(sched$home[i], sched$away[i], rtgs)
      sched$xHG[i] <- gp_i$key_probs$xHomeG
      sched$xAG[i] <- gp_i$key_probs$xAwayG
      sched$xHGD[i] <- gp_i$key_probs$xHomeDiff
      xhpts <- 3*gp_i$key_probs$home_Rwin + 2*gp_i$key_probs$home_OTwin + gp_i$key_probs$away_OTwin
      sched$xHpts[i] <- xhpts; sched$xApts[i] <- 3 - xhpts
      sched$hwin[i] <- gp_i$key_probs$home_win; sched$awin[i] <- gp_i$key_probs$away_win
    }
    
    # Home
    home <- sched %>% transmute(
      Date = paste0(month(prop_date, label = T, abbr = T), " ", day(prop_date)),
      Team = str_replace_all(home, "_", " "), `Opp.` = str_replace_all(away, "_", " "), 
      GF = homeG, GA = awayG, Result = case_when(
        homeG > awayG & (status == "Final OT" | status == "Final SO") ~ "OT Win",
        homeG > awayG & !(status == "Final OT" | status == "Final SO") ~ "Win",
        homeG < awayG & (status == "Final OT" | status == "Final SO") ~ "OT Loss",
        homeG < awayG & !(status == "Final OT" | status == "Final SO") ~ "Loss",
        TRUE ~ "Tie"
      ),
      GFax = round(reg_homeG - xHG, 2), GAax = round(reg_awayG - xAG, 2),
      GDax = round(reg_homeG - reg_awayG - xHGD, 2),
      PTSax = round(homePts - xHpts, 3), `Win Prob.` = round(100*hwin, 1),
      xGF = round(xHG, 2), xGA = round(xAG, 2), xPts = round(xHpts, 3)
    )
    
    # Away
    away <- sched %>% transmute(
      Date = paste0(month(prop_date, label = T, abbr = T), " ", day(prop_date)),
      Team = str_replace_all(away, "_", " "), `Opp.` = str_replace_all(home, "_", " "), 
      GF = awayG, GA = homeG, Result = case_when(
        homeG > awayG & (status == "Final OT" | status == "Final SO") ~ "OT Loss",
        homeG > awayG & !(status == "Final OT" | status == "Final SO") ~ "Loss",
        homeG < awayG & (status == "Final OT" | status == "Final SO") ~ "OT Win",
        homeG < awayG & !(status == "Final OT" | status == "Final SO") ~ "Win",
        TRUE ~ "Tie"
      ),
      GFax = round(reg_awayG - xAG, 2), GAax = round(reg_homeG - xHG, 2),
      GDax = round(reg_awayG - reg_homeG + xHGD, 2),
      PTSax = round(3 - homePts - xApts, 3), `Win Prob.` = round(100*awin, 1),
      xGF = round(xAG, 2), xGA = round(xHG, 2), xPts = round(xApts, 3)
    )
    
    # Exit
    return(bind_rows(home, away) |> mutate(`Lg.` = lg))
    
}

update_ratings <- function(lg, date){
  # Get Schedule
  live <- read_data(lg, 24) %>% mutate(game_id = as.numeric(game_id))
  
  # Update Ratings
  p <- read_rds(paste0("Data/prior/prior_", lg, ".rds"))
  m <- get_ratings(live, p, date = date)
  
  # Update History
  h <- get_gh_full(lg, m, live, date)
  
  write_rds(m, paste0("Data/ratings/ratings_", lg, ".rds"))
  write_csv(h, paste0("Data/history/history_", lg, ".csv"))
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

game_probs <- function(home, away, rating_list, c = 0.01){
  require(extraDistr)
  require(tidyverse)
  
  intercept <- rating_list[["intercept"]]
  homeEF <- rating_list[["homeEF"]]
  ho_rate <- rating_list[["ratings"]]$Off_adjed[which(rating_list[["ratings"]]$Team == home)]
  ao_rate <- rating_list[["ratings"]]$Off_adjed[which(rating_list[["ratings"]]$Team == away)]
  hd_rate <- rating_list[["ratings"]]$Def_adjed[which(rating_list[["ratings"]]$Team == home)]
  ad_rate <- rating_list[["ratings"]]$Def_adjed[which(rating_list[["ratings"]]$Team == away)]
  
  # DF of score & state probabilities
  df <- data.frame(expand_grid(homeG = c(0:12), awayG =c(0:12)))
  a <- ho_rate + ad_rate + .5*homeEF + intercept
  b <- ao_rate + hd_rate -.5*homeEF + intercept
  df <- df %>% mutate(prob = extraDistr::dbvpois(homeG, awayG, exp(a), exp(b), c)) %>% 
    mutate(prob = prob/sum(prob))
  
  
  # summary of key probabilities
  key_probs <- df %>% mutate(
    home_Rwin = sum(prob*(homeG > awayG)),
    away_Rwin = sum(prob*(awayG > homeG)),
    OT = sum(prob*(awayG == homeG)),
    home_OTwin = ((0.5 + (home_Rwin/(1 - OT)))/2)*OT, 
    away_OTwin = ((0.5 + (away_Rwin/(1 - OT)))/2)*OT,
    home_win = home_Rwin + home_OTwin,
    away_win = away_Rwin + away_OTwin,
    xHomeDiff = sum(prob*(homeG - awayG)),
    xAwayDiff = -1*xHomeDiff,
    xHomeG = sum(prob*homeG),
    xAwayG = sum(prob*awayG)
  ) %>% select(home_Rwin:xAwayG) %>% head(1)
  
  # list that shit
  exit <- list()
  exit[["df"]] <- df
  exit[["key_probs"]] <- key_probs
  exit
}

