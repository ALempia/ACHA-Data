library(tidyverse)
library(extraDistr)

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

get_data <- function(){
  exit <- list()
  
  # Schedule
  exit[["S_M1"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/schedule/schedule_M1.csv")
  exit[["S_M2"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/schedule/schedule_M2.csv")
  exit[["S_M3"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/schedule/schedule_M3.csv")
  exit[["S_W1"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/schedule/schedule_W1.csv")
  exit[["S_W2"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/schedule/schedule_W2.csv")
  
  # Power Rankings
  exit[["P_M1"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/power/power_M1.csv")
  exit[["P_M2"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/power/power_M2.csv")
  exit[["P_M3"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/power/power_M3.csv")
  exit[["P_W1"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/power/power_W1.csv")
  exit[["P_W2"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/power/power_W2.csv")
  
  # Ratings
  exit[["R_M1"]] <- read_rds("https://github.com/ALempia/ACHA-Data/raw/main/Data/ratings/ratings_M1.rds")
  exit[["R_M2"]] <- read_rds("https://github.com/ALempia/ACHA-Data/raw/main/Data/ratings/ratings_M2.rds")
  exit[["R_M3"]] <- read_rds("https://github.com/ALempia/ACHA-Data/raw/main/Data/ratings/ratings_M3.rds")
  exit[["R_W1"]] <- read_rds("https://github.com/ALempia/ACHA-Data/raw/main/Data/ratings/ratings_W1.rds")
  exit[["R_W2"]] <- read_rds("https://github.com/ALempia/ACHA-Data/raw/main/Data/ratings/ratings_W2.rds")
  
  # Weekend
  exit[["WKND"]] <- read_csv("https://raw.githubusercontent.com/ALempia/ACHA-Data/main/Data/weekend.csv")
  
  return(exit)
}

get_pwr <- function(league, DATA){
  lgs <- c("M1", "W1", "M2", "W2", "M3")
  leagues <- c("Men's Division 1", "Women's Division 1", "Men's Division 2", 
               "Women's Division 2", "Men's Division 3")
  lg <- lgs[which(leagues == league)]
  nm <- paste0("P_", lg)
  DATA[[nm]]
}

make_pwrtbl <- function(df, league){
  require(tidyverse)
  require(gt)
  
  lgs <- c("M1", "W1", "M2", "W2", "M3")
  leagues <- c("Men's Division 1", "Women's Division 1", "Men's Division 2", 
               "Women's Division 2", "Men's Division 3")
  
  cur_date <- max(df$Date); 
  prev_date <- ifelse(length(unique(df$Date)) > 1,
    max(df$Date[df$Date != cur_date]),
    0
  ) %>% as.Date(origin = .Date(0))
  
  max_rank <- nrow(df)
  
  form_date <- paste0(wday(cur_date, label = T, abbr = F), ", ", month(cur_date, label = T, abbr = F), " ", day(cur_date))
  titly <- paste0("ACHA ", league, " Power Rankings")
  
  if(prev_date == 0){
  output <- df %>% filter(Date == cur_date) %>% 
    mutate(Movement = 0, Change = 0) %>%
    select(Rank, Movement, Team, `Power Rating`, Change, `Offense Rank`, `Defense Rank`) %>% 
    gt() %>% data_color(
    columns = c(`Power Rating`),
    palette = "PuOr", domain = c(-1*max(abs(df$`Power Rating`)), max(abs(df$`Power Rating`)))
  ) %>% tab_header(
    title = titly,
    subtitle = paste0("As of ", form_date)
  ) 
  output %>% opt_interactive(use_compact_mode = T, use_page_size_select = T, 
                             use_highlight = T)
  }
  else{
    dfCUR <- df %>% filter(Date == cur_date) 
    prev_pwr <- df %>% filter(Date == ymd(prev_date)) %>% 
      transmute(
        Team = Team, prev_Rank = Rank, prev_Rating = `Power Rating`
      )
    df <- dfCUR %>% left_join(prev_pwr, by = "Team") 
    df <- df %>% mutate(
      prev_Rank = ifelse(is.na(prev_Rank), max(Rank), prev_Rank), 
      prev_Rating = ifelse(is.na(prev_Rating), 0, prev_Rating)
    ) %>% mutate(Movement = prev_Rank - Rank, Change = `Power Rating` - prev_Rating) %>%
      select(Rank, Movement, Team, `Power Rating`, Change, `Offense Rank`, `Defense Rank`) 
    output <- df %>% 
      gt() %>% data_color(
        columns = c(`Power Rating`),
        palette = "PuOr", domain = c(-1*max(abs(df$`Power Rating`)), max(abs(df$`Power Rating`)))
      ) %>% tab_header(
        title = titly,
        subtitle = paste0("As of ", form_date)
      ) %>%
      data_color(
        columns = Movement, palette = "RdBu",
        domain = c(-1*max(abs(df$Movement)), max(abs(df$Movement)))
      ) %>% 
      data_color(
        columns = Change, palette = "RdBu",
        domain = c(-1*max(abs(df$Change)), max(abs(df$Change)))
      )
    output %>% opt_interactive(use_compact_mode = T, use_page_size_select = T,
                               use_highlight = T, use_search = T)
  }
}

make_prog_plot <- function(league, team, DATA, type, variables){
  require(tidyverse)
  require(ggtext)
  
  df <- get_pwr(league, DATA) %>% filter(Team == team)
  
  form_date <- paste0(month(max(df$Date), label = T, abbr = F), " ", day(max(df$Date)))
  
  ex <- interval(min(df$Date), max(df$Date))
  
  
  if(type == "Rating"){
    x <- df %>% transmute(Date = Date, `Power Rating` = `Power Rating`, 
                          `Offense Rating` = Off_rating, `Defense Rating` = Def_rating) 
    minval <- min(c(x$`Power Rating`, x$`Offense Rating`, x$`Defense Rating`))
    maxval <- max(c(x$`Power Rating`, x$`Offense Rating`, x$`Defense Rating`))
    ranges <- ceiling(maxval) - floor(minval) 
    
    g <- ggplot(data = x, aes(x = Date)) + 
      ggtitle(label = paste0(team, " ACHA ", league, " Team Progression Through ", form_date)) + 
      theme(
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "gray15", linewidth = 0.2),
        panel.grid.minor.x = element_line(color = "gray", linewidth = 0.2),
        axis.ticks.x = element_line(color = "gray15", linewidth = 0.2), 
        panel.grid.major.y = element_line(color = "gray15", linewidth = 0.2),
        axis.ticks.y = element_line(color = "gray15", linewidth = 0.2),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(color = "black", size = 13), 
        axis.title = element_text(color = "black", size = 18),
        legend.position = "top",
      ) + xlab("Date of Power Ranking Calculation") + ylab("Rating") 
    
    
    if("Power" %in% variables){
      g <- g + geom_line(aes(y = `Power Rating`), color = "#f6c70f", linewidth = 2) + geom_point(aes(y = `Power Rating`), color = "#f6c70f", size = 5)
    }
    if("Defense" %in% variables){
      g <- g + geom_line(aes(y = `Defense Rating`), color = "#e20f0f", linewidth = 1.5, linetype = "dashed") + 
        geom_point(aes(y = `Defense Rating`), color = "#e20f0f", size = 4, shape = 17) 
    }
    if("Offense" %in% variables){
      g <- g + geom_line(aes(y = `Offense Rating`), color = "#1624c3", linewidth = 1.5, linetype = "dashed") + 
        geom_point(aes(y = `Offense Rating`), color = "#1624c3", size = 4, shape = 18)
    }
    g + coord_cartesian(ylim = c(floor(minval), ceiling(maxval))) + 
      annotate("label", x = ymd(int_start(ex) - days(7)), 
               y = ceiling(maxval) - (ranges/18), label = "POWER", color = "#f6c70f", fill = "#F3F3F3") + 
      annotate("label", x = ymd(int_start(ex) - days(7)), 
               y = ceiling(maxval) - (ranges/6), label = "OFFENSE", color = "#1624c3", fill = "#F3F3F3") + 
      annotate("label", x = ymd(int_start(ex) - days(7)), 
               y = ceiling(maxval) - (5*ranges/18), label = "DEFENSE", color = "#e20f0f", fill = "#F3F3F3") + 
      xlim(c(ymd(int_start(ex) - days(7)), ymd(int_end(ex) + days(7))))
  }
  
  else{
    x <- df %>% select(Date, Rank, `Offense Rank`, `Defense Rank`)
    minval <- min(c(x$Rank, x$`Offense Rank`, x$`Defense Rank`))
    maxval <- max(c(x$Rank, x$`Offense Rank`, x$`Defense Rank`))
    ranges <- maxval - minval + 2
    
    g <- ggplot(data = x, aes(x = Date)) + 
      ggtitle(label = paste0(team, " ACHA ", league, " Team Progression Through ", form_date)) +
      theme(
        panel.background = element_blank(), 
        panel.grid.major.x = element_line(color = "gray15", linewidth = 0.2),
        panel.grid.minor.x = element_line(color = "gray", linewidth = 0.2),
        axis.ticks.x = element_line(color = "gray15", linewidth = 0.2), 
        panel.grid.major.y = element_line(color = "gray15", linewidth = 0.2),
        axis.ticks.y = element_line(color = "gray15", linewidth = 0.2),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(color = "black", size = 13), 
        axis.title = element_text(color = "black", size = 18),
        plot.subtitle = element_markdown(size = 10)
      ) + xlab("Date of Power Ranking Calculation") + ylab("Rank") 
    
    
    if("Power" %in% variables){
      g <- g + geom_line(aes(y = Rank), color = "#f6c70f", linewidth = 2) + geom_point(aes(y = Rank), color = "#f6c70f", size = 6)
    }
    if("Defense" %in% variables){
      g <- g + geom_line(aes(y = `Defense Rank`), color = "#e20f0f", linewidth = 1.5, linetype = "dashed") + 
        geom_point(aes(y = `Defense Rank`), color = "#e20f0f", size = 4, shape = 17)
    }
    if("Offense" %in% variables){
      g <- g + geom_line(aes(y = `Offense Rank`), color = "#1624c3", linewidth = 1.5, linetype = "dashed") + 
        geom_point(aes(y = `Offense Rank`), color = "#1624c3", size = 4, shape = 18)
    }
    g + coord_cartesian(ylim = c(maxval + 1, minval - 1))+ 
      annotate("label", x = ymd(int_start(ex) - days(7)), 
               y = minval - 1 + (ranges/18), label = "POWER", color = "#f6c70f", fill = "#F3F3F3") + 
      annotate("label", x = ymd(int_start(ex) - days(7)), 
               y = minval - 1 + (ranges/6), label = "OFFENSE", color = "#1624c3", fill = "#f3f3f3") + 
      annotate("label", x = ymd(int_start(ex) - days(7)), 
               y = minval - 1 + (5*ranges/18), label = "DEFENSE", color = "#e20f0f", fill = "#f3f3f3") + 
      xlim(c(ymd(int_start(ex) - days(7)), ymd(int_end(ex) + days(7))))
  }
  
}

get_gh_full <- function(league, DATA, cur_date){
  # Get & Clean
  lgs <- c("M1", "W1", "M2", "W2", "M3")
  all_leagues <- c("Men's Division 1", "Women's Division 1", "Men's Division 2", 
                   "Women's Division 2", "Men's Division 3")
  lg <- lgs[which(league == all_leagues)]
  rtgs <- DATA[[paste0("R_", lg)]]
  sched <- DATA[[paste0("S_", lg)]] %>% 
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
  return(bind_rows(home, away))
}

make_gh_tbl <- function(tbl, team){
  
  max_val <- max(abs(c(tbl$GFax, tbl$GAax, tbl$GDax)), na.rm = T)
  
  if(team == "All"){
    tbl %>% gt() %>% opt_interactive(
      use_compact_mode = TRUE, use_pagination = TRUE, use_filters = FALSE, use_search = FALSE
    ) %>%
      tab_style(
        locations = cells_body(columns = Team),
        style = cell_text(weight = "bold")
      ) %>%
      data_color(
        columns = c(GFax, GDax),
        palette = "RdBu", 
        domain = c(-1*max_val, max_val)
      ) %>% 
      data_color(
        columns = PTSax,
        palette = "RdBu",
        domain = c(-3, 3)
      ) %>% 
      data_color(
        columns = GAax,
        palette = "RdBu",
        domain = c(-1*max_val, max_val),
        reverse = TRUE
      ) %>%
      cols_align(
        "left", columns = c(Date, Team, `Opp.`)
      ) %>% 
      cols_align(
        "center", columns = Result
      ) %>% 
      tab_header(
        title = "Game Results According to Current Ratings"
      )
  }
  
  else{
    tbl %>% filter(Team == team) %>%
      gt() %>% opt_interactive(
      use_compact_mode = TRUE, use_pagination = TRUE, use_filters = FALSE, use_search = FALSE
    ) %>%
      tab_style(
        locations = cells_body(columns = Team),
        style = cell_text(weight = "bold")
      ) %>%
      data_color(
        columns = c(GFax, GDax),
        palette = "RdBu", 
        domain = c(-1*max_val, max_val)
      ) %>% 
      data_color(
        columns = PTSax,
        palette = "RdBu",
        domain = c(-3, 3)
      ) %>% 
      data_color(
        columns = GAax,
        palette = "RdBu",
        domain = c(-1*max_val, max_val),
        reverse = TRUE
      ) %>%
      cols_align(
        "left", columns = c(Date, Team, `Opp.`)
      ) %>% 
      cols_align(
        "center", columns = Result
      ) %>% 
      tab_header(
        title = "Game Results According to Current Ratings"
      )
  }
}

make_wknd_tbl <- function(leagues, day, DATA, cur_date){
  require(lubridate)
  require(tidyverse)
  require(gt)
  
  today <- wday(cur_date, week_start = 1)
  
  lgs <- c("M1", "W1", "M2", "W2", "M3")
  all_leagues <- c("Men's Division 1", "Women's Division 1", "Men's Division 2", 
                   "Women's Division 2", "Men's Division 3")
  lgs_active <- lgs[which(all_leagues %in% leagues)]
  
  wkdys <- case_when(
    day == "Thursday" ~ c("Thu", "Fri", "Sat", "Sun"),
    day == "Friday" ~ c("Fri", "Sat", "Sun", ""),
    day == "Saturday" ~ c("Sat", "Sun", "", ""),
    day == "Sunday" ~ c("Sun", "", "", "")
  )
  
  end_date <- cur_date + days(7 - today)
  start_date <- case_when(
    day == "Thursday" ~ end_date - days(3),
    day == "Friday" ~ end_date - days(2),
    day == "Saturday" ~ end_date - days(1),
    day == "Sunday" ~ end_date
  )
  
  form_start_date <- paste0(day, ", ", month(start_date, label = T, abbr = F), " ", day(start_date))
  form_end_date <- paste0("Sunday", ", ", month(end_date, label = T, abbr = F), " ", day(end_date))
  
  wknd_og <- DATA$WKND %>% filter(Day %in% wkdys & `Lg.` %in% lgs_active)
  
  ## ADDED CHUNK - SPLIT - ADD PR - PASTE##
  wknd_m1 <- wknd_og %>% filter(`Lg.` == "M1") %>%
    left_join(
      filter(DATA[["P_M1"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Home" = "Team")) %>% 
    mutate(Home = paste0("#", Rank, "_", Home)) %>% 
    select(-c(Date:Def_rating)) %>%
    left_join(
      filter(DATA[["P_M1"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Away" = "Team")) %>% 
    mutate(Away = paste0("#", Rank, "_", Away)) %>% 
    select(-c(Date:Def_rating))
    
  wknd_m2 <- wknd_og %>% filter(`Lg.` == "M2") %>%
    left_join(
      filter(DATA[["P_M2"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Home" = "Team")) %>% 
    mutate(Home = paste0("#", Rank, "_", Home)) %>% 
    select(-c(Date:Def_rating)) %>%
    left_join(
      filter(DATA[["P_M2"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Away" = "Team")) %>% 
    mutate(Away = paste0("#", Rank, "_", Away)) %>% 
    select(-c(Date:Def_rating))
  
  wknd_m3 <- wknd_og %>% filter(`Lg.` == "M3") %>%
    left_join(
      filter(DATA[["P_M3"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Home" = "Team")) %>% 
    mutate(Home = paste0("#", Rank, "_", Home)) %>% 
    select(-c(Date:Def_rating)) %>%
    left_join(
      filter(DATA[["P_M3"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Away" = "Team")) %>% 
    mutate(Away = paste0("#", Rank, "_", Away)) %>% 
    select(-c(Date:Def_rating))
  
  wknd_w1 <- wknd_og %>% filter(`Lg.` == "W1") %>%
    left_join(
      filter(DATA[["P_W1"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Home" = "Team")) %>% 
    mutate(Home = paste0("#", Rank, "_", Home)) %>% 
    select(-c(Date:Def_rating)) %>%
    left_join(
      filter(DATA[["P_W1"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Away" = "Team")) %>% 
    mutate(Away = paste0("#", Rank, "_", Away)) %>% 
    select(-c(Date:Def_rating))
  
  wknd_w2 <- wknd_og %>% filter(`Lg.` == "W2") %>%
    left_join(
      filter(DATA[["P_W2"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Home" = "Team")) %>% 
    mutate(Home = paste0("#", Rank, "_", Home)) %>% 
    select(-c(Date:Def_rating)) %>%
    left_join(
      filter(DATA[["P_W2"]], Date == max(Date)) %>% mutate(
        Team = str_replace_all(Team, " ", "_")
      ), 
      by = c("Away" = "Team")) %>% 
    mutate(Away = paste0("#", Rank, "_", Away)) %>% 
    select(-c(Date:Def_rating))
  
  wknd <- bind_rows(wknd_m1, wknd_w1, wknd_m2, wknd_w2, wknd_m3) %>% arrange(
    factor(Day, levels = wkdys[wkdys != ""]), factor(`Lg.`, levels = lgs)
  )
  wknd_og <- wknd_og %>% arrange(
    factor(Day, levels = wkdys[wkdys != ""]), factor(`Lg.`, levels = lgs)
  )
  ## END ADDED CHUNK ##
  
  if(today < 3 | nrow(wknd) == 0){
    data.frame(
      `Lg.` = c("."), Day = c("."), Awaytm = c("."), AwayxG = c("."),
      Awaypct = c("."), Hometm = c("."), HomexG = c("."), Homepct = c(".")
    ) %>% filter(Day != ".")  %>% gt() %>% 
      cols_label(
        Awaytm = "Team", AwayxG = "xScore", Awaypct = "%", 
        Hometm = "Team", HomexG = "xScore", Homepct = '%',
      ) %>% 
      tab_spanner(
        label = "Away",
        columns = c(AwayxG, Awaytm, Awaypct)
      ) %>% 
      tab_spanner(
        label = "Home",
        columns = c(Homepct, Hometm, HomexG)
      ) %>% 
      tab_source_note(
        source_note = "Games will appear on Wednesday; see Matchup Simulator tab for custom matchups."
      ) %>% 
      tab_header(
        title = "Upcoming ACHA Games"
      )
  }
  else{
  
  wknd$AG = rep(0, nrow(wknd)); wknd$HG <- wknd$AG; wknd$Apct <- wknd$AG; wknd$Hpct <- wknd$AG
  
  for(i in 1:nrow(wknd)){
    rate = paste0("R_", wknd$`Lg.`[i])
    wknd$AG[i] <- game_probs(wknd_og$Home[i], wknd_og$Away[i], DATA[[rate]])$key_probs$xAwayG
    wknd$HG[i] <-  game_probs(wknd_og$Home[i], wknd_og$Away[i], DATA[[rate]])$key_probs$xHomeG
    wknd$Apct[i] <- game_probs(wknd_og$Home[i], wknd_og$Away[i], DATA[[rate]])$key_probs$away_win
  }
  
  wknd <- wknd %>% 
    mutate(Hpct = 1 - Apct, 
                          Away = str_replace_all(Away, "_", " "),
                            Home = str_replace_all(Home, "_", " "),
                          AG = round(AG, 2), HG = round(HG, 2),
                          Apct = round(100*Apct, 1), Hpct = round(100*Hpct, 1)
                            ) %>% 
    arrange(factor(Day, levels = wkdys[wkdys != ""]), factor(`Lg.`, levels = lgs))
  
  y <- wknd %>% transmute(
    `Lg.` = factor(`Lg.`, levels = lgs), Day = Day, `Away xGoals` = AG, Awaytm = Away, `Away %` = Apct, 
    `Home %` = Hpct, Hometm = Home, `Home xGoals` = HG
  ) %>% gt() %>% cols_align("center", c("Lg.", "Day", "Away %", "Home %")) %>% 
    cols_align("right", c("Awaytm")) %>% cols_align("left", c("Home xGoals")) %>%
    cols_label(
      `Away %` = "%", `Home %` = "%", Awaytm = "Team", Hometm = "Team", 
      `Away xGoals` = "xScore", `Home xGoals` = "xScore"
    ) %>% 
    tab_style(
      locations = cells_body(columns = c(Hometm, Awaytm)),
      style = cell_text(weight = "bold")
    ) %>% 
    data_color(
      columns = c(`Away %`, `Home %`),
      palette = "RdBu",
      domain = c(0, 100)
    ) %>% 
    data_color(
      columns = c(`Away xGoals`, `Home xGoals`),
      palette = "Greens",
      domain = c(0, 12)
    ) %>% 
    data_color(
      columns = `Lg.`,
      palette = c("#e34c37", "#5dbed3", "#05a188", "#3c5487", "#ecc31a")
    ) %>%
    tab_spanner(
      label = "Away", 
      columns = c(`Away xGoals`, Awaytm, `Away %`),
      level = 1
    ) %>% 
    tab_spanner(
      label = "Home",
      columns = c(`Home %`, `Hometm`, `Home xGoals`),
      level = 1
    ) %>% 
    tab_header(
      title = "Upcoming ACHA Games",
      subtitle = paste0(form_start_date, " to ", form_end_date)
    ) #%>%
  #  opt_interactive(use_compact_mode = T, use_page_size_select = T,
  #                        use_highlight = T, use_search = T)
  
  #y %>% 
  
  y 
  }
}

# wknd <- data.frame(
#   `Lg.` = c("M1", "M2", "M3", "W1", "W2"),
#   Day = c("Thu", "Fri", "Sat", "Sat", "Sun"),
#   Home = c("Niagara_University", "Denison_University", "Illinois_State_University", 
#            "University_of_Michigan", "The_Ohio_State_University"),
#   Away = c("University_of_Pittsburgh", "Ohio_University", "Drake_University", 
#            "Penn_State_University", "Miami_University")
# )

get_rtg <- function(league, DATA){
  lgs <- c("M1", "W1", "M2", "W2", "M3")
  leagues <- c("Men's Division 1", "Women's Division 1", "Men's Division 2", 
               "Women's Division 2", "Men's Division 3")
  lg <- lgs[which(leagues == league)]
  nm <- paste0("R_", lg)
  DATA[[nm]]
}

display_probs_away <- function(away, home, ratings, cur_power){
  require(tidyverse)
  require(gt)
  
  max_rank <- nrow(cur_power)
  
  df <- game_probs(home, away, ratings)$key_probs
  df <- df %>% bind_cols(cur_power %>% filter(Team == str_replace_all(away, "_", " ")))
  df %>% transmute(
    rank = Rank, O = `Offense Rank`, D = `Defense Rank`,
    reg_loss = round(100*home_Rwin, 1) %>% paste0("%"),
    ot_loss = round(100*home_OTwin, 1) %>% paste0("%"),
    ot_win = round(100*away_OTwin, 1) %>% paste0("%"),
    reg_win = round(100*away_Rwin, 1) %>% paste0("%"),
    xpts = round(home_OTwin + 2*away_OTwin + 3*away_Rwin, 3),
    xG = round(xAwayG, 2), pct = round(100*away_win, 1)
      ) %>% gt() %>% 
    cols_align("center") %>% 
    cols_label(
      rank = "Rank", O = "O. Rk.", D = "D. Rk.",
      reg_win = "3 pts.", ot_win = "2 pts.", ot_loss = "1 pt.", reg_loss = "0 pts.", xpts = "xPts.",
      xG = "xScore", pct = "Win %"
    ) %>% 
   # tab_spanner(
   #   columns = c(reg_loss, ot_loss, ot_win, reg_win),
   #   label = "3-2-1-0 Pt. Outcome Likelihoods"
   # ) %>% 
    tab_header(
      title = str_replace_all(away, "_", " ")
    ) %>% 
    data_color(
      columns = xpts, palette = "Purples", domain = c(0, 3)
    ) %>% 
    data_color(
      columns = xG, palette = "Greens", domain = c(0, 12)
    ) %>% data_color(
      columns = pct, palette = "RdBu", domain = c(0, 100)
    ) %>% data_color(
      columns = c(rank, O, D), palette = "RdBu", domain = c(max_rank, 1),
      reverse = T
    )
}

display_probs_home <- function(away, home, ratings, cur_power){
  require(tidyverse)
  require(gt)
  
  max_rank <- nrow(cur_power)
  
  df <- game_probs(home, away, ratings)$key_probs
  df <- df %>% bind_cols(cur_power %>% filter(Team == str_replace_all(home, "_", " ")))
  df %>% transmute(
    D = `Defense Rank`, O = `Offense Rank`, rank = Rank, 
    reg_win = round(100*home_Rwin, 1) %>% paste0("%"),
    ot_win = round(100*home_OTwin, 1) %>% paste0("%"),
    ot_loss = round(100*away_OTwin, 1) %>% paste0("%"),
    reg_loss = round(100*away_Rwin, 1) %>% paste0("%"),
    xpts = round(away_OTwin + 2*home_OTwin + 3*home_Rwin, 3),
    xG = round(xHomeG, 2), pct = round(100*home_win, 1)
  ) %>% gt() %>% 
    cols_align("center") %>% 
    cols_label(
      pct = "Win %", xG = "xScore", xpts = "xPts.", 
      reg_loss = "0 pts.", reg_win = "3 pts.", ot_win = "2 pts.", ot_loss = "1 pt.",
      D = "D. Rk.", O = "O. Rk.", rank = "Rank", 
    ) %>% 
   # tab_spanner(
   #   columns = c(reg_win, ot_win, ot_loss,  reg_loss),
   #   label = "3-2-1-0 Pt. Outcome Likelihoods"
   # ) %>% 
    cols_move_to_start(
      c(pct, xG, xpts, reg_win, ot_win, ot_loss, reg_loss)
    ) %>%
    tab_header(
      title = str_replace_all(home, "_", " ")
    ) %>% 
    data_color(
      columns = xpts, palette = "Purples", domain = c(0, 3)
    ) %>% 
    data_color(
      columns = xG, palette = "Greens", domain = c(0, 12)
    ) %>% 
    data_color(
      columns = pct, palette = "RdBu", domain = c(0, 100)
    ) %>% data_color(
      columns = c(rank, O, D), palette = "RdBu", domain = c(max_rank, 1),
      reverse = T
    )
}

away_dist <- function(away, home, ratings){
  require(tidyverse)
  
  df <- game_probs(home, away, ratings)$df
  xG <- game_probs(home, away, ratings)$key_probs$xAwayG
  
  dfother <- df %>% group_by(homeG) %>% summarize(prob = 100*sum(prob) %>% round(4))
  
  df <- df %>% group_by(awayG) %>% summarize(prob = 100*sum(prob) %>% round(4))
  
  top_val <- max(c(dfother$prob, df$prob)) + 5
  
  df %>% mutate(odd = (awayG %% 2 == 0)) %>%
    ggplot(mapping = aes(x = awayG, y = prob)) + geom_col(aes(fill = odd)) + 
    geom_vline(xintercept = xG, color = "#75b8d1", linetype = "dashed") + 
    scale_fill_manual(values = c("#be2033", "#0c233f")) +
    annotate("label", x = xG + 1, 
             y = top_val - 2, label = "xScore", color = "#75b8d1", fill = "#0c233f") + 
    theme(
      panel.background = element_blank(), 
      panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
      panel.grid.minor.y = element_line(color = "gray50", linewidth = 0.2),
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      axis.text = element_text(color = "black", size = 13), 
      axis.title = element_text(color = "black", size = 18)
    ) + 
    labs(
      title = paste0(str_replace_all(away, "_", " "), "'s Expected Score Distribution"),
      x = "Goals", y = "Probability (%)"
        ) + ylim(c(0, top_val)) + 
    scale_x_continuous(breaks = seq(0, 12, by = 1))
}

home_dist <- function(away, home, ratings){
  require(tidyverse)
  
  df <- game_probs(home, away, ratings)$df
  xG <- game_probs(home, away, ratings)$key_probs$xHomeG
  
  dfother <- df %>% group_by(awayG) %>% summarize(prob = 100*sum(prob) %>% round(4))
  
  df <- df %>% group_by(homeG) %>% summarize(prob = 100*sum(prob) %>% round(4))
  
  top_val <- max(c(dfother$prob, df$prob)) + 5
  
  df %>% mutate(odd = (homeG %% 2 == 0)) %>%
    ggplot(mapping = aes(x = homeG, y = prob)) + geom_col(aes(fill = odd)) + 
    geom_vline(xintercept = xG, color = "#75b8d1", linetype = "dashed") + 
    scale_fill_manual(values = c("#be2033", "#0c233f")) +
    annotate("label", x = xG + 1, 
             y = top_val - 2, label = "xScore", color = "#75b8d1", fill = "#0c233f") + 
    theme(
      panel.background = element_blank(), 
      panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
      panel.grid.minor.y = element_line(color = "gray50", linewidth = 0.2),
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      axis.text = element_text(color = "black", size = 13), 
      axis.title = element_text(color = "black", size = 18)
    ) + 
    labs(
      title = paste0(str_replace_all(home, "_", " "), "'s Expected Score Distribution"),
      x = "Goals", y = "Probability (%)"
    )  + ylim(c(0, top_val)) + 
    scale_x_continuous(breaks = seq(0, 12, by = 1))
}

sim_game <- function(away, home, ratings){
  require(tidyverse)
  df <- game_probs(home, away, ratings)$df %>% mutate(id = row_number())
  key_probs <- game_probs(home, away, ratings)$key_probs
  
  homeot <- key_probs$home_OTwin/(key_probs$home_OTwin + key_probs$away_OTwin)
  
  sim <- sample_n(df, 1, weight = df$prob)
  
  if(sim$homeG > sim$awayG){
    exit <- paste0(
      str_replace_all(home, "_", " "), " wins, ", as.character(sim$homeG), "-",
      as.character(sim$awayG), ", in regulation."
    )
  }
  if(sim$homeG < sim$awayG){
    exit <- paste0(
      str_replace_all(away, "_", " "), " wins, ", as.character(sim$awayG), "-",
      as.character(sim$homeG), ", in regulation."
    )
  }
  if(sim$homeG == sim$awayG){
    win <- sample(c("home", "away"), 1, prob = c(homeot, 1 - homeot))
    team <- ifelse(win == "home", home, away) %>% str_replace_all("_", " ")
    goals <- sim$homeG
    exit <- paste0(
      team, " wins, ", as.character(goals + 1), "-",
      as.character(goals), ", in overtime."
    )
  }
  return(exit)
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

current_power <- function(DATA, league, cur_date){
  
  cur_rate <- get_rtg(league, DATA)
  
  cur_power <- power_Rank(cur_rate, cur_date)
  return(cur_power)
}

info1 <- '<h4> Each 0% means <0.05%. Nothing is certainly impossible, least of all in this sport. </h4>

<h2> Calculation </h2> 
<p> 
The basic model used to evaluate teams is a Poisson regression model, which provides estimates for an intercept, 
the effect of home ice advantage, and the isolated offensive and defensive effect on scoring for each team.
</p> 
<p>
However, such a regression is only used to evaluate teams beginning in the second semester. During the fall semester,
I use a Bayesian approach. For the unfamiliar, this essentially means I use an estimate based on previous seasons for team strength. 
As more observations are obtained (games are played), the estimates are initially strongly biased toward the <i>prior</i>. As more games 
are played, then the <i>posterior</i> estimates are weighted more toward the <i>likelihood</i> of the observed data. More technically, 
I use an algorithm known as Gibbs Sampling, with a Gaussian prior. These are obtained daily throughout the season.
</p>
<p>
An important note: any team new to their league this year is assigned a weaker estimate of being perfectly average both offensively 
and defensively. Their ratings should approach their "true" values more quickly than other teams, at the cost of being slightly more volatile. 
Also of note, because the distribution of offensive and defensive ratings are different, their net power ratings appear non-zero. 
</p>
<p>
To obtain game probabilities from these numbers I use a Bivariate Poisson model. This model assigns a probability for each 
potential 60-minute score from 0-0 to 12-12 based on the supplied team strengths, intercept, and home ice advantage. 
The chance of winning in OT/SO when the game is tied is a weighted average of the regulation winning percentage and 50%. 
</p>
<p>
Power Rankings are also obtained from these estimates. They are converted into Goals Versus Average, where the posted Power Rating
can be interpreted as the expected goal differential in a game versus a hypothetical average team. When viewing a Team Progress plot with 
Rating selected as the Display Type, the Offense rating represents expected goals scored in such a game, higher being better. Defense 
represents expected goals allowed in such a game, lower being better. Power is simply the difference between these values. Power Rankings 
update each Wednesday. 
</p>
<hr>
<h2>Resources and Citations</h2>
'

info2 <- '
<hr>
<h2>Github Page</h2>
<p>
(Most of) the code I used to build the models and this app is available in a single github repository, which can be found at:
</p>
'

line <- '
<hr>
<p>
Good Luck to all of the athletes, coaches, staff, and fans in the ACHA this season!
</p>
'

dtip <- '
<hr>
<p>Note: A lower Defense Rating is better. Higher Offense and Power are better.</p>
'





























