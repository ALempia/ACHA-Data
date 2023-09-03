source("app-functions.R")

library(gt)
library(shiny)
library(tidyverse)
library(extraDistr)
library(bslib)
library(fresh)
library(shinyWidgets)
library(lubridate)


lgs <- c("M1", "W1", "M2", "W2", "M3")
leagues <- c("Men's Division 1", "Women's Division 1", "Men's Division 2", 
             "Women's Division 2", "Men's Division 3")
cur_date <- Sys.Date()

# GET DATA FROM GITHUB
DATA <- get_data()

ui <- navbarPage("ACHA Analytics", fluid = TRUE,
                 use_theme(create_theme(
                   theme = "default",
                   bs_vars_navbar(
                     default_bg = "#be2033",
                     default_color = "#be2033",
                     default_link_color = "#FFFFFF",
                     default_link_active_color = "#0c233f"
                   ),
                   bs_vars_color(
                     gray_base = "#000000",
                     brand_primary = "#75b8d1",
                     brand_success = "#0c233f",
                     brand_info = "#0c233f",
                     brand_warning = "#d1ab75",
                     brand_danger = "#d175b8"
                   ),
                   bs_vars_state(
                     success_text = "#0c233f",
                     success_bg = "#FFF",
                     success_border = "#FFF",
                     info_text = "#FFF",
                     info_bg = "#be2033",
                     info_border = "#0c233f",
                     danger_text = "#FFF",
                     danger_bg = "#d175b8",
                     danger_border = "#d175b8"
                   ),
                   bs_vars_wells(
                     bg = "#FFF",
                     border = "#3f2d54"
                   )
                 )),
                 header = div(style = "font-size:16px; text-align:right; padding-right:10px",
                              "Created and Maintained by ",
                              tags$a(href = "https://www.twitter.com/AndersLempia",
                                     "Anders Lempia", target = "_blank")
                 ),
                 tags$head(tags$style("#pg4_sim{font-size: 20px;}")),
    tabPanel("Power Rankings",
             fluidRow(div(" ")),
             fluidRow(
               column(2,
               pickerInput("pg1_lgs", "League", choices = leagues)
               ),
               column(8, 
                      gt_output(outputId = "pg1_table")
                      )
             ), 
             ),
    tabPanel("Team Progress",
             fluidRow(
               column(3,
                      pickerInput("pg2_vars", "Measures to Plot", 
                                  choices = c("Power", "Offense", "Defense"),
                                  selected = "Power", multiple = T)
               ),
               column(3,
                      pickerInput("pg2_type", "Display Type", choices = c("Rank", "Rating"))
                      ),
               column(3, 
                      pickerInput("pg2_lgs", "League", choices = leagues)
               ),
               column(3,
                      selectInput("pg2_tm", "Team", choices = NULL)
               ),
             ), 
             fluidRow(
               column(1),
               column(10,
                      plotOutput("pg2_plot")
                      )
             ),
             fluidRow(
               column(1),
               column(10,
                      htmlOutput("pg2_dtip")
                      )
             )
             ),
    tabPanel("Weekend Probabilities", 
             fluidRow(
               column(2,
               pickerInput("pg3_lgs", "Leagues", choices = leagues, 
                           multiple = TRUE, selected = c("Men's Division 1", "Women's Division 1"))
               ),
               column(8, 
                      gt_output("pg3_tbl")
                      ),
               column(2,
                      pickerInput("pg3_days", "Day Range Start", 
                                  choices = c("Thursday", "Friday", "Saturday", "Sunday"),
                                  selected = ifelse(
                                    as.character(wday(Sys.Date(), label = T, abbr = F)) %in% c("Monday", "Tuesday", "Wednesday"),
                                                    "Thursday", as.character(wday(Sys.Date(), label = T, abbr = F))
                                                    ))
                      )
             ), 
             ),
    tabPanel("Matchup Simulator", 
             fluidRow(
               column(2,
                      selectInput("pg4_lgs", "League", choices = leagues)
                      ),
               column(3,
                      selectInput("pg4_away", "Away Team", choices = NULL)
                      ),
               column(3,
                      selectInput("pg4_home", "Home Team", choices = NULL)
                      ),
               column(3, style = "margin-top: 25px;",
                      actionButton("pg4_go1", label = "Get Projection", class = "btn-success")
                      ),
                column(1)
             ),
             fluidRow(
               column(2),
               column(4, 
                      gt_output("pg4_away")
                      ),
               column(4, 
                      gt_output("pg4_home")
               ),
               column(2)
             ),
             fluidRow(
               column(1),
               column(5,
                      plotOutput("pg4_away_plot")
                      ),
               column(5,
                      plotOutput("pg4_home_plot")
                      )
             ),
             fluidRow(
               column(1),
               column(10,
                      shinyWidgets::panel(
                        actionButton("pg4_go2", label = "GO", class = "btn-success"),
                        textOutput("pg4_sim"),
                        status = "info", heading = "Simulate Game")
                      )
             )
             ),
    tabPanel("Model Information",
             fluidRow(
               column(1),
               column(10,
                      htmlOutput("pg5_info1")
                      )
             ),
             fluidRow(
               column(1),
               column(10,
                      div(style = "font-size:14px; text-align:left;",
                          "(1)",
                          tags$a(href = "https://www.zeileis.org/news/poisson/",
                                 "Info on Poisson distribution and a similar regression", target = "_blank")
               )
             )
             ),
             fluidRow(
               column(1),
               column(10,
                      div(style = "font-size:14px; text-align:left;",
                          "(2)",
                          tags$a(href = "https://mnahabedian.substack.com/p/player-development-a-probabilistic",
                                 "A hockey-related intro to Bayesian analysis", target = "_blank"), " by ", 
                          tags$a(href = "https://twitter.com/hunterofstats",
                                 "Mikael Nahabedian", target = "_blank")
                      )
               )
             ),
             fluidRow(
               column(1),
               column(10,
                      div(style = "font-size:14px; text-align:left;",
                          "(3)",
                          tags$a(href = "http://www2.stat-athens.aueb.gr/~jbn/papers2/08_Karlis_Ntzoufras_2003_RSSD.pdf",
                                 "Analysis of sports data by using bivariate Poisson models", target = "_blank"), 
                          " by Dimitris Karlis and Ioannis Ntzoufras, 20003",
                          
                      )
               )
             ),
             fluidRow(
               column(1),
               column(10,
                      div(style = "font-size:14px; text-align:left;",
                          "(4)",
                          tags$a(href = "https://cran.r-project.org/web/packages/bpr/bpr.pdf",
                                 "D’Angelo, L. (2021), 'bpr: Bayesian Poisson regression'", target = "_blank")
                      )
               )
             ),
             fluidRow(
               column(1),
               column(10,
                      htmlOutput("pg5_info2")
               )
             ),
             fluidRow(
               column(1),
               column(10,
                      div(style = "font-size:14px; text-align:left;",
                          tags$a(href = "https://github.com/ALempia/ACHA-Data",
                                 "https://github.com/ALempia/ACHA-Data", target = "_blank")
                      )
               )
             ),
             fluidRow(
               column(1),
               column(10,
                      htmlOutput("pg5_info3")
                      )
             )
  )
)

server <- function(input, output, session) {
  # Page 1
  output$pg1_table <- render_gt(expr = make_pwrtbl(get_pwr(input$pg1_lgs, DATA), input$pg1_lgs))
  
  # Page 2
  pg2_lgs_react <- reactive({
    get_pwr(input$pg2_lgs, DATA)
  })
  observeEvent(pg2_lgs_react(), {
    choices <- unique(pg2_lgs_react()$Team) %>% sort()
    updateSelectInput(session, inputId = "pg2_tm", choices = choices)
  })
  
  output$pg2_plot <- renderPlot({
    make_prog_plot(input$pg2_lgs, input$pg2_tm, DATA, input$pg2_type, input$pg2_vars)
  })
  
  output$pg2_dtip <- renderUI({
    HTML(dtip)
  })
  
  # Page 3
  output$pg3_tbl <- render_gt(expr = make_wknd_tbl(input$pg3_lgs, input$pg3_days, DATA, cur_date))
  
  pg4_lgs_react <- reactive({
    get_rtg(input$pg4_lgs, DATA)
  })
  observeEvent(pg4_lgs_react(), {
    choices <- str_replace_all(unique(pg4_lgs_react()$ratings$Team), "_", " ") %>% sort()
    updateSelectInput(session, inputId = "pg4_away", choices = choices)
    updateSelectInput(session, inputId = "pg4_home", choices = choices)
  })
  
  observeEvent(input$pg4_go1, {
    output$pg4_away <- display_probs_away(str_replace_all(input$pg4_away, " ", "_"), 
                                           str_replace_all(input$pg4_home, " ", "_"),
                                           get_rtg(input$pg4_lgs, DATA)) %>% render_gt()
    output$pg4_home <- display_probs_home(str_replace_all(input$pg4_away, " ", "_"), 
                                          str_replace_all(input$pg4_home, " ", "_"),
                                          get_rtg(input$pg4_lgs, DATA)) %>% render_gt()
    output$pg4_away_plot <- away_dist(str_replace_all(input$pg4_away, " ", "_"), 
                                      str_replace_all(input$pg4_home, " ", "_"),
                                      get_rtg(input$pg4_lgs, DATA)) %>% renderPlot()
    output$pg4_home_plot <- home_dist(str_replace_all(input$pg4_away, " ", "_"), 
                                      str_replace_all(input$pg4_home, " ", "_"),
                                      get_rtg(input$pg4_lgs, DATA)) %>% renderPlot()
  })
  
  observeEvent(input$pg4_go2,{
    output$pg4_sim <- renderText(sim_game(str_replace_all(input$pg4_away, " ", "_"), 
                                              str_replace_all(input$pg4_home, " ", "_"),
                                              get_rtg(input$pg4_lgs, DATA)))
  })
  
  # Page 5
  output$pg5_info1 <- renderUI({
    HTML(info1)
  })
  
  output$pg5_info2 <- renderUI({
    HTML(info2)
  })
  
  output$pg5_info3 <- renderUI({
    HTML(line)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

