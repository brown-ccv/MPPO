library(shiny)
library(sortable)
library(tidyverse)
library(data.table)
library(htmltools)
library(plotly)

################################################################################
# Runners
################################################################################
times <- list("Mile" = seq(1, 26),
              "The Ringer" = rep(6, 26), 
              "The Sprinter" = c(rep(4, 5), rep(Inf, 21)),
              "The Slow-and-Steady" = map(1:26, \(x) 10 + 0.2*x) %>% unlist,
              "The Walker" =  rep(20, 26), 
              "The Leg Cramp" = c(rep(7,3), rep(8, 3), rep(9, 2), seq(10,14), rep(15, 13)),
              "The Team Captain" = c(rep(7,2), rep(8, 3),  seq(8,10,0.25),
                                     rep(10, 5), seq(10, 12,0.5), rep(12, 2)),
              "The Bathroom Break" = c(rep(6, 3), 10, 60, rep(15, 21)),
              "The Beginner" = map(1:26, \(x) round(-9*(x/19-1)^2+19)) %>% unlist,
              "The Flame Out" = map(1:26, \(x) 5 + 0.5*x) %>% unlist,
              "The Procrastinator" = c(rep(14, 13), rep(7, 13))) %>% as.data.table 

setkey(times, "Mile")
runnames <- colnames(times)[-1]
n_runners <- seq(1, 5)
ui <-  fluidPage(
    title = "Relay Race",
    h2("MPA 2460: Relay Race Exercise"),
    br(),
    tabsetPanel(
      tabPanel(
        div("Team"),
        fluidRow(column(width = 4,
                        h4("Runner Speeds"),
                        tableOutput("teamrunners") 
                        ),
                 column(width = 4,
                        h4("Team Speed"),
                        tableOutput("relayrunners")
                        ),
                 column(width = 4,
                        h4("Team Info"),
                        selectInput(
                          inputId = "chooseteam",
                          label = 'Set Team Members',
                          choices = runnames,
                          selected = NULL,
                          multiple = TRUE, width = "100%"),
                        actionButton("setteam", "Set Team", width = "100%"),
                        br(),
                        br(),
                        uiOutput("teamrelay"),
                        br(),
                        h4("Team Performance"),
                        textOutput("finishtime"),
                        plotOutput("team_timeplot")
                        ),
                 tags$style(
                   HTML("
                      .rank-list-container.default-sortable.shiny-bound-input {
                        margin:0;
                        padding:0 !important;
                        border: 0px;
                      }
                      .default-sortable .rank-list {
                        border: 1px;
                        margin:0;
                        padding:0 !important;
                      }
                      .bucket-list-container.default-sortable .rank-list-container {
                          padding: 0px;
                          margin: 0px;
                          display: flex;
                          flex-flow: column nowrap;
                          flex-direction: row;
                          align-items: center;
                          width:100%;
                      } 

                      .bucket-list-container.default-sortable {
                        margin:0;
                        padding:0  !important;
                        border: 0px;
                        width:100%;
                      }
                      .bucket-list-container.default-sortable .rank-list-item {
                        background-color: #BDB;
                        margin:0;
                        padding:0  !important;
                        border: 1px black;
                        overflow-y: auto;
                      }
                    ")
                 )
              )
          ),
      tabPanel(
        div("Runners"),
        tableOutput("allrunners") 
        ),
      tabPanel(
        div("Marathon"),
        fluidRow(column(width = 8,
                        h4("Marathon Race"),
                        plotlyOutput("teamrace"),
                        h4("Marathon Results"),
                        plotOutput("teams_time_by_mile"),
                        plotOutput("teams_speed_by_mile")
                        ),
                 column(width = 4,
                       fileInput("relayfile", "Upload Relay Teams",
                                 accept = c("text/csv",
                                            "text/comma-separated-values,
                               .csv")),    
                       h4("Scoreboard"),
                       tableOutput("scoreboard")
                       )
                )
              )
      )
)


max_item_opts <- sortable_options(
  group = list(
    name = "mygroup",
    swap = TRUE,
    multiDrag = TRUE,
    put = htmlwidgets::JS("
      function(to) {
        // only allow a 'put' if there is less than 5 child already
        return to.el.children.length < 1;
      }
    ")
  )
)

makeranklist <- function(i){
  tags$tr(
    tags$td(align = "center", style = "width: 20%; border: 0.5px solid black;",  
            paste("# ", i)),
    tags$td(align = "left", style = "width: 60%; border: 0.5px solid black; overflow-y: auto;",
            rank_list(
              labels = NULL,
              input_id = str_c("rank_", i),
              options = max_item_opts,
              class = c("default-sortable", "custom-sortable")
              )
            ),
    tags$td(align = "left", style = "width: 20%; border: 0.5px solid black;",
           numericInput(inputId = str_c("milerun_",i), NULL, 0, min = 0, max = 26))
  )
}

getmtype <- function(id){
  gsub("TM .*: ", "", id)
}

addrelay <- function(runner, miles){
  if (miles > 0){
    times[1:miles, .(Runner = runner, Speed = get(getmtype(runner)))]
  }
  else {
    times[.0, .(Runner = runner, Speed = get(getmtype(runner)))]
  }
}

server <- function(input,output, session) {
  team_members <- reactiveVal()
  team_types <- reactiveVal()
  team_timetab <- reactiveVal()
  selectrunner_n <- reactiveVal(0)
  
  data <- reactive({
    infile <- input$relayfile
    if (is.null(infile)) {
      return(NULL)
    }
    fread(infile$datapath)
  })
  
observeEvent(input$chooseteam, {
  selected_values <- input$chooseteam
  if (!is.null(selected_values)){
  names(selected_values) <- gsub("\\..*", "", selected_values)
  selectrunner_n(selectrunner_n() + 1)
  new_choices <- paste(runnames, selectrunner_n(), sep = ".")
  names(new_choices) <- runnames
  all_choices <- c(selected_values, new_choices)
  } else {
    all_choices <- runnames
  }
  updateSelectInput(session, "chooseteam",
                    choices = all_choices,
                    selected = isolate(input$chooseteam))
}, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  relayfunc <- function(row){
    newtab <- times[1:as.numeric(row$runnermile),.(Speed = get(row$runner), Runner = row$runner)]
  }
  
  clean_googleform <- function(dataset){
    names(dataset) <- trimws(sub(":.*", "", names(dataset)))
    data <- melt(dataset[ , grepl("Relay Runner|Team name", names(dataset)), with=FALSE], id.vars = "Team name")
    data$value <- trimws(gsub("<[^>]*>", "", data$value))
    data <- data[, c("order", "type") := as.data.table(str_match(data$variable, "Relay Runner (\\d) (.*)")[, 2:3])]
    data <- dcast(data, `Team name` + order ~ type) 
    data <- data[,.(Team = `Team name`, order=as.numeric(order), runnermile = as.numeric(`Miles to Complete`), runner = type)]
    setkey(data, "Team")
    data[is.na(runnermile), runnermile := 0][runner %in% runnames,]
  }
  
  
  processteamfile <- function(file){
      clean_googleform(file) %>%
      split(by=c("Team")) %>% 
      lapply(\(x) x[, .(out = list(relayfunc(.SD))), by=order]$out %>% 
               rbindlist %>%
               .[1:26, .(Mile = 1:26, Runner, Speed, Time = cumsum(Speed))]) %>%
      rbindlist(, idcol = "Team") 
  }
  
  teamdata <- reactive({req(data())
                        processteamfile(data())})
  output$checkfile <- renderTable({data()})
  output$scoreboard <- renderTable({endtimes <- teamdata()[Mile == 26,][order(Time)] 
                                    endtimes[,.(Rank = rank(endtimes$Time, ties.method = "min"), 
                                                Team, "Finish Time" = checktime(Time))]})
  
  interpolate <- function(df, framelist){
    approx(x=c(1, df$Time, framelist[length(framelist)]), y = c(0, df$Mile, max(df$Mile)), xout = framelist)
  }
  
  output$teamrace <- renderPlotly({framelist <-  seq(1, round(max(teamdata()$Time[is.finite(teamdata()$Time) & !is.na(teamdata()$Time)]) + 100, 
                                                              digits = -2), 3)
                                  teamdata()[is.finite(Time),.(Time = unlist(framelist), 
                                                Mile = unlist(interpolate(.SD, framelist)$y)), by=Team] %>%
                                  plot_ly(x = ~Mile, y = ~Team, text = ~Team, 
                                          mode = "markers", 
                                          hoverinfo = "none",
                                          showlegend = FALSE,
                                          textposition = NULL,
                                          marker = list(size = 10))  %>% 
                                  layout(transition = list(duration = 0), 
                                         xaxis = list(
                                           title = "Miles",
                                           tickvals = seq(0,26,1)
                                         ),
                                         yaxis = list(
                                           title = "Teams",
                                           categoryorder = "array",
                                           categoryarray = sort(unique(teamdata()$Team), decreasing = TRUE)  
                                        )) %>%
                                  add_markers(color = ~Team, frame = ~Time, ids = ~Team) %>%
                                  animation_opts(60, easing = "elastic", redraw = FALSE)})
  
  output$teams_time_by_mile <- renderPlot({
    ggplot(teamdata(), aes(x=Time, y = Mile, color = Team)) + 
      geom_line() + ylab("Miles") + xlab("Time Elapsed") + geom_hline(aes(yintercept = 26), linewidth = 1) +
      scale_y_continuous(limits=c(1, 26), breaks = 1:26, expand = c(0,0.5)) +
      geom_vline(data = teamdata()[Mile == 26,], 
                 aes(xintercept = Time, col = Team), 
                 linetype = "dashed", show.legend = FALSE)  +
      theme_bw()
  })
  
  output$teams_speed_by_mile <- renderPlot({
    ggplot(teamdata(), aes(x=Mile, y=Speed, color = Team)) + 
      geom_point() + geom_line() + scale_x_continuous(limits=c(1, 26), breaks = 1:26, expand = c(0,0.5)) +
      xlab("Miles") + ylab("Speed") + theme_bw()
  })

  observeEvent(input$setteam, {
    chooseteam_clean <- gsub("\\..*", "", input$chooseteam)
    team_members(map(1:length(input$chooseteam), \(x) str_c("TM ", x, ": ", chooseteam_clean[x])) %>% unlist)
    team_types(chooseteam_clean %>% unlist %>% unique())
  })
  output$allrunners <- renderTable(times, striped = TRUE)
  output$teamrunners <- renderTable(times[,c("Mile", team_types()), with=FALSE])
  output$relayrunners <- renderTable({
                         tabs <- addrelay("Mile", 0)
                         for (i in n_runners){
                           runner <- input[[str_c("rank_",i)]]
                           miles <- input[[str_c("milerun_", i)]]
                           if (length(runner) != 0 & length(miles)!=0){
                             tabs <- rbind(tabs, addrelay(runner, miles))
                           }
                         }
                         team_timetab(tabs[1:26, .(Mile = 1:26, Runner, Speed, Time = cumsum(Speed))])
                         team_timetab()
                        })
  
  checktime <- function(finmin){
    if_else((!is.numeric(finmin)|!is.finite(finmin)), 
            "Did Not Finish.",
            str_c(dminutes(finmin) %>% seconds_to_period, " (", finmin, " minutes)"))
  }
  
  output$finishtime <- renderText({str_c("Total Time: ", checktime(team_timetab()[26, Time]))})

  output$team_timeplot <- renderPlot({
    req(team_timetab)
    ggplot(team_timetab(), aes(Mile, Time)) + 
      geom_line() +  scale_x_continuous(limits=c(1, 26), breaks = 1:26, expand = c(0,0.5)) +
      xlab("Miles") + ylab("Total Time") + theme_bw()
  })

  output$teamrelay <- renderUI({
                      tagList(
                      tags$div(rank_list(
                        text = "Unassigned Runners",
                        labels = team_members(),
                        input_id = "selectlist",
                        options = sortable_options(group = "mygroup")
                      )),
                      tags$br(),
                      tags$table(
                          style = "width: 100%; border: none; table-layout: fixed; margin:0px; padding:0px",
                          tags$tr(
                            tags$td(align = "center", style = "width: 20%; border: 0.5px solid black;",
                                    "Order"),
                            tags$td(align = "left", style = "width: 60%; border: 0.5px solid black; overflow-y: auto;",
                                    "Runner"),
                            tags$td(align = "left", style = "width: 20%; border: 0.5px solid black;", "Miles Run")
                          ),
                          lapply(n_runners, function(x) {
                            makeranklist(x)
                          })
                        )
                      )
                  })
}

shinyApp(ui = ui, 
         server = server)
