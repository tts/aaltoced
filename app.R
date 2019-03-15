library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(xts)
library(dygraphs)

data <- as.data.frame(readRDS("dataforapp_w_stats.rds"), stringsAsFactors = FALSE)

schools <- unique(data[, "School"])

ui <- function(request) {
  
  sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
      selectInput(inputId = "schools",
                  label = "Top level",
                  choices = c("All", sort(schools)),
                  multiple = FALSE,
                  selected = NULL),
      selectInput(inputId = "depts",
                  label = "Department",
                  choices = NULL,
                  multiple = FALSE,
                  selected = NULL),
      selectInput(inputId = "rgs",
                  label = "Research group",
                  choices = NULL,
                  multiple = FALSE,
                  selected = NULL)
    ),
    tags$div(class="form-group shiny-input-container", 
             HTML("<p>Altmetrics event data by Crossref as of 2019-02-17</p>
                  <p><a href='https://blogs.aalto.fi/suoritin/'>About (to do)</a></p>")
             ))
  
  
  body <- dashboardBody(
    fluidRow(
      column(width = 6,
            valueBoxOutput("toptweet")),
      column(width = 6,
             dygraphOutput("timeseries", height = "100px"))
      ),
    fluidRow(
      column(width = 12,
             height = "600px",
             DT::dataTableOutput("summary", 
                                 width = "100%",
                                 height = "600px"))
    )
  )
  
  
  dashboardPage(
    dashboardHeader(title = "Tweets on Aalto University publications 2017-", titleWidth = "800"),
    sidebar,
    body,
    skin = "black"
  )
  
}

server <- function(input, output, session) {
  
  #-------------------------------------
  #
  # Top level
  # 
  # Schools, Centres of Excellence,
  # University level
  #
  #-------------------------------------
  
 schoolData <- reactive({
    if ( input$schools == "All" ) return(data) else data[data$School %in% input$schools, ]
  })

  #--------------------
  #
  # Department level
  #
  #--------------------
  
  observe (
    updateSelectInput(session, 
                      inputId = 'depts', 
                      choices = if ( input$schools == "All" ) "Select top level" else c("All", sort(unique(schoolData()$`Department or research area`)))
    )
  )
  
  deptData <- reactive({
    if ( is.null(input$depts) || input$depts == "Select top level" || input$depts == "All") {
      return(schoolData())
    }
    isolate(schoolData() %>% filter( `Department or research area` %in% input$depts ))
    
  })
  
  #----------------------
  #
  # Research group level
  #
  #----------------------
  
  observe(
    updateSelectInput(session,
                      inputId = 'rgs',
                      choices = if ( input$depts == "Select top level" || input$depts == "All") "Select department" else c("All", sort(unique(deptData()$`Research group`)))
    )
  )

  rgData <- reactive({
    if ( is.null(input$rgs) || input$rgs == "Select department" || input$rgs == "All") {
      return(deptData())
    }
    isolate(deptData() %>%  filter(`Research group` %in% input$rgs))

  })
  
 
  # This seem non-interesting values in this app
  #
  # output$stats_school <- renderInfoBox({
  #   infoBox(
  #     "Tweets at top level",
  #    paste0(rgData()$Tweets_by_school[1], " (",  rgData()$Tweets_article_ratio_school[1], ")"),
  #    icon = icon("twitter"),
  #    width = NULL
  #   )
  # })
  # 
  # output$stats_dept <- renderInfoBox({
  #   infoBox(
  #     "Tweets at department level",
  #     paste0(rgData()$Tweets_by_dept[1], " (", rgData()$Tweets_article_ratio_dept[1], ")"),
  #     icon = icon("twitter"),
  #     color = "olive",
  #     width = NULL
  #   )
  # })
  # 
  # output$stats_rg <- renderInfoBox({
  #   infoBox(
  #     "Tweets at research group level",
  #     paste0(rgData()$Tweets_by_rg[1], " (",  rgData()$Tweets_article_ratio_rg[1], ")"),
  #     icon = icon("twitter"),
  #     color = "orange",
  #     width = NULL
  #   )
  # })
  

  output$toptweet <- renderValueBox({
    valueBox(
      value = "Top tweeted",
      ifelse(max(rgData()$Tweets_by_article) == 0, "-",
             paste0(rgData()[rgData()$Tweets_by_article == max(rgData()$Tweets_by_article), "title"][1], " (" , rgData()[rgData()$Tweets_by_article == max(rgData()$Tweets_by_article), "Tweets_by_article"][1], " times)")),
      icon = icon("twitter"),
      color = "orange",
      width = NULL,
      href = ifelse(max(rgData()$Tweets_by_article) == 0, "-", 
                    str_extract(rgData()[rgData()$Tweets_by_article == max(rgData()$Tweets_by_article), "Article"][1], "https://[^']+"))
    )
  })
  
  
  
  totable <- reactive({
    df <- rgData()
    
    df <- df %>% 
      rename(Tweets = Tweets_by_article) %>% 
      select(School, `Department or research area`, `Research group`, Year, Article, Tweet, Date)
  })

  
  
  totimeseries <- reactive({
    
    ttweet <-  rgData()[rgData()$Tweets_by_article == max(rgData()$Tweets_by_article),]
    
    if ( nrow(ttweet) >= 1 & ttweet$Tweets_by_article[1] != 0 ) {
      stats <- ttweet %>%
        mutate(date_col = date(Date)) %>%
        group_by(date_col) %>%
        summarize(value = n()) %>% 
        column_to_rownames(., "date_col")
      
      stats.xts <- as.xts(stats)
    }
    else return(NULL)
  })
  
  
  
  output$timeseries <- renderDygraph({
    if ( !is.null(totimeseries()) )
      dygraph(totimeseries()) 
  })
  
  
  
  output$summary <- DT::renderDataTable({
    dat <- datatable(totable(), 
                     escape = FALSE, 
                     rownames = FALSE,
                     filter = "top",
                     options(list(pageLength = 50)))
    return(dat)
    
  })
  
}


shinyApp(ui = ui, server = server)