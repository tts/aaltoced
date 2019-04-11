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
             HTML("<p>Data: Aalto University CRIS, CrossRef Event Data, Twitter API. Fetched 2019-04-08</p>
                  <p><a href='https://blogs.aalto.fi/suoritin/2019/04/10/everyday-altmetrics/'>About</a></p>")
             ))
  
  
  body <- dashboardBody(
    fluidRow(
      box(
        width = 4, valueBoxOutput("toptweet", width = "100%")
        ),
      box(
        width = 4, dygraphOutput("timeseries", width = "100%", height = "150px")
        ),
      box(
        width = 4, valueBoxOutput("mediantweet", width = "100%")
      )),
    fluidRow(
      box(
        width = 4, valueBoxOutput("longestlastingtweet", width = "100%")
      ),
      box(
        width = 4, dygraphOutput("timeseries2", width = "100%", height = "150px")
      ),
      box(
        width = 4, valueBoxOutput("medianage", width = "100%")
      )),
    fluidRow(
      column(width = 12,
             height = "600px",
             DT::dataTableOutput("summary", 
                                 width = "100%",
                                 height = "600px"))
    )
    #, fluidRow(
    #   box(
    #     width = 1, downloadButton("data_aalto", "Download"))
    # )
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
  
  
  #------------
  #
  # Output
  #
  #------------

  output$toptweet <- renderValueBox({
    valueBox(
      value = "Top tweeted",
      ifelse(max(rgData()$Tweets_by_article) == 0, "-",
             paste0(rgData()[rgData()$Tweets_by_article == max(rgData()$Tweets_by_article), "title"][1], " (" , rgData()[rgData()$Tweets_by_article == max(rgData()$Tweets_by_article), "Tweets_by_article"][1], " times)")),
      icon = icon("twitter"),
      color = "orange",
      width = "100%",
      href = ifelse(max(rgData()$Tweets_by_article) == 0, "-", 
                    str_extract(rgData()[rgData()$Tweets_by_article == max(rgData()$Tweets_by_article), "Article"][1], "https://[^']+"))
    )
  })
  
  output$mediantweet <- renderValueBox({
    valueBox(
      value = "Median number of tweets",
      median(rgData()$Tweets_by_article),
      icon = icon("twitter"),
      color = "teal",
      width = "100%")
  })
  
  output$longestlastingtweet <- renderValueBox({
    valueBox(
      value = "Longest life span",
             paste0(rgData()[rgData()$`Life span (days)` == max(rgData()$`Life span (days)`), "title"][1], " (" , rgData()[rgData()$`Life span (days)` == max(rgData()$`Life span (days)`), "Life span (days)"][1], " days)"),
      icon = icon("twitter"),
      color = "navy",
      width = "100%",
      href = str_extract(rgData()[rgData()$`Life span (days)` == max(rgData()$`Life span (days)`), "Article"][1], "https://[^']+")
    )
  })
  
  output$medianage <- renderValueBox({
    valueBox(
      value = "Median life span", 
      paste0(median(rgData()$`Life span (days)`), " days"),
      icon = icon("twitter"),
      color = "light-blue",
      width = "100%"
    )
  })

  
  totable <- reactive({
    df <- rgData()
    
    df <- df %>% 
      rename(Tweets = Tweets_by_article) %>% 
      select(School, `Department or research area`, `Research group`, Year, Article, Link, `Screen name of (re)tweeter`, Description, Location, Followers, Tweet, Retweet, Date, `Life span (days)`)
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
  
  
  
  totimeseries2 <- reactive({
    
    ltweet <-  rgData()[rgData()$`Life span (days)` == max(rgData()$`Life span (days)`), ]
    
    if ( nrow(ltweet) >= 1 & ltweet$Tweets_by_article[1] != 0 ) {
      stats2 <- ltweet %>%
        mutate(date_col = date(Date)) %>%
        group_by(date_col) %>%
        summarize(value = n()) %>% 
        column_to_rownames(., "date_col")
      
      stats2.xts <- as.xts(stats2)
    }
    else return(NULL)
  })
  
  
  output$timeseries <- renderDygraph({
    if ( !is.null(totimeseries()) )
      dygraph(totimeseries()) 
  })
  
  output$timeseries2 <- renderDygraph({
    if ( !is.null(totimeseries2()) )
      dygraph(totimeseries2()) 
  })
  
  
  output$summary <- DT::renderDataTable({
    dat <- datatable(totable(), 
                     escape = FALSE, 
                     rownames = FALSE,
                     filter = "top",
                     options(list(pageLength = 50)))
    return(dat)
    
  })
  
  
  output$data_aalto = downloadHandler(
    filename = function() {
      filename = 'data_aalto.csv'
    },
    content = function(file) {
      {
        write.csv(rgData(), 'temp.csv', row.names = FALSE)
        file.rename('temp.csv', file)    
      } 
    }
  )
  
  
}


shinyApp(ui = ui, server = server)