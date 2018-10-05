library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(reshape2)
library(data.table)
library(DT)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

#connect to API and get columns function
ckanColumn <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20%22", field, "%22%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

#get wanted columns to form dataframe
departmenttype <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "department_name")$department_name)

ledgercode <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "ledger_code")$ledger_code)

amount_city <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "amount")$amount)

funddescription <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "fund_description")$fund_description)

data_all <- data.frame(departmenttype,ledgercode,amount_city,funddescription)

# Define UI for application
ui <- navbarPage("City Wide Revenues and Expenses NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # select
                              selectInput("funddescription_select",
                                          "fund description:",
                                          choices = sort(unique(data_all$funddescription)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("GENERAL FUND","SENIOR CITIZENS PROG TF")),
                              # checkbox Selection
                              checkboxGroupInput("ledgercode_select", 
                                                 "Ledgercode:",
                                                 choices = sort(unique(data_all$ledgercode)),
                                                 selected = c("4", "5")),
                              # Slider Selection
                              sliderInput("amount_select",
                                          "Amount:",
                                          min = min(data_all$amount_city, na.rm = T),
                                          max = max(data_all$amount_city, na.rm = T),
                                          value = c(min(data_all$amount_city, na.rm = T), max(data_all$amount_city, na.rm = T)),
                                          step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              fluidRow(
                                plotlyOutput("plot1")
                              ),
                              fluidRow(
                                plotlyOutput("plot2")
                              ),
                              fluidRow(
                                plotlyOutput("plot3")
                              )))),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download City Wide Revenues and Expenses Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)


# Define server logic
server <- function(input, output, session = session) {
  # Filtered investing data
  dataInput <- reactive({
    data <- data_all %>%
      # Slider Filter
      filter(amount_city >= input$amount_select[1] & amount_city <= input$amount_select[2])
    #  Filter
    if (length(input$ledgercode_select) > 0 ) {
      data <- subset(data, ledgercode %in% input$ledgercode_select)
    }
    #  Filter
    if (length(input$funddescription_select)>0) {
      data <- subset(data, funddescription %in% input$funddescription_select)
    }
    return(data)
  })
  # plot three figures
  output$plot1 <- renderPlotly({
    dat <- dataInput()
    ggplotly(
      ggplot(data= dat, aes(x = funddescription, fill = departmenttype)) + 
        geom_bar() + theme(axis.title.x=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank()) +
        ggtitle("Count by Fund description") +
        xlab("funddescription") +
        ylab("Count"))
  })
  output$plot2 <- renderPlotly({
    dat <- dataInput() 
    ggplotly(
      ggplot(data= dat, aes(x = ledgercode, y = amount_city)) + 
      geom_boxplot() + theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank()) +
        ggtitle("Ledgercode and Amount ") +
        xlab("Ledgercode") +
        ylab("Amount"))
  })
  output$plot3 <- renderPlotly({
    dat <- dataInput() 
    ggplotly(
      ggplot(data= dat, aes(amount_city)) + 
        geom_histogram() +   theme(axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank()) +
        ggtitle("Count by Amount") +
        xlab("Amount") +
        ylab("Count")) 
  })
  # Data Table
  output$table <- DT::renderDataTable({
    data <- dataInput()
    
    subset(data, select = c(departmenttype,ledgercode,amount_city,funddescription))
  })
  
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("City Wide Revenues and Expenses", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(swInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "ledgercode_select", selected = c("4", "5"))
    updateSliderInput(session, "amount_select", value = c(min(data_all$amount_city, na.rm = T), 
                                                          max(data_all$amount_city, na.rm = T)))
    updateCheckboxGroupInput(session, "funddescription_select", choices = sort(unique(data_all$funddescription)),
                             selected = c("	GENERAL FUND","SENIOR CITIZENS PROG TF"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}




# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
