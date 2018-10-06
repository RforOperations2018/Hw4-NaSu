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

ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}


ledgercode1 <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "ledger_code")$ledger_code)

amount_city1 <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "amount")$amount)

funddescription1 <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "fund_description")$fund_description)



# Define UI for application
ui <- navbarPage("City Wide Revenues and Expenses NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # select
                              selectInput("funddescription_select",
                                          "fund description:",
                                          choices = funddescription1,
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("GENERAL FUND","SENIOR CITIZENS PROG TF")),
                              # checkbox Selection
                              checkboxGroupInput("ledgercode_select", 
                                                 "Ledgercode:",
                                                 choices = ledgercode1,
                                                 selected = c("4", "5")),
                              # Slider Selection
                              sliderInput("amount_select",
                                          "Amount:",
                                          min = min(amount_city1, na.rm = T),
                                          max = max(amount_city1, na.rm = T),
                                          value = c(min(amount_city1, na.rm = T), max(amount_city1, na.rm = T)),
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
  #get wanted columns to form dataframe
  departmenttype <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "department_name")$department_name)
  
  ledgercode <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "ledger_code")$ledger_code)
  
  amount_city <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "amount")$amount)
  
  funddescription <- sort(ckanColumn("f61f6e8c-7b93-4df3-9935-4937899901c7", "fund_description")$fund_description)
  # So, for this assignment the idea is to pass your filters into the function that calls for the data. Instead what you've done is loaded each column separately. Look at the code for the ckanSQL() function from the inclass example and you should see that you did not need to call each column individually. Your app mostly works however, so you do not get a failing grade for functionality.
  #Retrieve all field values 
  data_all <- data.frame(departmenttype,ledgercode,amount_city,funddescription)
  # Filtered investing data
  dataInput <- reactive({
    data <- data_all %>%
      # Slider Filter
      filter(amount_city >= input$amount_select[1] & amount_city <= input$amount_select[2])
    #  Filter
    # These should be in your URL generation
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
    updateSliderInput(session, "amount_select", value = c(min(amount_city1, na.rm = T), 
                                                          max(amount_city1, na.rm = T)))
    updateCheckboxGroupInput(session, "funddescription_select", choices = funddescription1,
                             selected = c("	GENERAL FUND","SENIOR CITIZENS PROG TF"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}




# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

