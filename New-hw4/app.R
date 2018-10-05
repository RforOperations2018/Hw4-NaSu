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

ckanUnique <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

currentdelq <- sort(ckanUnique("ed0d1550-c300-4114-865c-82dc7c23235b", "current_delq")$current_delq)
# statedescription <- sort(ckanUnique("ed0d1550-c300-4114-865c-82dc7c23235b", "state_description")$state_description)
prioryears <- sort(ckanUnique("ed0d1550-c300-4114-865c-82dc7c23235b", "prior_years")$prior_years)

# Define UI for application
ui <- navbarPage("Investing in Innovation Appplications NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("current_delq_select",
                                          "Current Delq",
                                          choices = currentdelq,
                                          selected = "0.04"),
                              # checkboxGroupInput("state_description_select", 
                              #                    "state description",
                              #                    choices = statedescription,
                              #                    selected = c("Residential")),
                              checkboxGroupInput("prior_years_select",
                                                 "prior years",
                                                 choices = prioryears,
                                                 selected = c(1)),
                              # sliderInput("prior_years_select",
                              #             "prior years",
                              #             min = min(prioryears),
                              #             max = max(prioryears),
                              #             value = c(min(prioryears), max(prioryears)),
                              #             step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            # Output plot
                            mainPanel(
                              fluidRow(
                                plotlyOutput("plot_length")
                              ),
                              fluidRow(
                                plotlyOutput("plot_requested")
                              ),
                              fluidRow(
                                plotlyOutput("plot_requested2")
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
server <- function(input, output,session = session) {
  datainput <- reactive({
    # Build API Query with proper encodes
    #url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22ed0d1550-c300-4114-865c-82dc7c23235b%22%20WHERE%20%22current_delq%22%20=%20%27", input$current_delq_select,"%27%20AND%20%22prior_years%22%20%3D%20%27", input$prior_years_select, "%27%20")
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22ed0d1550-c300-4114-865c-82dc7c23235b%22%20WHERE%20%22current_delq%22%20=%20%27", 
                  input$current_delq_select,"%27%20AND%20%22prior_years%22%20%3D%20%27", input$prior_years_select, "%27%20")
    dataCity <- ckanSQL(url) %>%
    # Slider Filter
    #filter(prior_years >= input$prior_years_select[1] & prior_years <= input$prior_years_select[2])
    #Filter
    if (length(input$current_delq_select) > 0 ) {
      dataCity <- subset(dataCity, current_delq %in% input$current_delq_select)
      }
    # Filter
    # if (length(input$statedescription_select)>0) {
    #   dataCity <- subset(dataCity, state_description %in% input$statedescription_select)
    # }
    #Filter
    # if (length(input$prior_years_select)>0) {
    #   dataCity <- subset(dataCity, prior_years %in% input$prior_years_select)
    # }
    print(colnames(dataCity))
    return(dataCity)
    
  })
  
  # plot three figures
  output$plot_length <- renderPlotly({
    dat <- datainput()
    ggplotly(
      ggplot(data= dat, aes(x = prior_years)) + 
        geom_bar() + 
        ggtitle("Count by Award Length") +
        xlab("Award Length") +
        ylab("Count"))
  })
  output$plot_requested <- renderPlotly({
    dat <- datainput() 
    ggplotly(
      ggplot(data= dat, aes(x = current_delq, y = prior_years)) + 
        geom_point() +   theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank()) +
        ggtitle("Count by Award Requested") +
        xlab("Applicant") +
        ylab("Award Requested")) 
  })
  output$plot_requested2 <- renderPlotly({
    dat <- datainput() 
    ggplotly(
      ggplot(data= dat, aes(x = prior_years, fill = current_delq)) + 
        geom_histogram() +   theme(axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank()) +
        ggtitle("Count by Award Requested") +
        xlab("Award Requested") +
        ylab("Count")) 
  })
  # Data Table
  output$table <- DT::renderDataTable({
    data <- datainput()
    
    subset(data, select = c(prior_years,current_delq))
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
      paste("City expense", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(dataInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "current_delq_select", selected = "0.04")
    # updateSliderInput(session, "Amount_select", value = c(min(amount_city), max(amount_city)))
    # updateCheckboxGroupInput(session, "statedescription_select", choices = statedescription, selected = c("Residential"))
    updateCheckboxGroupInput(session, "prior_years_select", choices = prioryears, selected = c(1))
    showNotification("You have successfully reset the filters", type = "message")
  })
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

