
library(shiny)
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
  # json2 <- gsub(' ', '%20', json, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

departmenttype <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "department_name")$department_name)

ledgercode <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "ledger_code")$ledger_code)

amount_city <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "amount")$amount)


# Define UI for application
ui <- navbarPage("Investing in Innovation Appplications NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("department_type_select",
                                          "Department Name",
                                          choices = departmenttype,
                                          selected = "OMI"),
                              checkboxGroupInput("ledger_code_select", 
                                                 "Ledger Code",
                                                 choices = ledgercode,
                                                 selected = c("5")),
                              sliderInput("Amount_select",
                                          "Amount",
                                          min = min(amount_city),
                                          max = max(amount_city),
                                          value = c(min(amount_city), max(amount_city)),
                                          step = 1),
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
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20department_name%20%3D%20%22", input$department_type_select, "%22%20AND%20ledger_code%20%3D%20%22", input$ledger_code_select, "%22%20")
    # url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$dates[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$dates[2], "%27%20AND%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27%20")
    # url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22department_name%22%20%3D%20%27", input$department_type_select, "%27%20AND%20%22ledger_code%22%20%3D%20%27", input$ledger_code_select, "%27%20AND%20%22amount%22%20%3D%20%27", input$Amount_select, "%27%20")
    dataCity <- ckanSQL(url)%>%
      # Slider Filter
      #filter(amount >= input$Amount_select[1] & amount <= input$Amount_select[2])
    # State Filter
    if (length(input$department_type_select) > 0 ) {
      dataCity <- subset(dataCity, department_name %in% input$department_type_select)
    }
    # Private Match Waiver Filter
    if (length(input$ledger_code_select)>0) {
      dataCity <- subset(dataCity, ledger_code %in% input$ledger_code_select)
    }
    return(dataCity)
  })
  
  # plot three figures
  output$plot_length <- renderPlotly({
    dat <- datainput()
    ggplotly(
      ggplot(data= dat, aes(x = amount, fill = department_name, text = paste0("<b>"))) + 
        geom_bar() + 
        ggtitle("Count by Award Length") +
        xlab("Award Length") +
        ylab("Count"),
      tooltip = "text")
  })
  output$plot_requested <- renderPlotly({
    dat <- datainput() 
    ggplotly(
      ggplot(data= dat, aes(x = fund_description, y = amount, fill = department_name)) + 
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
      ggplot(data= dat, aes(x = department_name, fill = amount)) + 
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
    
    subset(data, select = c(department_name, amount,ledger_code))
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
    updateSelectInput(session, "department_type_select", selected = "OMI")
    updateSliderInput(session, "Amount_select", value = c(min(amount_city), max(amount_city)))
    updateCheckboxGroupInput(session, "ledger_code_select", choices = ledgercode, selected = c("5"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

