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
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

departmenttype <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "department_name")$department_name)

ledgercode <- sort(ckanUniques("f61f6e8c-7b93-4df3-9935-4937899901c7", "ledger_code")$ledger_code)

# Define UI for application
ui <- navbarPage("Investing in Innovation Appplications NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("department_type_select",
                                          "Department Name",
                                          choices = departmenttype,
                                          selected = "URA Projects"),
                              checkboxGroupInput("ledger_code_select", 
                                                 "Ledger Code",
                                                 choices = ledgercode,
                                                 selected = c("5")),
                              sliderInput("Amount_select",
                                          "Amount",
                                          min = -100000,
                                          max = 100000,
                                          value = c(-100000, 100000),
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
  # datainput <- reactive({
  #   # Build API Query with proper encodes
  #   url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22department_name%22%20=%20%27", input$department_type_select, "%27%20AND%20%22object_account_description%22%20=%20%27", input$ledger_code_select, "%27%20AND%20%22amount%22%20=%20%27", input$Amount_select, "%27")
  #   
  #   # Load and clean data
  #   dataurl <- ckanSQL(url)
  #   dataCity <- dataurl %>%
  #     # Slider Filter
  #     filter(amount >= input$Amount_select[1] & amount <= input$Amount_select[2])
  #   # State Filter
  #   if (length(input$department_type_select) > 0 ) {
  #     dataCity <- subset(dataCity, department_name %in% input$department_type_select)
  #   }
  #   # Private Match Waiver Filter
  #   if (length(input$ledger_code_select)>0) {
  #     dataCity <- subset(dataCity, ledger_code %in% input$ledger_code_select)
  #   }
  #   return(dataCity)
  # })
  datainput <- reactive({
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22f61f6e8c-7b93-4df3-9935-4937899901c7%22%20WHERE%20%22department_name%22%20=%20%27", input$department_type_select, "%27%20AND%20%22object_account_description%22%20=%20%27", input$ledger_code_select, "%27%20AND%20%22amount%22%20=%20%27", input$Amount_select, "%27")

    # Load and clean data
    dataCity <- ckanSQL(url)%>%
      return(dataCity)
  #   dataCity <- dataurl %>%
  #     # Slider Filter
  #     filter(amount >= input$Amount_select[1] & amount <= input$Amount_select[2])
  #   # State Filter
  #   if (length(input$department_type_select) > 0 ) {
  #     dataCity <- subset(dataCity, department_name %in% input$department_type_select)
  #   }
  #   # Private Match Waiver Filter
  #   if (length(input$ledger_code_select)>0) {
  #     dataCity <- subset(dataCity, ledger_code %in% input$ledger_code_select)
  #   }
  #   return(dataCity)
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
      write.csv(swInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "department_type_select", selected = "URA Projects")
    updateSliderInput(session, "Amount_select", value = c(-100000, 100000))
    updateCheckboxGroupInput(session, "ledger_code_select", choices = ledgercode, selected = c("5"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}


# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
