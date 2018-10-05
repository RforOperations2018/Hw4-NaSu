library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(ggplot2)
library(plyr)
library(httr)
library(jsonlite)
library(htmltools)

#Load and transform Data
data <- read.csv("./Investingininnovation2010.csv", header = TRUE)

data.load <- transform(data, 
                  Award.Length = mapvalues(Award.Length, c(""), c(NA)))

mutate(data.load, Project.Title = as.character(Project.Title),
       Grant.Type = as.factor(Grant.Type),
       State = as.factor(State),
       Applicant = as.factor(Applicant))

# Define UI for applicatio
ui <- navbarPage("Investing in Innovation Appplications NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # State select
                              selectInput("StateSelect",
                                          "State:",
                                          choices = sort(unique(data.load$State)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("MA", "VA","AR")),
                              # checkbox Selection
                              checkboxGroupInput("Private.Match.WaiverSelect", 
                                                 "Private.Match.Waiver:",
                                                 choices = sort(unique(data.load$Private.Match.Waiver)),
                                                 selected = c("Yes", "No")),
                              # Award Requested Selection
                              sliderInput("AwardRequestedSelect",
                                          "Award Requested:",
                                          min = min(data.load$Award.Requested, na.rm = T),
                                          max = max(data.load$Award.Requested, na.rm = T),
                                          value = c(min(data.load$Award.Requested, na.rm = T), max(data.load$Award.Requested, na.rm = T)),
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
                            downloadButton("downloadData","Download Investing in Innovative Applications Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)


# Define server logic
server <- function(input, output, session = session) {
  # Filtered investing data
  iaInput <- reactive({
    data <- data.load %>%
      # Slider Filter
      filter(Award.Requested >= input$AwardRequestedSelect[1] & Award.Requested <= input$AwardRequestedSelect[2])
    # State Filter
    if (length(input$StateSelect) > 0 ) {
      data <- subset(data, State %in% input$StateSelect)
    }
    # Private Match Waiver Filter
    if (length(input$Private.Match.WaiverSelect)>0) {
      data <- subset(data, Private.Match.Waiver %in% input$Private.Match.WaiverSelect)
    }
    return(data)
  })

  # plot three figures
  output$plot_length <- renderPlotly({
    dat <- iaInput()
    ggplotly(
      ggplot(data= dat, aes(x = Award.Length, fill = Grant.Type, text = paste0("<b>"))) + 
        geom_bar() + 
        ggtitle("Count by Award Length") +
        xlab("Award Length") +
        ylab("Count"),
        tooltip = "text")
    })
  output$plot_requested <- renderPlotly({
    dat <- iaInput() 
    ggplotly(
      ggplot(data= dat, aes(x = Applicant, y = Award.Requested, fill = Award.Length , text = paste0("<b>", Applicant, ":</b>",
                                                                                                    "<br>State:",State,
                                                                                                    "<br>Award Requested:", Award.Requested
                                                                                                    ))) + 
        geom_point() +   theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank()) +
        ggtitle("Count by Award Requested") +
        xlab("Applicant") +
        ylab("Award Requested"),
      tooltip = "text") 
    })
  output$plot_requested2 <- renderPlotly({
    dat <- iaInput() 
    ggplotly(
      ggplot(data= dat, aes(x = Award.Requested, fill = Award.Length)) + 
        geom_histogram() +   theme(axis.title.x=element_blank(),
                               axis.text.x=element_blank(),
                               axis.ticks.x=element_blank()) +
        ggtitle("Count by Award Requested") +
        xlab("Award Requested") +
        ylab("Count")) 
  })
  # Data Table
  output$table <- DT::renderDataTable({
    data <- iaInput()
    
    subset(data, select = c(Applicant, City, State, Project.Title, Grant.Type, Award.Requested, Location))
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
      paste("investing-in-innovative-applications", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(swInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "StateSelect", selected = c("MA", "VA","AR"))
    updateSliderInput(session, "AwardRequestedSelect", value = c(min(data.load$Award.Requested, na.rm = T), max(data.load$Award.Requested, na.rm = T)))
    updateCheckboxGroupInput(session, "Private.Match.WaiverSelect", choices = sort(unique(data.load$Private.Match.Waiver)), selected = c("Yes", "No"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
