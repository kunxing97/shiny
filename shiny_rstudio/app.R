#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents"),
            plotOutput("distPlot"),
            plotOutput("lmPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    dataInput <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = "x",ylab = "y" )
        
        linearmodel <- lm(dataInput()$y ~ dataInput()$x, data=dataInput())
        
        output$lmPlot <- renderPlot({
            plot(dataInput()$x, dataInput()$y, 
                 abline(linearmodel), cex = 1.3,pch = 16,xlab = "x",ylab = "y" )
        })
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

