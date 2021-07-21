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
    titlePanel("Shiny App Linear Regression"),
    
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
                         selected = "head"), 
           
             # Making an action button 
            actionButton("submit", label = "Linear Model")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            plotOutput("distPlot"),
            h3("rsquared"),
            textOutput("rsquared"),
            h3("slope"),
            textOutput("slope"),
            h3("intercept"),
            textOutput("intercept"),
            plotOutput("lmPlot"), 
            # uiOutput(outputId = "information")
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
    
    linmod <-eventReactive(input$submit, {lm(dataInput()$y ~ dataInput()$x, data=dataInput())
    
    }) 
    
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, xlab = "x",ylab = "y", main = "Scatter Plot" )
    
    
    # output$information <- renderPrint({
    #     linearmodel <- lm(dataInput()$y ~ dataInput()$x, data=dataInput())
    #     summary(linearmodel)
    # })
        
    output$lmPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y, 
        abline(linmod()), cex = 1.3,pch = 16,xlab = "x",ylab = "y", main = "Linear Model")
        
    output$rsquared <- renderText({
        print(round(summary(linmod())$r.squared, 3))
    })
    
    output$slope <- renderText({
        print(coef(linmod())[2],2)
    })
    
    output$intercept <- renderText({
        print(coef(linmod())[1], 1)
    })
    
    # output$information <- renderPrint({
    #   summary(linmod())
    # })
       
        
    })
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

