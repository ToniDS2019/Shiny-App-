#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data Changed 3"),
  


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
            
            actionButton("go", "Model Data"), 
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
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
    
    modeled_input <- eventReactive(input$go, {
        model = lm(formula = y ~ x, 
                   data = dataInput())
        print(model) 
        return(model) 
        
        model_summary <- summary(modeled_input)
        intercept_value <- model_summary$coefficients[1,1]
        intercept_value
        
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
     
    
    output$distPlot <- renderPlot({
        ggplot(data = dataInput(),aes(x = dataInput()$x, y = dataInput()$y)) + 
          geom_point(aes(x = dataInput()$x, y = dataInput()$y),
                       colour = 'black') + 
         ggtitle('Regrex1 Data') + 
          xlab('X') +  
          ylab('Y') 
        #return (data_plot)
        
    })
    
    output$lmPlot <- renderPlot({
        model_summary <- summary(modeled_input())
        lm_stats <- paste("y=", model_summary$coefficients[2,1], "x+", model_summary$coefficients[1,1], "\nr2=", model_summary$adj.r.squared)
        ggplot() +
          geom_point(aes(x = dataInput()$x, y = dataInput()$y),
                       colour = 'black') +
          geom_line(aes(x = dataInput()$x, y = predict(modeled_input(), newdata = dataInput())), 
                      colour = 'green') + 
          ggtitle(lm_stats) + 
          xlab('X') +  
          ylab('Y') 
        #return (modeled_plot)
        

    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
