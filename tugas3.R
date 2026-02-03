library("readxl")
library("shiny")
library("knitr")


data <- read_excel("Tugas 3 - weather.xlsx", skip=1, col_names = TRUE)
header <- names(data)
intHeader <- setdiff(header,c('WindDir9am','WindDir3pm','WindGustDir','RainToday','RainTomorrow'))


ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        <style>
        table {
          font-family: Arial, Helvetica, sans-serif;
          border-collapse: collapse;
          width: 100%;
        }
        
        td, th {
          border: 1px solid #ddd;
          padding: 8px;
        }
        
        tr:nth-child(even){background-color: #f2f2f2;}
        
        tr:hover {background-color: #ddd;}
        
        th {
          padding-top: 12px;
          padding-bottom: 12px;
          text-align: left;
          background-color: grey;
          color: white;
        }
        </style>
        "
      )
    )
  ),
  sidebarLayout(
    sidebarPanel (
      selectInput(
        'plotType',
        label="Plot Type",
        choices= c("scatter","line","bar", "table"),
        selected="table"
      ),
      conditionalPanel(
        condition="input.plotType=='scatter'",
        selectInput(
          "x",
          label="x",
          choices=intHeader
        ),
        selectInput(
          "y",
          label="y",
          choices=intHeader
        )
      ),
      conditionalPanel(
        condition="input.plotType=='line'",
        selectInput(
          "x",
          label="x",
          choices=intHeader
        )
      ),
      conditionalPanel(
        condition="input.plotType=='bar'",
        selectInput(
          "x",
          label="x",
          choices=header
        )
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input$plotType=='table'",
        tableOutput("table")
      ),
      conditionalPanel(
        condition = "input$plotType!='table'",
        plotOutput("plot")
      )

    )
  )
)
server <- function(input, output) {

    output$plot <- renderPlot(
      {
        if(input$plotType=='scatter'){
          plot(data[[input$x]],data[[input$y]],xlab=input$x,ylab=input$y) 
        } else if (input$plotType=='line'){
          plot(data[[input$x]],type='o',xlab=input$x)
        } else if(input$plotType=='bar'){
          barplot(table(data[[input$x]]), xlab=input$x)
        }
      }
    )

  

    output$table <- function(){
      if(input$plotType=='table'){
        HTML(
            kable(
              data,
              format="html"
            )
        )
      }
      
    }
    
    #outputOptions(output, suspendWhenHidden=FALSE)
}

shinyApp(ui=ui,server=server)

