library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
source('helper-functions.R')

# Define UI
ui <- fluidPage(
    titlePanel("Quick Barcoder"),
    sidebarPanel(

    fluidRow(
        column(4,
               fileInput('fileload.id',h3('Load Sample Metadata'),accept='.tsv')),
        column(4,
               selectInput("strat.vars.id",
                                  h3("Stratification Variables"),
                                  multiple = T,
                                  choices = list())),
    ),

    fluidRow(
        column(4,
               numericInput("slots.id",
                            h3("Slots per Batch"),
                            value = 18)),
        column(4,
               numericInput("rep.id",
                            h3("Rep Threshold"),
                            value = 100))


    ),

    fluidRow(
        column(4,actionButton("action.id","Sort Samples")),
        column(4,downloadButton('download','Save'))
    )
    ),
    mainPanel(plotOutput("samplePlot"),plotOutput('subjectPlot'))
)

# Define server logic required to sort and plot ----
server <- function(input, output, session) {


    # Update stratification variable dropdown based on metadata
    observe({
        req(input$fileload.id) # Require that an input is available
        df <- read.table(input$fileload.id$datapath,sep='\t',header=T)
        updateSelectInput(session,"strat.vars.id",choices=colnames(df))
    })

    # Load and use metadata
    meta<-reactive({
        req(input$fileload.id) # Require that an input is available
        df <- read.table(input$fileload.id$datapath,sep='\t',header=T)
        return(df)
    })

    barcodeTab <- eventReactive(input$action.id,{
        tab <- auto_barcode(meta(),
                                 strata.vars = input$strat.vars.id,
                                 slots.per.batch = input$slots.id,
                                 rep.threshold = input$rep.id)


    })

    output$samplePlot <- renderPlot({
        req(barcodeTab())
        pdat <- data.frame(table(barcodeTab()[,c('PID','strata.group')]))
        ggplot(pdat,aes(fill=strata.group,x=PID,y=Freq))+
            geom_bar(position='stack',stat='identity')+
            ylab('Sample Count')
    })

    output$subjectPlot <- renderPlot({
        req(barcodeTab())
        sdat <- unique(barcodeTab()[,c('PID','strata.group','subject.id')])
        pdat <- data.frame(table(sdat[,c('PID','strata.group')]))
        ggplot(pdat,aes(fill=strata.group,x=PID,y=Freq))+
            geom_bar(position='stack',stat='identity')+
            ylab('Subject Count')
    })

    # Download and barcoding scheme
    output$download <- downloadHandler(
        filename = function() {
            paste0('barcoded_',input$fileload.id)
        },
        content = function(file) {
            write.table(barcodeTab(),file,sep='\t',col.names=T,row.names=F)
        }
    )

}

shinyApp(ui = ui, server = server)
