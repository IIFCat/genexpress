library(shiny)

library(dplyr)
library(Seurat) #Version 3
library(Matrix)
library(ggplot2)

library(dygraphs)
library(plotly)

library(shinyjs)
library(shinycssloaders)

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

load("~/Documents/2019b_Summer/academic_stuff/Blue Data/Adult_Normal_Kidney_snDrop_Lake2019_NCOMM_Seuratv3.Robj")
load("~/Documents/2019b_Summer/academic_stuff/Blue Data/Adult_Normal_Kidney_snDrop_Lake2019_NCOMM_Cluster_Colors.Robj")

gene.set.selected <- as.list(rownames(Loadings(ank.3, reduction = "pca")))


#if (interactive()) {
  ui <- fluidPage(
    
    useShinyjs(),
    inlineCSS(appCSS),
    
    # Loading message
    div(
      id = "loading-content",
      h2("Loading...")
    ),
    
    hidden(
      div(
        id = "app-content",
        titlePanel("Expression Plot & Location on Dimension Reduction Plot of Selected Gene:"),
        sidebarPanel(
          textInput('features', 'Gene'),
          verbatimTextOutput("value"),
          actionButton("button", "Show"),
          width = 4
        ),
        mainPanel(
          tabsetPanel(type = "tabs",
            tabPanel("Expression Plot", plotlyOutput('plot1', height = NULL)%>% withSpinner(color="#0dc5c1")),
            tabPanel("Location on Dimension Reduction Plot", plotlyOutput('plot2', height = NULL)%>% withSpinner(color="#0dc5c1"))
          )
        )
      )
    ),
    
    hidden(
      p(
        id = "looking-up-mes",
        textOutput("Looking up gene entered.\n")
      )
    ),
    
    hidden(
      p(
        id = "error-mes",
        textOutput('Entered gene not found.\n')
      )
    )
  )

  
  server <- function(input, output) {
    
    selDF <- eventReactive(input$button, {
      (input$features)
    })
    
    observeEvent(input$button, {
      show("looking-up-mes")
      
      if (is.element(selDF(), gene.set.selected)){
        output$value <- renderText({input$features})
        output$plot1 <- renderPlotly(
          #Plot Expression
          DotPlot(object = ank.3, features = selDF())
        )
        output$plot2 <- renderPlotly(
          #Plot Location
          FeaturePlot(object = ank.3, reduction = "pUMAP", features = selDF())
        )
      } else {
          show("error-mes")
        }
      })
    output$value <- renderText({selDF()})
    
    # Hide the loading message when the rest of the server function has executed
    hide(id = "loading-content", anim = TRUE, animType = "fade")    
    show("app-content")
  }
  #shinyApp(ui, server)
#}
