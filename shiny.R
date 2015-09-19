library(shiny)
library(ggplot2)

#ui
ui <- fluidPage(titlePanel(strong("Cows overview")),sidebarPanel(width= 2, 
                                                                 
                                                                 selectInput("dataset", "Choose a group:",
                                                                             choices = c("all cows", "group1", "group2", "group3" , "group4", "group5")),
                                                                 
                                                                 radioButtons("plot_type", "Choose plot type",
                                                                              c("Without lines", "With lines"), inline = TRUE)
),
#fluidRow(
column(width = 10, class = "well",
       h3(strong("Explore the cows habitat")),
       plotOutput("plot1", height = 500
       )
),
#  ),
column(width = 12, class = "well",
       selectInput("cow", "Choose a specific cow:", 
                   choices = c("cow2927", "cow3010", "cow3024", "cow3049" , "cow3077", "cow3107", "cow3188",
                               "cow3194", "cow3203", "cow3227", "cow3242" , "cow3253", "cow3297", "cow3300",
                               "cow3302", "cow3307", "cow3341", "cow3354" , "cow3388", "cow3397", "cow3404",
                               "cow3405", "cow3406", "cow3418", "cow3427" , "cow3431", "cow3436", "cow3437",
                               "cow3440", "cow3441", "cow3452", "cow3463")),
       h3(strong("Expore one cow- left plot controls right plot")),
       fluidRow(
         column(width = 4,
                plotOutput("plot2", height = 300,
                           brush = brushOpts(
                             id = "plot2_brush",
                             resetOnNew = TRUE
                           )
                )
         ),
         column(width = 4,
                plotOutput("plot3", height = 300)
         )
       )
)

)


#------------------------------------------------------------
#server

server <- function(input, output) {
  
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    if (input$dataset== "all cows"){
      if (input$plot_type == "Without lines") {
        t
      } else if (input$plot_type == "With lines") {
        t + geom_density2d(colour=1)
      }
    }else if (input$dataset =="group1"){
      if (input$plot_type == "Without lines") {
        Pfat1
      } else if (input$plot_type == "With lines") {
        Pfat1 + geom_density2d(colour=1)
      }
    }else if (input$dataset =="group2"){
      if (input$plot_type == "Without lines") {
        Pfat2
      } else if (input$plot_type == "With lines") {
        Pfat2 + geom_density2d(colour=1)
      }  
    }else if (input$dataset =="group3"){
      if (input$plot_type == "Without lines") {
        Pfat3
      } else if (input$plot_type == "With lines") {
        Pfat3 + geom_density2d(colour=1)
      }
    }else if (input$dataset =="group4"){
      if (input$plot_type == "Without lines") {
        Pfat4
      } else if (input$plot_type == "With lines") {
        Pfat4 + geom_density2d(colour=1)
      }
    }else if (input$dataset =="group5"){
      if(input$plot_type=="Without lines"){
        Pfat5
      }else if (input$plot_type == "With lines") {
        Pfat5 + geom_density2d(colour=1)
      }
    }
    
  })
  
  
  # -------------------------------------------------------------------
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({ if (input$cow== "cow2927"){pCow1}
    else if (input$cow== "cow3010"){pCow2}
    else if (input$cow== "cow3024"){pCow3}
    else if (input$cow== "cow3049"){pCow4}
    else if (input$cow== "cow3077"){pCow5}
    else if (input$cow== "cow3107"){pCow6}
    else if (input$cow== "cow3188"){pCow7}
    else if (input$cow== "cow3194"){pCow8}
    else if (input$cow== "cow3203"){pCow9}
    else if (input$cow== "cow3227"){pCow10}
    else if (input$cow== "cow3242"){pCow11}
    else if (input$cow== "cow3253"){pCow12}
    else if (input$cow== "cow3297"){pCow13}
    else if (input$cow== "cow3300"){pCow14}
    else if (input$cow== "cow3302"){pCow15}
    else if (input$cow== "cow3307"){pCow16}
    else if (input$cow== "cow3341"){pCow17}
    else if (input$cow== "cow3354"){pCow18}
    else if (input$cow== "cow3388"){pCow19}
    else if (input$cow== "cow3397"){pCow20}
    else if (input$cow== "cow3404"){pCow21}
    else if (input$cow== "cow3405"){pCow22}
    else if (input$cow== "cow3406"){pCow23}
    else if (input$cow== "cow3418"){pCow24}
    else if (input$cow== "cow3427"){pCow25}
    else if (input$cow== "cow3431"){pCow26}
    else if (input$cow== "cow3436"){pCow27}
    else if (input$cow== "cow3437"){pCow28}
    else if (input$cow== "cow3440"){pCow29}
    else if (input$cow== "cow3441"){pCow30}
    else if (input$cow== "cow3452"){pCow31}
    else if (input$cow== "cow3463"){pCow32}
    
  })
  
  output$plot3 <- renderPlot({if (input$cow== "cow2927"){pCow1+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow=="cow3010"){pCow2+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow=="cow3024"){pCow3+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow=="cow3049"){pCow4+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow=="cow3077"){pCow5+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow=="cow3107"){pCow6+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow=="cow3188"){pCow7+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3194"){pCow8+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3203"){pCow9+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3227"){pCow10+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3242"){pCow11+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3253"){pCow12+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3297"){pCow13+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3300"){pCow14+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3302"){pCow15+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3307"){pCow16+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3341"){pCow17+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3354"){pCow18+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3388"){pCow19+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3397"){pCow20+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3404"){pCow21+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3405"){pCow22+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3406"){pCow23+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3418"){pCow24+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3427"){pCow25+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3431"){pCow26+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3436"){pCow27+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3437"){pCow28+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3440"){pCow29+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3441"){pCow30+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3452"){pCow31+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
    else if (input$cow== "cow3463"){pCow32+ coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)}
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })#observe
  
}#function (input,output)

shinyApp(ui, server)
