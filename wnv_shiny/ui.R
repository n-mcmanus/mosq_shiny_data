

library(shiny)
library(leaflet)


## Top bar with title and tabs
navbarPage(title = "WNV in Kern County", id = "nav",
           
           
    ## TAB 1: WNV Interactive map       
    tabPanel("Interactive map",
             id = "tab1",
        div(class = "outer",
            
            ## Use custom CSS
            tags$head(
              includeCSS("styles.css")
              ),
            
            
            ## Interactive map
            leafletOutput("map", width = "100%", height = "100%"),
            
            
            ## Dragable panel on right
            absolutePanel(id = "controls", class = "panel panel-default",
                          fixed = TRUE, draggable = TRUE,
                          top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 350, height = "auto",
              
              # h2("Title here"),
              
              ## User inputs zipcode
              textInput(inputId = "zip_box", label = h3("Zip code:"),
                        value = NA,
                        placeholder = "Enter your zip code..."
                        ),
              
              ## Transmission risk text
              htmlOutput("trans_header"),
              htmlOutput("zip_risk"),
              br(),
              
              ## Standing water plot
              htmlOutput("water_header"),
              plotOutput("water_plot", height = 180),
              br(),
              
              ## Trap plots
              htmlOutput("trap_header"),
              fluidRow(   ## put both input boxes in-line
                column(width = 6, uiOutput("trap_time")),
                column(width = 6, uiOutput("trap_month"))
              ),
              plotOutput("trap_plot", height = 180)
              
              
        ) ## End panel
            
            
            ),
        
        
           ), ## END TAB 1
    
    
    
    
    ## TAB 2: WNV TRAP CASES
    tabPanel("Trap cases",
             value = "tab2",
             p("This is where we'll have more detailed graphs and exploration of standardized trap data.")
             
             ), ## END TAB 2
    
    
    
    
    
    ## TAB 3: STANDING WATER
    tabPanel("Standing water",
             value = "tab3",
             p("This is where we'll explore changes of standing water over time with graphs and/or animated GIFs.")
             
             ), ## END TAB 3
    
    
    
    
    ## TAB 4: INFO
    tabPanel(title = NULL, icon = icon("info-circle", "fa-1.5x"),
             value = "tab4",
             h3("WNV INFO!!"),
             p("here is more info on mosquito breeding cycle and stuff.")
             
    ), ## END TAB 4
    
    
    
    
) ## END UI
  
  

   
