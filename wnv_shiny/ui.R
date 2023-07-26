#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)


## Top bar with title and tabs
navbarPage("WNV transmission", id = "nav",
           
           
    ## TAB 1: WNV Interactive map       
    tabPanel("Interactive map",
        div(class = "outer",
            
            ## Use custom CSS
            tags$head(
              includeCSS("styles.css")
              ),
            
            
            ## Interactive map
            leafletOutput("map", width = "100%", height = "100%"),
            
            
            ## Draggable panel on right
            absolutePanel(id = "controls", class = "panel panel-default",
                          fixed = TRUE, draggable = TRUE,
                          top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 350, height = "auto",
              
              h2("Title here"),
              
              ## User inputs zipcode
              textInput(inputId = "zip_box", label = "Zip code",
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
              plotOutput("trap_plot_yr", height = 180)
              
              
        ) ## End panel
            
            
            ),
        
        
           ) ## END TAB 1
) ## END UI
  
  

   
