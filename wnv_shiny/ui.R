

library(shiny)
library(leaflet)


## Top bar with title and tabs
navbarPage(title = "WNV in Kern County", id = "nav",
           
           
    ## TAB 1: WNV Interactive Map ----------------------------------------------  
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
                        value = NULL,
                        placeholder = "Enter your zip code..."
                        ),
              
              ## Transmission risk text
              htmlOutput("trans_header"),
              htmlOutput("zip_risk"),
              br(),
              
              ## Temp plot
              htmlOutput("temp_header"),
              uiOutput("temp_dateRange"),
              htmlOutput("tempDays_text"),
              plotOutput("temp_plot", height = 180),
              
              ## Standing water plot
              htmlOutput("water_header"),
              plotOutput("water_plot", height = 180),
              br()
              
              
        ) ## End panel
            ),
           ), ## END TAB 1
    
    
    
    
    ## TAB 2: WNV TRAP CASES ---------------------------------------------------
    tabPanel("Trap cases",
             value = "tab2",
             h2("Mosquito and WNV Abundance"),
             p("This is some more info and copy about the data. Such as: Mosquito abundance and WNV data comes from trapping efforts by the Kern County Vector Control Board. Stuff about methodology: Traps are layed out and checked every 1-X days. These pools of mosquitos are then tested for WNV. To standardize for monitoring effort, abundance and WNV cases have been standardized to cases per night."),
             p("You can view information by zip code either by entering the zip code of interest on the left-hand panel, or by clicking on the zip code within the map on the right."),
             ### Side Panel: 
             sidebarPanel(
               h3(strong("Trap information:")),
               ### Zipcode input:
               textInput(inputId = "zip_box_trap", label = h4("Zip code:"),
                         value = NULL,
                         placeholder = "Enter your zip code..."
               ),
               br(),
               ### Abundance/WNV:
               radioButtons("trapRadio", label = h4("Metric:"),
                            inline = TRUE,
                            choices = c("Abundance" = "abundance",
                                        "WNV positive" = "wnv")),
               br(),
               ### Time period:
               h4("Time period:"),
               fluidRow(   ## put both input boxes in-line
                 column(width = 6, selectInput("trapAnnual", label = "Select timeframe:",
                                               choices = list("Annual" = "annual",
                                                              "Monthly" = "monthly",
                                                              "Custom" = "custom"),
                                               selected = "annual")),
                 column(width = 6, uiOutput("trapMonth"))
               ),
               uiOutput("trapDates"),
               
             ), ### End side panel
             
             mainPanel(
               leafletOutput("trapMap", height = "400px"),
               htmlOutput("trapMap_caption"),
               plotOutput("trap_plot"))
             
             
             ), ## END TAB 2
    
    
    
    
    
    ## TAB 3: STANDING WATER  --------------------------------------------------
    tabPanel("Standing water",
             value = "tab3",
             p("This is where we'll explore changes of standing water over time with graphs and/or animated GIFs.")
             
             ), ## END TAB 3
    
    
    
    
    ## TAB 4: INFO -------------------------------------------------------------
    tabPanel(title = NULL, icon = icon("info-circle", "fa-1.5x"),
             value = "tab4",
             h3("West Nile Virus:"),
             p("West Nile Virus (WNV) is one of 15 known mosquito-borne diseases in California (newsom). In North America, WNV was first detected in New York in 1999 (Lanciotti); the virus rapidly spread across the continent, reaching southern California by 2003 (Reisen) and spreading to all 58 counties in the state within a year (Hartley). Currently, WNV is the most prevalent mosquito-borne disease in California, having infected over 7,500 people and killing 345 between 2003 and 2022 (Newsom)."),
             tabsetPanel(type = "tabs",
                         tabPanel("Spread:", p("WNV is mainly spread by mosquitoes in the genus Culex (Hartley; Bosner))"),
                                  img(src='culex_lifecycle.jpg', align = 'middle')),
                         tabPanel("Risk factors:", 
                                  p("Here we'll discuss what can lead to increased risk of WNV, such as amount of standing water, daily temperature, proximity to avian vectors, etc.")),
                         tabPanel("Symptoms:", p("The majority (8 out of 10) of people infected with WNV remain asymptomatic; those who do develop symptoms may experience fever, head and body aches, vomiting, and fatigue. Roughly 1 in 150 people develop serious symptoms including encephalitis or meningitis, which can result in death. (CDC). ")))
             # p("here is more info on mosquito breeding cycle and stuff.")
             
    ), ## END TAB 4
    
    
    
    
) ## END UI
  
  

   
