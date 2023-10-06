

library(shiny)
library(leaflet)


## Top bar with title and tabs
navbarPage(title = "WNV in Kern County", id = "nav",
           

    ## TAB 1: WNV TRAP CASES ---------------------------------------------------
    tabPanel("Mosquito Data",
             value = "tab1",
             h2("Mosquito Abundance and WNV"),
             p("Here will be info and copy about the data, such as: Mosquito abundance and WNV data comes from monitoring and testing efforts of the Kern Mosquito and Vector Control District. Copy on methodology: Traps are layed out and checked every 1-X days. These pools of trapped mosquitos are then tested for a range of mosquito-borne diseases. To standardize for monitoring effort, abundance and WNV cases are reported as mosquitos per trap night and minimum infection rate (MIR), respectively. For more details, click on the information icon in the navigation bar."),
             p("You can view information by zip code either by entering the zip code of interest on the left-hand panel, or by clicking on the zip code within the map on the right."),
             ### Side Panel: 
             sidebarPanel(
               # h3(strong("Trap data:")),
               ### Zipcode input:
               textInput(inputId = "zip_box_trap", label = h4(strong("Zip code:")),
                         value = NULL,
                         placeholder = "Enter your zip code..."
               ),
               br(),
  
               ### Abundance/WNV:
               # radioButtons("trapRadio", label = h4("Metric:"),
               #              inline = TRUE,
               #              choices = c("Abundance" = "abundance",
               #                          "WNV cases" = "wnv")),
               
               
               
               h4(strong("Time period:")),
               # p("Quick selection:"),
               fluidRow(   ## put both input boxes in-line
                 column(width = 6, selectInput("trapYear", label = "Year:",
                                               choices = list("2023" = "2023",
                                                              "2022" = "2022",
                                                              "2021" = "2021",
                                                              "2020" = "2020",
                                                              "2019" = "2019",
                                                              "2018" = "2018"),
                                               selected = 2023)),
                 column(width = 6, selectInput("trapMonth", label = "Month:",
                                               choices = list("NA" = "none",
                                                              "June" = "06",
                                                              "July" = "07",
                                                              "August" = "08",
                                                              "September" = "09",
                                                              "October" = "10"),
                                               selected = "NA"))
               ),
               p("Custom date range:"),
               dateRangeInput("trap_dateRange", 
                              label = NULL,
                              start = "2023-01-01",
                              end = "2023-07-31",
                              min = "2018-04-15",
                              max = "2023-07-31"),
               
               
               ### Time period (OLD CODE):
               # h4("Time period:"),
               # fluidRow(   ## put both input boxes in-line
               #   column(width = 6, selectInput("trapTime", label = "Select timeframe:",
               #                                 choices = list("Annual" = "annual",
               #                                                "Monthly" = "monthly",
               #                                                "Custom" = "custom"),
               #                                 selected = "annual")),
               #   column(width = 6, uiOutput("trapMonth"))
               # ),
               # uiOutput("trap_dateRange"),
               
             ), ### End side panel
             
             ### Interactive map:
             mainPanel(
               leafletOutput("trapMap", height = "380px"),
               htmlOutput("trapMap_caption"),
               br(), br()),
             
             ### Plots
             fluidRow(
               column(width = 6, uiOutput("abund_plot")),
               column(width = 6, uiOutput("wnv_plot"))
             ),
             fluidRow(
               column(width = 6, style='padding-left:55px;', 
                      htmlOutput("abundPlot_caption")),
               column(width = 6, style='padding-left:55px;',
                      htmlOutput("wnvPlot_caption"))
             ),
             br()
             
             
             ), ## END TAB 1
    
    
    
    
           
    ## TAB 2: WNV Interactive Map ----------------------------------------------  
    tabPanel("Risk Map",
             id = "tab2",
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
              
              ## Zip code
              textInput(inputId = "zip_box", label = h3("Zip code:"),
                        value = NULL,
                        placeholder = "Enter your zip code..."
                        ),
              hr(style = 'border-top: 1.5px solid #2d3e50'),
              
              ## Transmission risk text
              htmlOutput("r0_header"),
              htmlOutput("r0_value"),
              htmlOutput("r0_line"),
              
              ## Temp plot
              htmlOutput("temp_header"),
              uiOutput("temp_dateRange"),
              htmlOutput("tempDays_text"),
              plotOutput("temp_plot", height = 180),
              htmlOutput("temp_line"),
              
              ## Standing water plot
              htmlOutput("water_header"),
              plotOutput("water_plot", height = 170)
              
              
        ) ## End panel
            ),
           ), ## END TAB 2
    
    
    
    
    
    ## TAB 3: STANDING WATER  --------------------------------------------------
    tabPanel("Standing water",
             value = "tab3",
             p("This is where we'll explore changes of standing water over time with graphs and/or animated GIFs."),
             
             ## time slider
             sliderInput("waterDate", 
                         "Slide to date:",
                         min = as.Date("2023-03-13"),
                         max = as.Date("2023-07-11"),
                         value = as.Date("2023-03-13"),
                         timeFormat = "%d %b %y",
                         ticks = TRUE,
                         animate = animationOptions(interval = 100)),
             # verbatimTextOutput("value"),
             
             ## map
             leafletOutput("waterMap", height = "400px")
             
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
  
  

   
