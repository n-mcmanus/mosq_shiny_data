

library(shiny)
library(leaflet)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)


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
            
            ## "Dragable" panel w/plots
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
              # htmlOutput("r0_header"),
              # htmlOutput("r0_value"),
              # htmlOutput("r0_line"),
              
              ## Date range:
              htmlOutput("dates_header"),
              uiOutput("risk_dateRange"),
              htmlOutput("date_line"),
              
              ## Temp plot
              htmlOutput("temp_header"),
              # uiOutput("temp_dateRange"),
              htmlOutput("tempDays_text"),
              plotOutput("temp_plot", height = 180),
              htmlOutput("temp_line"),
              
              ## Standing water plot
              htmlOutput("water_header"),
              uiOutput("water_dateRange"),
              plotOutput("water_plot", height = 170)
              
              ) ## END panel
            )
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
                         animate = animationOptions(interval = 500)),
             # verbatimTextOutput("value"),
             
             ## map
             leafletOutput("waterMap", height = "500px")
             
             ), ## END TAB 3
    
    
    
    
    ## TAB 4: INFO -------------------------------------------------------------
    tabPanel(title = NULL, icon = icon("info-circle", "fa-1.5x"),
             value = "tab4",
             mainPanel(width = 12,
               h3("Information:"),
               p(),
               
               bsCollapse(id="collapsePanels",
                          ## Panel 1

                          bsCollapsePanel(title="Mosquitoes:", 
                                          p("While over 50 species of mosquitoes can be found in California, not all present a threat to human health. Within Kern county, there are four species of concern responsible for disease transmission. Three of these species belong to the", em("Culex"), "genus and share similar life cycles and breeding conditions (true??).", em("A. aegypti"), ", however, differ in breeding habitat and biting activity. More information on these mosquito-borne diseases can be found in the following section."),
                                          HTML("<p> You can find more <a href='#temp_dateRange'> click here</a> other stuff"),
                                          bsCollapsePanel(title = p(em("Culex"),"mosquitoes:"),
                                                          p("Mosquitos from the genus Culex are the most common in Kern. 
                                                            While they all are capabile of transmitting "),
                                                          tabsetPanel(type = "pills",
                                                                      tabPanel("C. tarsalis",
                                                                               p(em("Culex tarsalis"), ", commonly known as the Western Encephalitis mosquito, is one of the primary vectors for West Nile, St. Louis encephalitis, and equine encephalitis viruses. This species breeds in agricultural, natural, and human-made water sources and is most active at dawn, dusk, and after dark."),
                                                                               img(src="c_tarsalis.jpg",
                                                                                   height = "300px")),
                                                                      tabPanel("C. pipiens",
                                                                               p(em("Culex pipiens"), "commonly known as the Northern House mosquito, transmits both West Nile and St. Louis encephalitis viruses. This species primarily breeds in foul or polluted water sources, both natural and human-made, as well as artificial containers.", em("C. pipiens"), "bites at dawn, dusk, and after dark."),
                                                                               img(src="c_pipiens.jpg",
                                                                                   height = "300px")),
                                                                      tabPanel("C. quinquefasciatus",
                                                                               p(em("Culex quinquefasciatus"), "is the same as pipiens??"),
                                                                               img(src="c_quinquefasciatus.jpg",
                                                                                   height = "300px")),
                                                                      footer = "Graphic and info on breeding cycle.",
                                                                      img(src='culex_lifecycle.jpg',
                                                                          align = 'middle'))
                                                          ), ##end culex panel
                                          bsCollapsePanel(title = em("A. aegypti"),
                                                          p(em("Aedes aegypti"), "commonly known as the Asian tiger mosquito, transmits Zika, dengue, chikungunya, and yellow fever viruses. Unlike", em("Culex"), "mosquitoes,", em("A. aegypti"), "are active during daytime as well as dawn and dusk. This species can breen in very small sources or standing water, allowing it to thrive in urban environments."),
                                                          img(src="a_aegypti.jpg",
                                                              height = "300px")
                                                          ) ##end aegypti panel
                                          ), ##end panel 1
                          ## Panel 2
                          bsCollapsePanel(title = "Mosquito Borne Diseases:",
                                          value = "mbd",
                                          "Info on WNV and STEV here.",
                                          tabsetPanel(type = "pills",
                                                      tabPanel("WNV:",
                                                               p("West Nile virus (WNV) is one of 15 known mosquito-borne diseases in California (newsom). In North America, WNV was first detected in New York in 1999 (Lanciotti); the virus rapidly spread across the continent, reaching southern California by 2003 (Reisen) and spreading to all 58 counties in the state within a year (Hartley). Currently, WNV is the most prevalent mosquito-borne disease in California, with over 7,500 cases 345 fatalties in California between 2003 and 2022 (Newsom)."),
                                                                 p("WNV is mainly spread by mosquitoes in the genus Culex (Hartley; Bosner"), 
                                                                 p(strong("Symptoms:"), "The majority (8 out of 10) of people infected with WNV remain asymptomatic; those who do develop symptoms may experience fever, head and body aches, vomiting, and fatigue. Roughly 1 in 150 people develop serious symptoms including encephalitis or meningitis, which can result in death. (CDC). ")
                                                               ),
                                                      tabPanel("SLEV:",
                                                               p("St. Louis encephalitis virus (SLEV) is another MBD of concern in California, spread to people through the bite of infected", em("C. pipiens"), "or", em("C. quinquefasciatus"), " mosquitoes. Human cases are typically uncommon, with fewer than 10 infection reported per year in California since 1990 (CDPH). In 2022, however, there were 16 confirmed human cases of SLEV, marking the highest number of infections since 2015 (Newsom)."),
                                                               p(strong("Symptoms:"), "Most people infected with SLEV remain asymptomatic; those who do develop symptoms may experience them between 4 to 14 days after initial infection (CDC). Symptoms may include sudden fever, headache, dizziness, and nausea lasting several days to two weeks. For some, including older adults or people with weakened immune systems, SLEV continues to develop into encephalitis or meningitis. Roughly 5-20% of those diagnosed with SLEV die as result of infection (CDC).")
                                                               )
                                                      )
                                          ), ##end panel 2
                          ## Panel 3
                          bsCollapsePanel(title = "Risk Factors:",
                                          p("Here we'll discuss what can lead to increased risk of WNV, such as amount of standing water, daily temperature, proximity to avian vectors, etc."),
                                          br(),
                                          fluidRow(
                                            column(width = 5, icon("droplet", "fa-5x"), align="center",
                                                   br(),
                                                   strong("Standing water"),
                                                   p("The amount of standing water near your home/place of work can have a large impact on the abundance of mosquitoes in the area blahalba lbha albha lbd")),
                                            column(width = 5, icon("temperature-three-quarters", "fa-5x"),
                                                   align = "center")
                                          )
                                          ), ## end panel 3
                          ## Panel 4
                          bsCollapsePanel(title = "Sources:",
                                          "Info on data and lit sources") ##end panel 4
                          
                          ) ##END bsCollapse
             ) ## END main panel
    ) ## END TAB 4
    
    
    
    
) ## END UI
  
  

   
