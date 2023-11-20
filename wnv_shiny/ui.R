

library(shiny)
library(leaflet)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)


## Top bar with title and tabs
navbarPage(title = "MBD in Kern County", id = "nav",
           

    ## TAB 1: WNV TRAP CASES ---------------------------------------------------
    tabPanel("Mosquito Data",
             value = "tab1",
             h2("Mosquito Abundance and Diseases"),
             p("Mosquito-borne diseases (MBD) and abundance data comes from monitoring and testing efforts of the ", tags$a(href="https://www.kernmosquito.com/", "Kern Mosquito and Vector Control District."),  "Mosquito traps are deployed at various locations in Kern county and checked regularly. These pools of trapped mosquitos are then counted and tested for a range of mosquito-borne diseases. To standardize for monitoring effort, abundance and MBD cases are reported as mosquitos per trap night and minimum infection rate (MIR), respectively. For more details on mosquito species and MBD, click on the", actionLink("link_to_info", "information icon"), "in the navigation bar."),
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
                                                              "2018" = "2018",
                                                              "2017" = "2017",
                                                              "2016" = "2016",
                                                              "2015" = "2015",
                                                              "2014" = "2014",
                                                              "2013" = "2013",
                                                              "2012" = "2012",
                                                              "2011" = "2011", 
                                                              "2010" = "2010"),
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
                              end = "2023-09-30",
                              min = "2010-03-01",
                              max = "2023-09-30"),
             ), ### End side panel
             
             ### Interactive map:
             mainPanel(
               leafletOutput("trapMap", height = "380px"),
               htmlOutput("trapMap_caption"),
               br(), br()),
             
             ### Plots
             fluidRow(
               column(width = 6, uiOutput("abund_plot")),
               column(width = 6, uiOutput("wnv_plot"),
                      uiOutput("slev_plot"))
             ),
             fluidRow(
               column(width = 6, style='padding-left:50px;', 
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
              
              ## Date range:
              dateRangeInput("risk_dateRange",
                             label = h3("Date range:"),
                             start = "2023-01-01",
                             end = "2023-09-30"),
              htmlOutput("date_line"),
              
              # # Transmission risk text
              # htmlOutput("r0_header"),
              # htmlOutput("r0_value"),
              # htmlOutput("r0_line"),
              
              ## Temp plot
              htmlOutput("temp_header"),
              # uiOutput("temp_dateRange"),
              htmlOutput("tempDays_text"),
              plotOutput("temp_plot", height = 180),
              htmlOutput("temp_line"),
              
              ## Standing water plot
              htmlOutput("water_header"),
              # uiOutput("water_dateRange"),
              plotOutput("water_plot", height = 170)
              
              ) ## END panel
            )
           ), ## END TAB 2
    
    
    
    
    
    ## TAB 3: STANDING WATER  --------------------------------------------------
    tabPanel("Standing water",
             value = "tab3",
             mainPanel(
               p("Here you can explore the changes of standing water over time. Enter a Kern county zip code and select a year in the boxes below to see when and where standing water was present. Standing water can provide breeding habitat for mosquitoes; therefore, proximity to standing water may result in increased mosquito abundance and mosquito-borne disease risk. For more information, please visit the 'Info' tab."),
               fluidRow(
                 column(width = 4,
                        textInput(inputId = "zip_box_water", label = strong("Zip code:"),
                                  value = NULL,
                                  placeholder = "Enter your zip code...")
                        ),
                 column(width = 4,
                        selectInput("waterYear", label = strong("Year:"),
                                    choices = list("2023" = "2023",
                                                   "2022" = "2022"),
                                    selected = "2023")
                        )),
               hr(style = 'border-top: 1.5px solid #2d3e50; 
                  margin: 0px -250px 20px 0px')
             ), ## End main panel
             
               # mainPanel(
                 fluidRow(
                   column(width = 5, style='padding-left:30px;',
                          uiOutput("waterGif")),
                   column(width = 6, 
                   style='padding-left: 70px',
                   uiOutput("waterTab_plot"))
                 )
               
             
             
          ), ## END TAB 3
    
    
    
    
    ## TAB 4: INFO -------------------------------------------------------------
    tabPanel(title = NULL, icon = icon("info-circle", "fa-1.5x"),
             value = "tab4",
             mainPanel(width = 10,
               h3("Information:"),
               p(),
               
               bsCollapse(id="collapsePanels",
                          ## Panel 1

                          bsCollapsePanel(title="Mosquitoes:", 
                                          p("While over 50 species of mosquitoes can be found in California, not all present a threat to human health. Within Kern county, there are four species of concern responsible for disease transmission. Three of these species belong to the", em("Culex"), "genus and share similar life cycles and breeding conditions.", em("A. aegypti"), ", however, differ in breeding habitat and biting activity. More information on these mosquito-borne diseases can be found in the following section."),
                                          tabsetPanel(type = "tabs",
                                                      tabPanel(title = p(em("Culex"),"mosquitoes"),
                                                               img(src="c_tarsalis.jpg",
                                                                   height = "250px",
                                                                   style = 'border: 1px solid #2d3e50'),
                                                               p(),
                                                               p("The majority of mosquitoes in Kern County belong to the", em("Culex"), "genus. These mosquitoes are the primary vectors for West Nile, St. Louis encephalitis, and equine encephalitis viruses. ", em("Culex"), " mosquitoes breed in agricultural, natural, and human-made water sources, such as ornamental ponds, ditches, and puddles. Females lay their eggs on the surface of stagnant or fresh water, and within 10-14 days these eggs develop into adult mosquitoes that can fly away from the breeding site (Manimegalai & Sukanya, 2014). ", em("Culex"), " mosquitoes are most active at dawn, dusk, and after dark.")),
                                                      tabPanel(title = p(em("A. aegypti")),
                                                               img(src="a_aegypti.jpg",
                                                                   height = "250px",
                                                                   style = 'border: 1px solid #2d3e50'),
                                                               p(),
                                                               p(em("Aedes aegypti"), "commonly known as the Asian tiger mosquito, transmits zika, chikungunya, dengue, and yellow fever viruses (CDPH, 2023B; Pliego Pliego et al, 2017). Unlike", em("Culex"), "mosquitoes,", em("A. aegypti"), "are active during daytime as well as dawn and dusk. This species can breed in very small sources of standing water, such as bottle caps, allowing it to thrive in urban environments. Their life cycle closely follows that of ", em("Culex"), " mosquitoes; however, it takes only 7-10 days for an egg to develop into an adult mosquito (CDPH, 2023B)."))
                                                      # br(),
                                                      # img(src='culex_lifecycle.jpg',
                                                      #     align = 'middle'))
                                          )), ##end panel 1
                          ## Panel 2
                          bsCollapsePanel(title = "Mosquito Borne Diseases:",
                                          value = "mbd",
                                          tabsetPanel(type = "tabs",
                                                      tabPanel(strong("WNV:"),
                                                               p(),
                                                               p("West Nile virus (WNV) is one of 15 known mosquito-borne diseases in California (CDPH, 2023A). In North America, WNV was first detected in New York in 1999 (Lanciotti et al., 1999); the virus rapidly spread across the continent, reaching southern California by 2003 and spreading to all 58 counties in the state within a year (Hartley et al., 2012; Reisen et al., 2004). Currently, WNV is the most prevalent mosquito-borne disease in California, with over 7,500 cases 345 fatalties in California between 2003 and 2022 (CDPH, 2023A)."),
                                                                 p("WNV is mainly spread by mosquitoes in the Culex genus (Boser et al., 2021). The majority (8 out of 10) of people infected with WNV remain asymptomatic; those who do develop symptoms may experience fever, head and body aches, vomiting, and fatigue. Roughly 1 in 150 people develop serious symptoms including encephalitis or meningitis, which can result in death (CDC, 2023). ")
                                                               ), ##end WNV
                                                      tabPanel(strong("SLEV:"),
                                                               p(),
                                                               p("St. Louis encephalitis virus (SLEV) is another mosquito-borne disease of concern in California, spread to humans through the bite of infected", em("Culex"), " mosquitoes. Human cases are typically uncommon, with fewer than 10 infection reported per year in California since 1990 (CDPH, 2023A). In 2022, however, there were 16 confirmed human cases of SLEV, marking the highest number of infections since 2015 (CDPH, 2023A)."),
                                                               p("Most people infected with SLEV remain asymptomatic; those who do develop symptoms may experience them between 4 to 14 days after initial infection (CDC, 2023). Symptoms may include sudden fever, headache, dizziness, and nausea lasting several days to two weeks. For some, including older adults or people with weakened immune systems, SLEV continues to develop into encephalitis or meningitis. Roughly 5-20% of those diagnosed with SLEV die as result of infection (CDC, 2023).")
                                                               ) ##end SLEV
                                                      ),
                                          br(),
                                          footer = "More information on WNV and SLEV symptoms, prevention, and treatment can be found at the ", tags$a(href="https://www.cdc.gov/mosquitoes/about/diseases.html", "Center for Disease Control and Prevention's website.")
                                          
                                          ), ##end panel 2
                          ## Panel 3
                          bsCollapsePanel(title = "Risk Factors:",
                                          p("Here we'll discuss what can lead to increased risk of MBD, such as the amount of standing water, daily temperature, etc.")
                                          # br(),
                                          # fluidRow(
                                          #   column(width = 5, icon("droplet", "fa-5x"), align="center",
                                          #          br(),
                                          #          strong("Standing water"),
                                          #          p("The amount of standing water near your home and place of work can impact the abundance of mosquitoes in the area etc etc")),
                                          #   column(width = 5, icon("temperature-three-quarters", "fa-5x"),
                                          #          align = "center")
                                          # )
                                          ), ## end panel 3
                          ## Panel 4
                          bsCollapsePanel(title = "Sources:",
                                          tags$li("Beyer, H. L., Dujardin, Y., Watts, M. E., & Possingham, H. P. (2016). Solving conservation planning problems with integer linear programming. Ecological Modelling, 328, 14–22. https://doi.org/10.1016/j.ecolmodel.2016.02.005"),
                                          tags$li("California Department of Public Health, Mosquito & Vector control Association of California. (2023A). University of California. California Mosquito-Borne Virus Surveillance and Response Plan."),
                                          tags$li("California Department of Public Health. (2023B). Aedes Aegypti and Aedes Albopictus Mosquitos. https://www.cdph.ca.gov/Programs/CID/DCDC/pages/Aedes-aegypti-and-Aedes-albopictus-mosquitoes.aspx"),
                                          tags$li("Centers for Disease Control and Prevention. (2023). West Nile Virus: Symptoms, Diagnosis, and Treatment. https://www.cdc.gov/westnile/symptoms/index.html"),
                                          tags$li("Hartley, D. M., Barker, C. M., Le Menach, A., Niu, T., Gaff, H. D., & Reisen, W. K. (2012). Effects of Temperature on Emergence and Seasonality of West Nile Virus in California. The American Journal of Tropical Medicine and Hygiene, 86(5), 884–894. https://doi.org/10.4269/ajtmh.2012.11-0342"),
                                          tags$li("Lanciotti, R. S. et al. (1999). Origin of the West Nile Virus Responsible for an Outbreak of Encephalitis in the Northeastern United States. Science, 286(5448), 2333–2337. https://doi.org/10.1126/science.286.5448.2333"),
                                          tags$li("Manimegalai, K., & Sukanya, S. (2014). Original Research Article Biology of the filarial vector, Culex quinquefasciatus (Diptera:Culicidae). 718–724."),
                                          tags$li("Pliego Pliego, E., Velázquez-Castro, J., & Fraguela Collar, A. (2017). Seasonality on the life cycle of Aedes aegypti mosquito and its statistical relation with dengue outbreaks. Applied Mathematical Modelling, 50, 484–496. https://doi.org/10.1016/j.apm.2017.06.003"),
                                          tags$li("Reisen, W., Lothrop, H., Chiles, R., Madon, M., Cossen, C., Woods, L., Husted, S., Kramer, V., & Edman, J. (2004). West Nile Virus in California. Emerging Infectious Diseases, 10(8), 1369–1378. https://doi.org/10.3201/eid1008.040077")
                                          ) ##end panel 4
                          
                          ) ##END bsCollapse
             ) ## END main panel
    ) ## END TAB 4
    
    
    
    
) ## END UI
  
  

   
