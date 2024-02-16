## Read in packages
library(shiny)
library(leaflet)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(bslib)
library(bsicons)
library(plotly)

## Start UI with top navigation bar
ui <- page_navbar(
  title = "MBD in Kern County",
  id = "nav",
  fillable = FALSE, ## Page spacing gets weird if TRUE
  theme = bs_theme(bootswatch = "lumen"),
  
  
  # TAB 1: WNV TRAP CASES ---------------------------------------------------
  nav_panel(
    title = "Mosquito Data",
    value = "tab1",
    h2("Mosquito Abundance and Diseases"),
    ## Intro text
    p(
      "This page explores how mosquito abundance and mosquito-borne diseases (MBD) change over time across zip codes in Kern County. Data comes from mosquito and disease surveillance efforts of the ", 
      tags$a(href = "https://www.kernmosquito.com/", "Kern Mosquito and Vector Control District,"),
      "who deploy mosquito traps throughout the county. These pools of trapped mosquitos are then counted and tested for a range of MBD’s. To standardize for monitoring effort, abundance of mosquitos and rates of infection in mosquitos are reported as mosquitos per trap night and minimum infection rate (MIR), respectively. Details on these metrics can be found by pressing the ",
      bs_icon("question-circle", size = "1em"),
      " on their respective figures. For more details on mosquito species and MBD in Kern County, please visit the Information tab."
      # Example for linking to another tab. Not sure if it's necessary here though
      # actionLink("link_to_info", "tab.")
    ),
    
    ## Map with sidebar
    card(
      height = 500,
      full_screen = TRUE, ##Allows for option of full screen
      ## Removed this for now, but could be added back if functionality is built out
      # card_header(
      #   popover(
      #     trigger = bs_icon("bar-chart-fill", class = "ms-auto"),
      #     title = "Test",
      #     "Here is where we can add a pop-up chart of all the counties with positive MIR values during the given time period."
      #   ),
      #   class = "d-flex align-items-center gap-1"
      # ),
      layout_sidebar(
        ## remove padding around map
        class = "p-0",
        ## leaflet map
        card_body(class = "p-0",
                  leafletOutput("trapMap")),
        ## sidebar
        sidebar = sidebar(
          width = "310px",
          position = "left",
          padding = "15px",
          ## zip code box
          textInput(
            inputId = "zip_box_trap",
            label = h5(strong("Zip code:")),
            value = NULL,
            placeholder = "Enter your zip code..."
          ),
          p(),
          h5(strong("Time period:")),
          # Select year/month
          fluidRow(column(
            width = 6,
            selectInput(
              inputId = "trapYear",
              label = span(
                "Year:",
                tooltip(
                  bs_icon("info-circle"),
                  "To select multiple years, use the custom date range below",
                  options = list(class = "background-color: #f00")
                )),
              choices = list(
                "2023" = "2023",
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
                "2010" = "2010"
              ),
              selected = 2023
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = "trapMonth",
              label = "Month:",
              choices = list(
                "NA" = "none",
                "June" = "06",
                "July" = "07",
                "August" = "08",
                "September" = "09",
                "October" = "10"
              ),
              selected = "NA"
            )
          )),##end fluidrow
          p("Custom date range:"),
          ## calendar inputs
          dateRangeInput(
            inputId = "trap_dateRange",
            label = NULL,
            start = "2023-01-01",
            end = "2023-09-30",
            ## don't let user select dates 
            ## outside of when we have data
            min = "2010-03-01",
            max = "2023-09-30"
          )
        ),
      ), ##end layout_sidebar
      card_footer(
        htmlOutput("trapMap_caption")
      )
    ), ##end map card
    
    ## Abundance and MIR plots
    ## generate once zip code is selected
    htmlOutput("trapPlots")
    
  ), ##END TAB 1
  
  
  
  # TAB 2: WNV Interactive Map ----------------------------------------------  
  nav_panel(title = "Risk Map",
            value = "tab2",
            div(class = "outer",
                ## Use custom CSS for map and panel
                tags$head(
                  includeCSS("styles.css")
                ),
                
                ## Interactive map
                leafletOutput("riskMap", width = "100%", height = "100%"),
                
                ## "Dragable" panel w/plots
                absolutePanel(id = "controls", 
                              class = "panel panel-default",
                              fixed = TRUE, draggable = TRUE,
                              top = 80, left = "auto", right = 20, bottom = "auto",
                              width = 380, height = "auto", cursor = "move",
                              
                              ## Zip code
                              textInput(inputId = "zip_box", label = h5("Zip code:"),
                                        value = NULL,
                                        placeholder = "Enter your zip code..."
                              ),
                              
                              ## Date range:
                              dateRangeInput("risk_dateRange",
                                             label = h5("Date range:"),
                                             start = "2023-01-01",
                                             end = "2023-09-30",
                                             ## don't let user select dates 
                                             ## outside of when we have data
                                             min = "2010-03-01",
                                             max = "2023-09-30"
                              ),
                              htmlOutput("date_line"),
                              
                              ### For when R0 is incorporated
                              ## R0 text 
                              # htmlOutput("r0_header"),
                              # htmlOutput("r0_value"),
                              # htmlOutput("r0_line"),
                              
                              ## Putting temp and water in drop-down menus
                              htmlOutput("riskAccordion")
                              
                              ### If not using drop-down menus, use
                              ### this code below
                              ## Temp plot
                              # htmlOutput("temp_header"),
                              # htmlOutput("tempDays_text"),
                              # plotOutput("temp_plot", height = 180),
                              # htmlOutput("temp_line"),
                              # 
                              # ## Standing water plot
                              # htmlOutput("water_header"),
                              # plotOutput("water_plot", height = 170)
                              
                ) ##END panel
            )
            
  ), ##END TAB 2
  
  
  
  # TAB 3: SURFACE WATER  --------------------------------------------------
  nav_panel("Surface water",
            value = "tab3",
            h2("Surface Water in Kern County"),
            p("Here you can explore the changes in surface water between 2022-2023. Either select a zip code on the map to the right or enter one in the box below to see when and where surface water was present. Surface water can provide breeding habitat for mosquitos; therefore, proximity to slow-moving or standing water may result in increased mosquito abundance and mosquito-borne disease risk. For more information, please visit the Information tab."),
            ## Water map and sidebar
            card(
              height = 450,
              full_screen = TRUE, 
              layout_sidebar(
                ## remove padding around map
                class = "p-0",
                ## leaflet map
                card_body(class = "p-0", 
                          leafletOutput("waterMap"), 
                          width = "800px"
                ),
                ## sidebar
                sidebar = sidebar(
                  position="left",
                  padding = "15px",
                  textInput(inputId = "zip_box_water",
                            label = h4(strong("Zip code:")),
                            value = NULL,
                            placeholder = "Enter your zip code...")
                )
              ),##End sidebar
              card_footer("Interactive map of surface water in Kern County zip codes. The water imagery 'heat map' displays how many weeks surface water was present in a given location throughout 2022-2023, with darker blue areas having the most persistent surface water. Select a zip code to view how surface water changes over time. Base map imagery, county borders, and the water image can all be toggled using the map options in the upper left corner.")
            ),##End map card
            
            
            ## Water video and plot
            ## generates after zip code is selected
            htmlOutput("water_card")
            
            
  ),## END TAB 3
  
  
  
  # TAB 4: INFO -------------------------------------------------------------
  nav_panel(title = bs_icon("info-circle-fill"),
            value = "tab4",
            h2("More Information:"),
            
            ## Folding panels
            accordion(
              ## Mosquito panel
              accordion_panel(
                title = "Mosquito Species:",
                p("While over 50 species of mosquitos can be found in California, not all present a threat to human health. Within Kern county, there are four species of concern responsible for disease transmission. Three of these species belong to the", em("Culex"), "genus and share similar life cycles and breeding conditions.", em("Aedes aegypti"), ", however, differ in breeding habitat, biting activity, and disease transmission. More information on these mosquitos and the diseases they transmit can be found in the following section."),
                layout_columns(
                  navset_card_underline(
                    ## Culex spp
                    nav_panel(
                      title = em("Culex spp."),
                      img(src = "c_tarsalis.jpg",
                          width = "75%",
                          style = 'border: 1px solid #2d3e50;
                          margin-right: auto; margin-left: auto;'),
                      p("The majority of mosquitos in Kern County belong to the", em("Culex"), "genus. These mosquitos are the primary vectors for West Nile, St. Louis encephalitis, and westerm equine encephalitis viruses. ", em("Culex"), " mosquitos breed in agricultural, natural, and human-made water sources, such as ornamental ponds, ditches, and puddles. Females lay their eggs on the surface of standing water, and within 10-14 days these eggs develop into adult mosquitos that can fly away from the breeding site (Manimegalai & Sukanya, 2014). ", strong(em("Culex"), " mosquitos most actively bite at dawn, dusk, and after dark."))
                    ),
                    ## A. aegypti
                    nav_panel(
                      title = em("A. aegypti"),
                      img(src="a_aegypti.jpg",
                          width = "70%",
                          style = 'border: 1px solid #2d3e50;
                          margin-right: auto; margin-left: auto;'),
                      p(em("Aedes aegypti,"), " commonly known as the yellow fever mosquito, can transmit zika, chikungunya, dengue, and yellow fever viruses (CDPH, 2023B; Pliego Pliego et al, 2017). In 2023, two southern California residents were infected with locally transmitted cases of dengue fever by this mosquito species, marking the first documented cases of dengue in California not associated with travel (Associated Press, 2023; LACDPH, 2023)."),
                      p("Unlike", em("Culex"), "mosquitos, ", strong(em(" A. aegypti"), "actively bite during daytime as well as dawn and dusk."), "This species can breed in very small sources of standing water, such as bottle caps, allowing it to thrive in urban environments. Their life cycle closely follows that of ", em("Culex"), " mosquitos; however, it takes only 7-10 days for an egg to develop into an adult mosquito (CDPH, 2023B).")
                    ),
                  ),## End tabs
                  card(
                    card_header("Mosquito Life Cycle"),
                    img(src="lifecycle.png",
                        width = "100%",
                        style="margin-top: auto; margin-bottom: auto; 
                        margin-left: auto; margin-right: auto;"
                        # style = 'border: 1px solid #2d3e50'
                    )
                  )##end card
                ),##end columns
              ),## End panel
              
              ## MBD panel
              accordion_panel(
                title="Mosquito-borne Diseases:",
                navset_card_underline(
                  ## WNV tab
                  nav_panel(
                    title = "WNV:",
                    p("West Nile virus (WNV) is one of 15 known mosquito-borne diseases in California (CDPH, 2023A). In North America, WNV was first detected in New York in 1999; the virus rapidly spread across the continent, reaching southern California by 2003 and spreading to all 58 counties in the state within a year (Hartley et al., 2012; Lanciotti et al., 1999; Reisen et al., 2004). Currently, WNV is the most prevalent mosquito-borne disease in California, with over 7,500 cases and ~350 fatalties in California between 2003 and 2022 (CDPH, 2023A)."),
                    p("WNV is mainly spread by mosquitos in the ",em("Culex"), " genus (Reisen, 2013). The majority of people (8 out of 10) infected with WNV remain asymptomatic; those who do develop symptoms may experience fever, head and body aches, vomiting, and fatigue. Roughly 1 in 150 people develop serious symptoms including encephalitis or meningitis, which can result in death (CDC, 2023). ")
                  ),
                  ## SLEV tab
                  nav_panel(
                    title = "SLEV:",
                    p("St. Louis encephalitis virus (SLEV) is another mosquito-borne disease of concern in California, spread to humans through the bite of infected", em("Culex"), " mosquitos. Human cases are typically uncommon, with fewer than 10 infections reported per year in California since 1990 (CDPH, 2023A). In 2022, however, there were 16 confirmed human cases of SLEV, marking the highest number of infections since 2015 (CDPH, 2023A)."), 
                    p("Most people infected with SLEV remain asymptomatic; those who do develop symptoms may experience them between 4 to 14 days after initial infection (CDC, 2023). Symptoms may include sudden fever, headache, dizziness, and nausea lasting several days to two weeks. For some, including older adults or people with weakened immune systems, SLEV continues to develop into encephalitis or meningitis. Roughly 5-20% of those diagnosed with SLEV die as result of infection (CDC, 2023).")
                  )
                  
                ),##End tabs
                p("More information on MBD and symptoms can be found online at the ", 
                  a(href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/MosquitoesandMosquitoBorneDiseases.aspx", "California Dept. of Public Health"), 
                  "or the", 
                  a(href = "https://www.cdc.gov/mosquitoes/about/diseases.html", "Centers for Disease Control and Prevention."))
              ),##End panel
              
              ## Risk factors
              accordion_panel(
                title = "Risk factors:",
                p("The risk of a MBD infection largely depends on the proximity to infected mosquito populations. The geographic and seasonal distribution of mosquitos depends on many factors, including climate, weather, land cover, and pest control efforts. This application explores two main determinants of where and when mosquitos may breed, and how abundant biting adult mosquitos are: surface water and daily air temperature. Standing surface water provides the habitat for breeding, while air temperature determines breeding success and development time, as well as adult mosquito activity and survival. Therefore, an increased amount of standing water on the landscape and increased number of days within an optimal air temperature range may increase local mosquito populations."),
                p("Residents can take several steps to minimize exposure to mosquitos. When outdoors, wearing long-sleeve shirts and pants, as well as insect repellent, reduces the risk of being bitten. This is especially true during times of heightened mosquito activity, such as dawn and dusk, or near locations where mosquitos likely breed. Regularly emptying water-holding containers and installing secure screens on windows and doors also helps control mosquito numbers both outside and inside your home."),
                p("For more information on MBD risk factors and ways to minimize risk, you can visit the CDC's", a(href = "https://www.cdc.gov/mosquitoes/index.html", "website"), "or the", a(href = "https://www.kernmosquito.com/how-can-i-stop-prevent-nuisance-mosquitoes-and-control-the-spread-of-diseases", "Kern Mosquito and Vector Control District."))
              ), ##End panel
              
              ## Sources
              accordion_panel(
                title = "Sources:",
                tags$li(
                  ""
                ),
                tags$li(
                  "Associated Press. (2023, Nov 3). California officials confirm 2 cases of dengue, a mosquito-borne illness rarely transmitted in US. AP News. https://apnews.com/article/california-dengue-fever-cases-46a7a012e1e3566fef527b3b2be3ee0a"
                ),
                tags$li(
                  "California Department of Public Health, Mosquito & Vector control Association of California. (2023A). University of California. California Mosquito-Borne Virus Surveillance and Response Plan."
                ),
                tags$li(
                  "California Department of Public Health. (2023B). Aedes Aegypti and Aedes Albopictus Mosquitos. https://www.cdph.ca.gov/Programs/CID/DCDC/pages/Aedes-aegypti-and-Aedes-albopictus-mosquitoes.aspx"
                ),
                tags$li(
                  "Centers for Disease Control and Prevention. (2023). West Nile Virus: Symptoms, Diagnosis, and Treatment. https://www.cdc.gov/westnile/symptoms/index.html"
                ), 
                tags$li(
                  "Hartley, D. M., Barker, C. M., Le Menach, A., Niu, T., Gaff, H. D., & Reisen, W. K. (2012). Effects of Temperature on Emergence and Seasonality of West Nile Virus in California. The American Journal of Tropical Medicine and Hygiene, 86(5), 884–894. https://doi.org/10.4269/ajtmh.2012.11-0342"
                ),
                tags$li(
                  "Lanciotti, R. S. et al. (1999). Origin of the West Nile Virus Responsible for an Outbreak of Encephalitis in the Northeastern United States. Science, 286(5448), 2333–2337. https://doi.org/10.1126/science.286.5448.2333"
                ),
                tags$li(
                  "Los Angeles County Department of Public Health (2023). Dengue. http://publichealth.lacounty.gov/acd/VectorDengue.htm#:~:text=Current%20Situation%20in%20LA%20County,suspect%20cases%20have%20been%20identified."
                ),
                tags$li(
                  "Manimegalai, K., & Sukanya, S. (2014). Original Research Article Biology of the filarial vector, Culex quinquefasciatus (Diptera:Culicidae). 718–724."
                ),
                tags$li(
                  "Pliego Pliego, E., Velázquez-Castro, J., & Fraguela Collar, A. (2017). Seasonality on the life cycle of Aedes aegypti mosquito and its statistical relation with dengue outbreaks. Applied Mathematical Modelling, 50, 484–496. https://doi.org/10.1016/j.apm.2017.06.003"
                ),
                tags$li(
                  "Reisen, W. K. (2013). Ecology of West Nile Virus in North America. Viruses, 5(9), 2079–2105. https://doi.org/10.3390/v5092079"
                ),
                tags$li(
                  "Reisen, W., Lothrop, H., Chiles, R., Madon, M., Cossen, C., Woods, L., Husted, S., Kramer, V., & Edman, J. (2004). West Nile Virus in California. Emerging Infectious Diseases, 10(8), 1369–1378. https://doi.org/10.3201/eid1008.040077"
                )
              ) ##End panel
            )##End accordions
            
  ) ##END TAB 4
  
  
)## END UI


