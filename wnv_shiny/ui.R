## Read in packages
library(shiny)
library(leaflet)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(bslib)
library(bsicons)
library(plotly)


## Top bar with title and tabs
ui <- page_navbar(
  title = "MBD in Kern County",
  id = "nav",
  fillable = FALSE,
  theme = bs_theme(bootswatch = "yeti"),
  # theme = bs_theme(),
  

# navbarPage(title = "MBD in Kern County", id = "nav",
           

    ## TAB 1: WNV TRAP CASES ---------------------------------------------------
    # tabPanel("Mosquito Data",
    #          value = "tab1",
    nav_panel("Mosquito Data",
              value = "tab1",
             h2("Mosquito Abundance and Diseases",
                # style = 'margin-top:-5px; padding-left: 15px'
                ),
             p("Mosquito-borne diseases (MBD) and abundance data comes from monitoring and testing efforts of the ", tags$a(href="https://www.kernmosquito.com/", "Kern Mosquito and Vector Control District."),  "Mosquito traps are deployed at various locations in Kern county and checked regularly. These pools of trapped mosquitos are then counted and tested for a range of mosquito-borne diseases. To standardize for monitoring effort, abundance and MBD cases are reported as mosquitos per trap night and minimum infection rate (MIR), respectively. For more details on mosquito species and MBD, click on the", actionLink("link_to_info", "information icon"), "in the navigation bar.",
               # style='padding-left: 15px; padding-right: 10px'
               ),
             
  
             ##TEST BSLIB: ----
             ## Map and sidebar
             card(
               height = 500,
               full_screen = TRUE, 
               card_header(
                 popover(
                   trigger = bs_icon("bar-chart-fill", class = "ms-auto"),
                   selectInput("yvar", "Split by", c("sex", "species", "island")),
                   title = "Test"
                 ),
                 class = "d-flex align-items-center gap-1"
               ),
               layout_sidebar(
                 ## remove padding around map
                 class = "p-0",
                 ## leaflet map
                 card_body(class = "p-0", leafletOutput("trapMap")),
                 ## sidebar
                 sidebar = sidebar(
                   width = "310px",
                   position="left",
                   padding = "15px",
                   open = "always", ##change when i figure out how to open again
                   textInput(inputId = "zip_box_trap", label = h5(strong("Zip code:")),
                             value = NULL,
                             placeholder = "Enter your zip code..."
                   ),
                   p(),
                   h5(strong("Time period:")),
                   fluidRow(
                     column(width = 6, 
                            selectInput("trapYear", label = "Year:",
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
                     column(width = 6, 
                            selectInput("trapMonth", label = "Month:",
                                        choices = list(
                                          "NA" = "none",
                                          "June" = "06",
                                          "July" = "07",
                                          "August" = "08",
                                          "September" = "09",
                                          "October" = "10"
                                        ),
                                        selected = "NA"))
                   ), ##end fluidrow
                   p("Custom date range:"),
                   dateRangeInput("trap_dateRange",
                                  label = NULL,
                                  start = "2023-01-01",
                                  end = "2023-09-30",
                                  min = "2010-03-01",
                                  max = "2023-09-30")
                 ), 
               ), ##end layout_sidebar
               card_footer(
                 # class = "fs-6",
                 p("Fig 1: Interactive map of zip codes within the Central Valley and Kern County, California. The selected zip code is outline in yellow, and the border for the central valley is shown in blue. Borders for Kern County and the central valley can be toggled using the map options in the upper left corner.")
               )
             ), ##end card
             
             
             htmlOutput("twoplots")
             
    ),

             # layout_column_wrap(
             #   width = 1/2,
             #   height = 300,
             #   ## Abundance plot
             #   card(
             #     full_screen = TRUE,
             #     card_header("Abundance plot:",
             #                 tooltip(bs_icon("info-circle"),
             #                         "tooltip text")),
             #     card_body(plotOutput("abund_plot2"))
             #   ),
             #   ## MIR plots
             #   card(
             #     full_screen = TRUE,
             #     card_header("MIR plot:"),
             #     card_body(uiOutput("wnv_plot"))
             #   )
             # ),
             
             
             
             
             ### Side Panel: 
             # sidebarPanel(
             #   # h3(strong("Trap data:")),
             #   ### Zipcode input:
             #   textInput(inputId = "zip_box_trap", label = h4(strong("Zip code:")),
             #             value = NULL,
             #             placeholder = "Enter your zip code..."
             #   ),
             #   br(),
             #   
             #   h4(strong("Time period:")),
             #   # p("Quick selection:"),
             #   fluidRow(   ## put both input boxes in-line
             #     column(width = 6, selectInput("trapYear", label = "Year:",
             #                                   choices = list("2023" = "2023",
             #                                                  "2022" = "2022",
             #                                                  "2021" = "2021",
             #                                                  "2020" = "2020",
             #                                                  "2019" = "2019",
             #                                                  "2018" = "2018",
             #                                                  "2017" = "2017",
             #                                                  "2016" = "2016",
             #                                                  "2015" = "2015",
             #                                                  "2014" = "2014",
             #                                                  "2013" = "2013",
             #                                                  "2012" = "2012",
             #                                                  "2011" = "2011", 
             #                                                  "2010" = "2010"),
             #                                   selected = 2023)),
             #     column(width = 6, selectInput("trapMonth", label = "Month:",
             #                                   choices = list("NA" = "none",
             #                                                  "June" = "06",
             #                                                  "July" = "07",
             #                                                  "August" = "08",
             #                                                  "September" = "09",
             #                                                  "October" = "10"),
             #                                   selected = "NA"))
             #   ),
             #   p("Custom date range:"),
             #   dateRangeInput("trap_dateRange", 
             #                  label = NULL,
             #                  start = "2023-01-01",
             #                  end = "2023-09-30",
             #                  min = "2010-03-01",
             #                  max = "2023-09-30"),
             # ), ### End side panel
             
             ### Interactive map:
             # mainPanel(
             #   leafletOutput("trapMap", height = "380px"),
             #   htmlOutput("trapMap_caption"),
             #   br(), br()),
             
             
             
             ### Plots
             # fluidRow(
               # column(width = 6, uiOutput("abund_plot"),
               #        bsTooltip("abund_plot", 
               #                  "Test tooltip plz work!",
               #                  placement = "top"
               #                  )
               #        ),
             #   column(width = 6, uiOutput("wnv_plot"),
             #          uiOutput("slev_plot"))
             # ),
             # fluidRow(
             #   column(width = 6, style='padding-left:50px;', 
             #          htmlOutput("abundPlot_caption")),
             #   column(width = 6, style='padding-left:55px;',
             #          htmlOutput("wnvPlot_caption"))
             # ),
             # br()
             # ), ## END TAB 1
    
    
    
    ## Test tab 1.5 ________________
    # tabPanel("test_tab",
    #          page_fillable(
    #            ##test card
    #            card(
    #              height = 250,
    #              full_screen = TRUE,
    #              card_header("Header",
    #                          tooltip(
    #                            bsicons::bs_icon("question-circle"),
    #                            "tooltip info here!",
    #                            placement = "right"
    #                          )
    #                          ),
    #              card_body(uiOutput("abund_plot"))
    #            )
    #          )
    # 
    #          
    #          ), ## end test tab
    
    
    
    
           
    ## TAB 2: WNV Interactive Map ----------------------------------------------  
  nav_panel(title = "Risk Map",
            value = "tab2",
                

# tabPanel("Risk Map",
#              id = "tab2",
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
                          top = 50, left = "auto", right = 20, bottom = "auto",
                          width = 350, height = "auto",
              
              ## Zip code
              textInput(inputId = "zip_box", label = h4("Zip code:"),
                        value = NULL,
                        placeholder = "Enter your zip code..."
                        ),
              
              ## Date range:
              dateRangeInput("risk_dateRange",
                             label = h4("Date range:"),
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
   nav_panel("Standing water",
             value = "tab3",
             
   # tabPanel("Surface water",
   #           value = "tab3",
             h2("Surface Water in Kern County"),
                # style = 'margin-top:-5px; padding-left: 15px'),
                p("Here you can explore the changes in surface water between 2022-2023. Either select a zip code on the map to the right or enter one in the box below to see when and where surface water was present. Surface water can provide breeding habitat for mosquitoes; therefore, proximity to slow-moving or standing water may result in increased mosquito abundance and mosquito-borne disease risk. For more information, please visit the 'Info' tab."),
   ## Map and sidebar
   card(
     height = 370,
     full_screen = TRUE, 
     layout_sidebar(
       ## remove padding around map
       class = "p-0",
       ## leaflet map
       card_body(class = "p-0", leafletOutput("waterMap"), width = "800px"),
       ## sidebar
       sidebar = sidebar(
         # width = "310px",
         position="left",
         padding = "15px",
         # open = "always",
         textInput(inputId = "zip_box_water",
                   label = h4(strong("Zip code:")),
                   value = NULL,
                   placeholder = "Enter your zip code...")
         ),
       ), 
   ), ##end card
   
             # sidebarPanel(
             #   p("Here you can explore the changes in surface water between 2022-2023. Either select a zip code on the map to the right or enter one in the box below to see when and where surface water was present. Surface water can provide breeding habitat for mosquitoes; therefore, proximity to slow-moving or standing water may result in increased mosquito abundance and mosquito-borne disease risk. For more information, please visit the 'Info' tab."),
             #   br(),
             #   ## Zip code
             #   textInput(inputId = "zip_box_water",
             #             label = h4(strong("Zip code:")),
             #             value = NULL,
             #             placeholder = "Enter your zip code...")
             # ),##End sidebarpanel
             
             ## Interactive Map
             # mainPanel(
             #   leafletOutput("waterMap", height = "350px", width = "800px"),
             #   br()
             # ),
             
             # ## Video and plot
             # fluidRow(
             #   column(width = 5,
             #          uiOutput("waterVid"),
             #          style = 'padding-left: 30px'),
             #   column(width = 7,
             #          # style='padding-left: -1700px',
             #          uiOutput("waterTab_plot"))
             # )
             
   # layout_columns(
   #   uiOutput("waterVid"),
   #   card(
   #     full_screen = TRUE,
   #     card_body(uiOutput("waterTab_plot"))
   #   )
   # )
   
   htmlOutput("water_test")
   # card(
   #   layout_columns(
   #     uiOutput("waterVid"),
   #     uiOutput("waterTab_plot")
   #   )
   # )
   
       # layout_column_wrap(
       #   # width = 1/2,
       #   height = "450px",
       #   ## water video
       #   card(
       #     full_screen = TRUE,
       #     card_body(uiOutput("waterVid"))
       #   ),
       #   ## water plot
       #   card(
       #     full_screen = TRUE,
       #     card_body(uiOutput("waterTab_plot"))
       #   )
       # )

          ), ## END TAB 3
    
    
    
    
    ## TAB 4: INFO -------------------------------------------------------------
  nav_panel(title = bs_icon("info-circle-fill"),
            value = "tab4",
            # icon = bs_icon("info-circle-fill"),
              
  # tabPanel(title = NULL, 
  #            icon = bs_icon("info-circle-fill"),
  #            # icon = icon("info-circle", "fa-1.5x"),
  #            value = "tab4",
               mainPanel(width = 10,
                         h2("Information:",
                            style = 'margin-top:-5px')
               ),
             ## Folding panels
             accordion(
               open = c("Mosquitoes"),
               accordion_panel(
                 "Mosquito Species:",
                 p("While over 50 species of mosquitoes can be found in California, not all present a threat to human health. Within Kern county, there are four species of concern responsible for disease transmission. Three of these species belong to the", em("Culex"), "genus and share similar life cycles and breeding conditions.", em("A. aegypti"), ", however, differ in breeding habitat and biting activity. More information on these mosquito-borne diseases can be found in the following section."),
                 navset_card_underline(
                  nav_panel(title = p(em("Culex"), "mosquitoes"),
                            img(src="c_tarsalis.jpg",
                                # height = "250px",
                                width = "30%",
                                style = 'border: 1px solid #2d3e50'), 
                            p("The majority of mosquitoes in Kern County belong to the", em("Culex"), "genus. These mosquitoes are the primary vectors for West Nile, St. Louis encephalitis, and equine encephalitis viruses. ", em("Culex"), " mosquitoes breed in agricultural, natural, and human-made water sources, such as ornamental ponds, ditches, and puddles. Females lay their eggs on the surface of standing water, and within 10-14 days these eggs develop into adult mosquitoes that can fly away from the breeding site (Manimegalai & Sukanya, 2014). ", em("Culex"), " mosquitoes are most active at dawn, dusk, and after dark.")
                            ),
                  nav_panel(title = p(em("A. aegypti")),
                            img(src="a_aegypti.jpg",
                               # height = "250px",
                               width = "30%",
                               style = 'border: 1px solid #2d3e50'), 
                            # p(),
                            p(em("Aedes aegypti"), ", commonly known as the Asian tiger mosquito, may transmit zika, chikungunya, dengue, and yellow fever viruses (CDPH, 2023B; Pliego Pliego et al, 2017). Unlike", em("Culex"), "mosquitoes,", em("A. aegypti"), "are active during daytime as well as dawn and dusk. This species can breed in very small sources of standing water, such as bottle caps, allowing it to thrive in urban environments. Their life cycle closely follows that of ", em("Culex"), " mosquitoes; however, it takes only 7-10 days for an egg to develop into an adult mosquito (CDPH, 2023B).")
                            ),
                  nav_spacer(),
                  # nav_menu(
                  #   title = "Links",
                  #   nav_item("Future links to more info here")
                  # )
                 )## End tabs
               ),## End panel
               
               accordion_panel(
                 title="Mosquito-borne Diseases:",
                 navset_card_underline(
                   nav_panel(title = "WNV:",
                             p("West Nile virus (WNV) is one of 15 known mosquito-borne diseases in California (CDPH, 2023A). In North America, WNV was first detected in New York in 1999; the virus rapidly spread across the continent, reaching southern California by 2003 and spreading to all 58 counties in the state within a year (Hartley et al., 2012; Lanciotti et al., 1999; Reisen et al., 2004). Currently, WNV is the most prevalent mosquito-borne disease in California, with over 7,500 cases 345 fatalties in California between 2003 and 2022 (CDPH, 2023A)."),
                             p("WNV is mainly spread by mosquitoes in the ",em("Culex"), " genus (Boser et al., 2021). The majority of people (8 out of 10) infected with WNV remain asymptomatic; those who do develop symptoms may experience fever, head and body aches, vomiting, and fatigue. Roughly 1 in 150 people develop serious symptoms including encephalitis or meningitis, which can result in death (CDC, 2023). ")
                             ),
                   nav_panel(title = "SLEV:",
                             p("St. Louis encephalitis virus (SLEV) is another mosquito-borne disease of concern in California, spread to humans through the bite of infected", em("Culex"), " mosquitoes. Human cases are typically uncommon, with fewer than 10 infection reported per year in California since 1990 (CDPH, 2023A). In 2022, however, there were 16 confirmed human cases of SLEV, marking the highest number of infections since 2015 (CDPH, 2023A)."), 
                             p("Most people infected with SLEV remain asymptomatic; those who do develop symptoms may experience them between 4 to 14 days after initial infection (CDC, 2023). Symptoms may include sudden fever, headache, dizziness, and nausea lasting several days to two weeks. For some, including older adults or people with weakened immune systems, SLEV continues to develop into encephalitis or meningitis. Roughly 5-20% of those diagnosed with SLEV die as result of infection (CDC, 2023).")
                             )

                 ),

                #  h5(strong("WNV:")),
                #  p("West Nile virus (WNV) is one of 15 known mosquito-borne diseases in California (CDPH, 2023A). In North America, WNV was first detected in New York in 1999; the virus rapidly spread across the continent, reaching southern California by 2003 and spreading to all 58 counties in the state within a year (Hartley et al., 2012; Lanciotti et al., 1999; Reisen et al., 2004). Currently, WNV is the most prevalent mosquito-borne disease in California, with over 7,500 cases 345 fatalties in California between 2003 and 2022 (CDPH, 2023A)."),
                #  p("WNV is mainly spread by mosquitoes in the ",em("Culex"), " genus (Boser et al., 2021). The majority of people (8 out of 10) infected with WNV remain asymptomatic; those who do develop symptoms may experience fever, head and body aches, vomiting, and fatigue. Roughly 1 in 150 people develop serious symptoms including encephalitis or meningitis, which can result in death (CDC, 2023). "),
                #  br(),
                #  h5(strong("SLEV:")), 
                # p("St. Louis encephalitis virus (SLEV) is another mosquito-borne disease of concern in California, spread to humans through the bite of infected", em("Culex"), " mosquitoes. Human cases are typically uncommon, with fewer than 10 infection reported per year in California since 1990 (CDPH, 2023A). In 2022, however, there were 16 confirmed human cases of SLEV, marking the highest number of infections since 2015 (CDPH, 2023A)."), 
                # p("Most people infected with SLEV remain asymptomatic; those who do develop symptoms may experience them between 4 to 14 days after initial infection (CDC, 2023). Symptoms may include sudden fever, headache, dizziness, and nausea lasting several days to two weeks. For some, including older adults or people with weakened immune systems, SLEV continues to develop into encephalitis or meningitis. Roughly 5-20% of those diagnosed with SLEV die as result of infection (CDC, 2023)."),
                p("More information on MBD and symptoms can be found online at the ", a(href = "https://www.cdph.ca.gov/Programs/CID/DCDC/Pages/MosquitoesandMosquitoBorneDiseases.aspx", "California Dept. of Public Health"), "or the", a(href = "https://www.cdc.gov/mosquitoes/about/diseases.html", "Center for Disease Control and Prevention."))
                  ),##End panel
               
               accordion_panel(
                 title = "Risk factors:",
                 p("Unsurprisingly, the risk of a MBD infection largely depends on the proximity to infected mosquito populations. The geographic and seasonal distribution of mosquitoes depends on many factors, including climate, weather, land cover, and pest control efforts. This application explores two main determinants of where and when mosquitoes may breed: surface water and daily air temperature. Standing surface water provides the habitat for breeding, while optimal air temperature provides the conditions for breeding and survival. Therefore, an increased amount of standing water on the landscape and increased number of days within an optimal air temperature range may increase local mosquito populations."),
                 p("Residents can take several steps to minimize exposure to mosquitoes. When outdoors, wearing long-sleeve shirts and pants, as well as insect repellent, reduces the risk of being bitten. This is especially true during times of heightened mosquito activity, such as dawn and dusk, or near locations where mosquitoes likely breed. Regularly emptying water-holding containers and installing secure screens on windows and doors also helps control mosquito numbers both outside and inside your home."),
                 p("For more information on MBD risk factors and ways to minimize risk, you can visit the CDC's", a(href = "https://www.cdc.gov/mosquitoes/index.html", "website"), "or the", a(href = "https://www.kernmosquito.com/how-can-i-stop-prevent-nuisance-mosquitoes-and-control-the-spread-of-diseases", "Kern Mosquito and Vector Control District."))
               ), ##End panel
               
               accordion_panel(
                 title = "Sources:",
                 tags$li(
                   "Beyer, H. L., Dujardin, Y., Watts, M. E., & Possingham, H. P. (2016). Solving conservation planning problems with integer linear programming. Ecological Modelling, 328, 14–22. https://doi.org/10.1016/j.ecolmodel.2016.02.005"
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
                   "Manimegalai, K., & Sukanya, S. (2014). Original Research Article Biology of the filarial vector, Culex quinquefasciatus (Diptera:Culicidae). 718–724."
                 ),
                 tags$li(
                   "Pliego Pliego, E., Velázquez-Castro, J., & Fraguela Collar, A. (2017). Seasonality on the life cycle of Aedes aegypti mosquito and its statistical relation with dengue outbreaks. Applied Mathematical Modelling, 50, 484–496. https://doi.org/10.1016/j.apm.2017.06.003"
                 ),
                 tags$li(
                   "Reisen, W., Lothrop, H., Chiles, R., Madon, M., Cossen, C., Woods, L., Husted, S., Kramer, V., & Edman, J. (2004). West Nile Virus in California. Emerging Infectious Diseases, 10(8), 1369–1378. https://doi.org/10.3201/eid1008.040077"
                 )
               ) ##End panel
             )##End accordions
             
    ) ##END PAGE 4

    
) ## END UI
  
  
# ) ## End page-fillable (?)
   
