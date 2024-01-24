### READ IN DATA AND PACKAGES

library(shiny)
library(bslib)
library(bsicons)
library(shinyalert)    ## Modals
library(shinyvalidate) ## Text box validation messages
library(tidyverse)     ## Always
library(here)          ## Easier reading/writing data
library(ggiraph)       ## interactive plots
library(lubridate)     ## Wrangling/plotting dates
library(leaflet)       ## Interactive map
library(raster)        ## Leaflet-friendly raster pkg
library(sf)            ## Leaflet-friendly vector pkg

## Rasters
# wnv_trans <- raster(here('data/Kern_transmission_raster_wgs84.tif'))
# water <- raster(here('data/water/water_reproj.tif'))

## Vectors
zips_sf <- st_read(here('data/zipcodes/kern_zips.shp'))
kern_sf <- st_read(here('data/counties_ca/kern.shp'))
valley_sf <- st_read(here('data/central_valley/valley.shp'))

## Data frames
water_zip_df <- read_csv(here("data/water/water_acre_zipcode.csv"))
r0_zip_df <- read_csv(here("data/transmission_efficiency_zipcodes.csv"))
wnv_df <- read_csv(here("data/traps/plotting/wnvMIR_plotting.csv"))
slev_df <- read_csv(here("data/traps/plotting/slevMIR_plotting.csv"))
abund_df <- read_csv(here('data/traps/plotting/abundance_plotting.csv')) %>% 
  janitor::clean_names()
temp_zip_df <- read_csv(here('data/temp/kern_tmean_20100401_20230930.csv')) %>% 
  mutate(cx_opt = factor(cx_opt),
         cx_opt = fct_relevel(cx_opt, "optimal", "in range", "out range"))

## Raster color palette
# pal <- colorNumeric(palette = 'viridis', domain = values(wnv_trans),
#                     reverse = TRUE,
#                     na.color = "transparent")

# START SERVER
function(input, output, session) {
  
  ## WELCOME MODAL -----------------------------------------------------------
  
  # ## Initial modal on app launch
  # shinyalert(
  #   title = "Welcome",
  #   text = paste0("This app lets you explore the risks and hazards associated with mosquito-borne diseases (MBD) in Kern County, California. Information by zip code* can be explored by either manually entering the zip code of interest, or by clicking the location on the map.", "<br>", "<br>", "For more information on MBD transmission, select the 'More Info' button or click on the information icon at the top of the page.", "<br>", "<br>", "<span style='font-size: 13px;'>", "<i>", "*(Note: Zip code boundaries have been limited to the extent present within both Kern County and the California Central Valley. These boundaries can be visually toggled by (un)selecting layers listed at the top left of the map.", "</i>", ")",  "</span>"),
  #   size = "m", 
  #   closeOnEsc = TRUE,
  #   closeOnClickOutside = TRUE,
  #   html = TRUE,
  #   type = "",
  #   showConfirmButton = TRUE,
  #   showCancelButton = TRUE,
  #   confirmButtonText = "More Info",
  #   confirmButtonCol = "#AEDEF4",
  #   cancelButtonText = "View Map",
  #   timer = 0,
  #   imageUrl = "",
  #   animation = TRUE
  # )
  # 
  # ## Go to "info" tab if button pressed
  # observeEvent(input$shinyalert, {
  #   if (input$shinyalert == TRUE) {
  #     updateNavbarPage(session, "nav", selected = "tab4")
  #   }
  # })
  
  # TAB 1 - Trap Data ##########################################################
  observeEvent(input$link_to_info, {
    newValue <- "tab4"
    updateTabItems(session, "nav", newValue)
  })
  
  ## RESPONSIVE SIDE PANEL WIDGETS --------------------
  
  ### Zip code box ----------------
  ## React to user input and slightly delay response
  zipcodeTrap <- reactive(input$zip_box_trap)
  zipcodeTrap_d <- debounce(zipcodeTrap, millis = 1500)
  
  ## Validation text below zip box
  ivTrap <- InputValidator$new()
  ### Must be 5 numbers
  ivTrap$add_rule("zip_box_trap", function(length) {
    length = nchar(zipcodeTrap_d())
    if (length !=5 & length != 0) {
      "Only 5-character entries are permitted."
    }
  })
  ### Must be zip in Kern
  ivTrap$add_rule("zip_box_trap", function(zipcodeTrap_d) {
    if (!(zipcodeTrap_d() %in% zips_sf$zipcode) & nchar(zipcodeTrap_d()) != 0) {
      "Please enter a valid zip code."
    } 
  })
  ivTrap$enable()
  
  
  ### Trap date selection ----------------
  observe({
    if (input$trapMonth == "none" & input$trapYear == "2023") {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, "01-01", sep = "-"),
                           end = paste(input$trapYear, "09-30", sep = "-"))
    } else if (input$trapMonth == "none" & input$trapYear == "2010") {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, "03-01", sep = "-"),
                           end = paste(input$trapYear, "12-31", sep = "-"))
    } else if (input$trapMonth == "none") {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, "01-01", sep = "-"),
                           end = paste(input$trapYear, "12-31", sep = "-"))
    } else {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, input$trapMonth, "01", sep = "-"),
                           end = paste(input$trapYear, input$trapMonth, "31", sep = "-"))
    }
  })  
  
  
  ## TRAP MAP ------------------------------------------------------------
  output$trapMap <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      ### Zip codes
      addPolygons(data = zips_sf, color = "#343434",
                  weight = 2, fillOpacity = 0.05,
                  label = paste0("Zip code: ", zips_sf$zipcode),
                  labelOptions = labelOptions(textsize = "11px"),
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes",
                  layerId = ~zipcode) %>%  
      
      ### Kern county
      addPolylines(data = kern_sf,
                   color = 'black', weight = 4, fillOpacity = 0,
                   group = "Kern county") %>% 
      
      ### Central valley
      addPolylines(data = valley_sf,
                   color = 'blue', weight = 2.5, fillOpacity = 0,
                   group = "Central Valley") %>%
      
      ## Create map groups
      addLayersControl(
        baseGroups = c("OpenStreetMaps", "World Imagery"),
        overlayGroups = c("Zip codes", "Kern county", "Central Valley"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      
      ## Don't show all layers by default
      hideGroup(c("Kern county")) %>% 
      
      ## Add map inset
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        zoomLevelOffset = -5,
        position = 'bottomright',
        toggleDisplay = TRUE) %>% 
      
      setView(-119.2, 35.38, zoom = 9)
  }) ## END LEAFLET
  
  
  ### Interactive Leaflet elements:  -----------------------
  ## Click on zip code polygon to input value in text box
  observe({
    event <- input$trapMap_shape_click
    if(is.null(event$id))
      return()
    
    ## change text box value
    updateTextInput(session, 
                    inputId = "zip_box_trap", 
                    value = event$id)
  })
  
  
  ## Zoom and highlight zip code
  observe({
    ## establish zip code boundaries
    geom <- zips_sf %>%
      dplyr::filter(zipcode == input$zip_box_trap)
    bounds <- geom %>%
      st_bbox() %>%
      as.character()
    
    ## Update map
    leafletProxy("trapMap") %>%
      clearGroup("highlighted_polygon") %>%
      ## zoom to zip
      flyToBounds(lng1 = bounds[1], lat1 = bounds[2],
                  lng2 = bounds[3], lat2 = bounds[4]) %>%
      ## highlight selected zip
      addPolylines(stroke=TRUE, weight = 5, color="yellow",
                   fill = TRUE, fillColor = "white", fillOpacity = 0.4,
                   data = geom, group = "highlighted_polygon")
  })
  
  ## TEST UPDATE COLORS FOR WNV
  # observe({
  #   ## List w/pos MIR during time range
  #   wnvPos <- wnv_df %>% 
  #     dplyr::filter(date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2]) %>% 
  #     dplyr::filter(mir_all > 0) %>% 
  #     dplyr::filter(!is.na(zipcode))
  #   
  #   slevPos <- slev_df %>% 
  #     dplyr::filter(date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2]) %>% 
  #     dplyr::filter(mir_all > 0) %>% 
  #     dplyr::filter(!is.na(zipcode))
  #   
  #   ## Filter zips to keep those with pos WNV
  #   wnvZips <- zips_sf %>% 
  #     dplyr::filter(zipcode %in% wnvPos$zipcode)
  #   
  #   slevZips <- zips_sf %>% 
  #     dplyr::filter(zipcode %in% slevPos$zipcode)
  #   
  #   
  #   ##update map
  #   leafletProxy("trapMap") %>% 
  #     clearGroup("wnvPos") %>% 
  #     addPolygons(stroke = TRUE, weight = 2, color = "#8b4726",
  #                 fill = TRUE, fillColor = "#ee7942", fillOpacity = 0.4,
  #                 label = paste0("Zip code: ", wnvZips$zipcode),
  #                 labelOptions = labelOptions(textsize = "11px"),
  #                 highlight = highlightOptions(weight = 5,
  #                                              color = "white",
  #                                              bringToFront = TRUE),
  #                  data = wnvZips, group = "wnvPos",
  #                 layerId = ~zipcode)
  #   
  # })
  
  
  ## Reactive caption for map
  output$trapMap_caption <- renderText({
    paste("<b>","Figure 1:","</b>","Interactive map of zip codes within the Central Valley and Kern County, California. The selected zip code is outline in yellow, and the border for the central valley is shown in blue. Borders for Kern County and the central valley can be toggled using the map options in the upper left corner.")
  })
  
  
  
  ## Filter Trap Data ----------------------------------------------------
  ## Abundance -----------------
  ### Filtered abundance by time and zip
  abund_data <- reactive ({
    df <- abund_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(abund_woy_avg = mean(mos_per_trap_night, na.rm = TRUE))
    
    return(df)
  })
  
  ### Avg abund for zip (all time)
  avgAbund_zip <- reactive ({
    x <- abund_df %>% 
      filter(zipcode == zipcodeTrap_d())
    
    xMean <- mean(x$mos_per_trap_night, na.rm = TRUE)
    
    return(xMean)
  })
  
  ### Avg abund for time (all Kern)
  avgAbund_kern <- reactive ({
    x <- abund_df %>% 
      filter(date >= input$trap_dateRange[1] & date <= input$trap_dateRange[2])
    
    xMean <- mean(x$mos_per_trap_night, na.rm = TRUE)
    
    return(xMean)
  })
  
  
  ## WNV MIR  --------------------
  ### Filtered MIR by time and zip
  wnv_data <- reactive ({
    df <- wnv_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date >= input$trap_dateRange[1] & date <= input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(mir_avg = mean(mir_all, na.rm = TRUE))
    return(df)
  })
  
  ### Avg MIR for zip (all time)
  avgWnv_zip <- reactive ({
    x <- wnv_df %>% 
      filter(zipcode == zipcodeTrap_d())
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  ### Avg MIR for time (all Kern)
  avgWnv_kern <- reactive ({
    x <- wnv_df %>% 
      filter(date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2])
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  
  ## SLEV MIR  --------------------
  ### Filtered slev by time and zip
  slev_data <- reactive ({
    df <- slev_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date >= input$trap_dateRange[1] & date <= input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(mir_avg = mean(mir_all, na.rm = TRUE))
    return(df)
  })
  
  ### Avg SLEV for zip (all time)
  avgSlev_zip <- reactive ({
    x <- slev_df %>% 
      filter(zipcode == zipcodeTrap_d())
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  ### Avg abund for time (all Kern)
  avgSlev_kern <- reactive ({
    x <- slev_df %>% 
      filter(date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2])
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  
  ### Plots ----------------------------------------------------
 
  ##test plot
  # output$abund_plot <- renderUI({
  #   card(
  #     card_header(
  #       "Mosquito abundance:",
  #       tooltip(
  #         bsicons::bs_icon("info-circle"),
  #         "Info on abundance metric.",
  #         placement = "right"
  #       )
  #     ),
  #     renderPlot({
  #       ggplot(data = abund_data(), aes(x = date, y = cases)) +
  #         ## Kern avg (in time period):
  #         geom_hline(yintercept = avgAbund_kern(),
  #                    color = "black", linetype = "dashed", linewidth = 0.8) +
  #         
  #         ## Zip avg (all time):
  #         geom_hline(yintercept = avgAbund_zip(),
  #                    color = "purple", linetype = "dashed", linewidth = 0.8) +
  #         ## Filtered zip/time data
  #         geom_line(color = "seagreen4", linewidth = 0.8) +
  #         geom_point(color = "seagreen3", size = 3, alpha = 0.8) +
  #         labs(y = "Average weekly abundance",
  #              x = element_blank()) +
  #         scale_x_date(date_labels = "%d %b %y",
  #                      breaks = unique(abund_data()$date)) +
  #         theme_classic() +
  #         theme(
  #           axis.title.y = element_text(vjust = 2, size = 14),
  #           axis.title.x = element_text(vjust = -1, size = 14),
  #           axis.text = element_text(size = 13)) 
  #     })
  #   )
  #   
  # })
  
  
  output$twoplots <- renderUI({
    if (!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
      ## If no data, no plot
    } else {
    layout_column_wrap(
      width = 1/2,
      height = "470px",
      ## Abundance plot
      card(
        full_screen = TRUE,
        card_header("Mosquito abundance:",
                    popover(
                      trigger = bs_icon("question-circle"),
                      placement = "right",
                      "Due to the large number of traps in Kern county and variability in collection frequency, mosquito abundance is standardized as “mosquitoes per trap night”, or:",
                      withMathJax("$$\\scriptsize{\\frac{\\text{Total number individuals}}{\\text{Number nights since last collection}}}$$"),
                      "This value is then averaged across all traps in a given area (zip code or county) by week. As such, reported average abundance values do not represent the total number of mosquitoes present in a week, but rather a standardized metric that can be used to track relative changes in abundance over time.")
                    ),
        card_body(uiOutput("abund_plot"),
                  fill = TRUE,
                  padding = "5px"
                  ),
        # card_footer(
        #   htmlOutput("abundPlot_caption")
        # )
      ), 
      ## MIR plots
      card(
        full_screen = TRUE,
        card_header("Infection rates:",
                    popover(
                      trigger = bs_icon("question-circle"),
                      placement = "right",
                      "Minimum infection rate (MIR) is a common method used to estimate infection rates in mosquito populations. Mosquitoes are trapped and tested in groups, or “pools”. MIR assumes that if a tested pool comes back positive for a MBD, only one mosquito in the pool is infected. Reported MIR values can be interpreted as:  “At minimum, X number of mosquitoes were infected during week Y.” ",
                      withMathJax("$$\\scriptsize{MIR = \\frac{\\text{Number Positive Pools}} {\\text{Total Number Individuals}}*1000}$$"),
                      )
                    ),
        card_body(
          uiOutput("mir_plots")
        )
      )
    )
    }
  })
  
  
  
   ## Abundance plot
  output$abund_plot <- renderUI({
    if (length(abund_data()$abund_woy_avg)==0) {
      strong("No abundance data available for this location and time.")
      ## If plotting under two months
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 62) {
      renderPlotly({
        p <- ggplot(data = abund_data(), aes(x = date, y = abund_woy_avg)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +

          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 3, alpha = 0.8,
                     aes(text = paste0("Week:  ", date, "<br>",
                                      "Average abundance:  ",round(abund_woy_avg,2))
                     )) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%d %b %y",
                       breaks = unique(abund_data()$date)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10))
       
        ggplotly(p, tooltip = c("text")) %>% 
           config(displaylogo = FALSE,
                  modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                             "hoverClosestCartesian",
                                             "hoverCompareCartesian"))
      })
      ## If plotting between 2 months to a year
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 365) {
      renderPlotly({
       p <- ggplot(data = abund_data(), aes(x = date, y = abund_woy_avg)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 3, alpha = 0.8,
                     aes(text = paste0("Week:  ", date, "<br>",
                                      "Average abundance:  ",round(abund_woy_avg,2))
                     )) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%b %y",
                       date_breaks = "1 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10))
       
       ggplotly(p, tooltip = c("text")) %>% 
         config(displaylogo = FALSE,
                modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian"))
      })
      ## If plotting between a year and two years
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 730) {
      renderPlotly({
        p <- ggplot(data = abund_data(), aes(x = date, y = abund_woy_avg)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 2, alpha = 0.8,
                     aes(text = paste0("Week:  ", date, "<br>",
                                      "Average abundance:  ",round(abund_woy_avg,2))
                     )) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "2 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10))
        
        ggplotly(p, tooltip = c("text")) %>%  
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                            "hoverClosestCartesian",
                                            "hoverCompareCartesian"))
      })
      ## If plot time is greater than two years
    }  else {
      renderPlotly({
        p <- ggplot(data = abund_data(), 
                    aes(x = date, y = abund_woy_avg)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 1.5, alpha = 0.8,
                     aes(text = paste0("Week:  ", date, "<br>",
                                  "Average abundance:  ",round(abund_woy_avg,2))
                         )) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "3 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10))
        
        ggplotly(p, tooltip = c("text")) %>% 
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                            "hoverClosestCartesian",
                                            "hoverCompareCartesian"))
      })
    }
  })

  ## Test combo
  output$mir_plots <- renderPlotly({
    wnv <- ggplotly(
      ggplot(data = wnv_data(), aes(x = date, y = mir_avg)) +
      ## Filtered to zip/time
      geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
      ## Kern avg (in time period):
      geom_hline(yintercept = avgWnv_kern(),
                 color = "black", linetype = "dashed", linewidth = 0.8) +
      ## Zip avg (all time):
      geom_hline(yintercept = avgWnv_zip(),
                 color = "purple", linetype = "dashed", linewidth = 0.8) +
      labs(y = "Weekly WNV MIR",
           x = element_blank()) +
      scale_x_date(date_labels = "%b %y",
                   date_breaks = "1 month")+
      # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme_classic() +
      theme(
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text = element_text(size = 10)) 
    )
    
    slev <- ggplotly(
      ggplot(data = slev_data(), aes(x = date, y = mir_avg)) +
        ## Filtered to zip/time
        geom_col(fill = "#fff9ae", color = "#a98600", alpha = 0.7) +
        ## Kern avg (in time period):
        geom_hline(yintercept = avgSlev_kern(),
                   color = "black", linetype = "dashed", linewidth = 0.8) +
        ## Zip avg (all time):
        geom_hline(yintercept = avgSlev_zip(),
                   color = "purple", linetype = "dashed", linewidth = 0.8) +
        labs(y = "Weekly SLEV MIR",
             x = element_blank()) +
        scale_x_date(date_labels = "%b %y",
                     date_breaks = "1 month")+
        # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_classic() +
        theme(
          axis.title.y = element_text(vjust = 2, size = 14),
          axis.text = element_text(size = 10)) 
    )
    
    subplot(wnv, slev, nrows = 2, margin = 0.05, titleY = TRUE)
  })
  
  
  
  ## WNV plot
  wnv_plot <- reactive({
    ## Under two months
    if (difftime(input$trap_dateRange[2],
                 input$trap_dateRange[1]) <= 62) {
      ggplotly(
        ggplot(data = wnv_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week: ", date, "<br>",
                                "Average MIR: ",
                                round(mir_avg, 3)))) +
          ## Filtered to zip/time
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly WNV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%d %b %y")+
          # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10)),
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    ## Between 2mo and 1yr
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 365) {
      ggplotly(
        ggplot(data = wnv_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week: ", date, "<br>",
                                "Average MIR: ",
                                round(mir_avg, 3)))) +
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly WNV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b %y",
                       date_breaks = "1 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10)),
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    ## Between 1yr and 2yrs
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 730) {
      ggplotly(
        ggplot(data = wnv_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week: ", date, "<br>",
                                "Average MIR: ",
                                round(mir_avg, 3)))) +
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly WNV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "2 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10)),
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    ## Plot time over two years
    } else {
      ggplotly(
        ggplot(data = wnv_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week:  ", date, "<br>",
                                "Average MIR:  ",
                                round(mir_avg, 3)))) +
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly WNV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "3 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10)),
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    }
  })
  
  
  ## SLEV plot
  slev_plot <- reactive({
    if (difftime(input$trap_dateRange[2],
                 input$trap_dateRange[1]) <= 62) {
      ## Under 2mos
      ggplotly(
        ggplot(data = slev_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week:  ", date, "<br>",
                                "Average MIR:  ",
                                round(mir_avg, 3)))) +
          ## Filtered to zip/time
          geom_col(fill = "#fff9ae", color = "#a98600", alpha = 0.7) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgSlev_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgSlev_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly SLEV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%d %b %y")+
          # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10)),
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    ## Between 2mo and 1yr
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 365) {
      ggplotly(
        ggplot(data = slev_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week:  ", date, "<br>",
                                "Average MIR:  ",
                                round(mir_avg, 3)))) +
          geom_col(fill = "#fff9ae", color = "#a98600", alpha = 0.7) +
          geom_hline(yintercept = avgSlev_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          geom_hline(yintercept = avgSlev_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly SLEV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b %y",
                       date_breaks = "1 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10)), 
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    ## Between 1yr and 2yrs
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 730) {
      ggplotly(
        ggplot(data = slev_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week:  ", date, "<br>",
                                "Average MIR:  ",
                                round(mir_avg, 3)))) +
          geom_col(fill = "#fff9ae", color = "#a98600", alpha = 0.7) +
          geom_hline(yintercept = avgSlev_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          geom_hline(yintercept = avgSlev_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly SLEV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "2 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10)),
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    ## Over 2yrs
    } else {
      ggplotly(
        ggplot(data = slev_data(), 
               aes(x = date, y = mir_avg,
                   text = paste0("Week:  ", date, "<br>",
                                "Average MIR:  ",
                                round(mir_avg, 3)))) +
          geom_col(fill = "#fff9ae", color = "#a98600", alpha = 0.7) +
          geom_hline(yintercept = avgSlev_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.5) +
          geom_hline(yintercept = avgSlev_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.5) +
          labs(y = "Weekly SLEV MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "3 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 10),
            axis.text = element_text(size = 10)),
        tooltip = c("text")
      ) %>% 
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                          "hoverClosestCartesian",
                                          "hoverCompareCartesian"))
    }
  })
  
  output$mir_plots <- renderUI({
    ## If neither have WNV nor SLEV have data
    if (all(is.na(wnv_data()$mir_avg)) & all(is.na(slev_data()$mir_avg))) {
      strong("Neither WNV nor SLEV infection data are available for this location and time.")
    ## If only WNV is NA
    } else if (all(is.na(wnv_data()$mir_avg))) {
      strong("No WNV infection data are available for this location and time.")
      renderPlotly({
        slev_plot()
      })
    ## If only SLEV is NA
    } else if (all(is.na(slev_data()$mir_avg))) {
      strong("No SLEV infection data are available for this location and time.")
      renderPlotly({
        wnv_plot()
      })
    ## If data for both
    } else {
      renderPlotly({
        wnv <- wnv_plot()
        slev <- slev_plot()
        subplot(wnv, slev, nrows = 2, margin = 0.06, titleY = TRUE)
      })
    }
  })

  
  
  ## Reactive captions for map
  output$abundPlot_caption <- renderText({
    ## If invalid zip, don't show text
    if(!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste("Average mosquito abundance within Kern County. The green line displays average weekly abundance within zip code ", zipcodeTrap_d(), " between ", input$trap_dateRange[1], " and ", input$trap_dateRange[2], ". The dashed black line represents the average mosquito abundance within this time period across all Kern zip codes, while the dashed purple line shows average abundance in zip code ", zipcodeTrap_d(), " from 2010 to present.")
    }
  })
  
  output$wnvPlot_caption <- renderText({
    if(!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste("<b>","Fig. 3: Average minimum infection rates (MIR) for West Nile (top) and St. Louis Encephalitis (bottom) viruses within Kern County.","</b>", "The orange and yellow bars displays average weekly MIR for WNV and SLEV, respectivley, within zip code ", zipcodeTrap_d(), " between ", input$trap_dateRange[1], " and ", input$trap_dateRange[2], ". The dashed black line represents the average MIR within this time period across all Kern zip codes, while the dashed purple line shows average MIR in zip code ", zipcodeTrap_d(), " from 2010 to present.")
    }
  })
  
  

  # TAB 2 - Interactive Map ##################################################
  
  ### PANEL ELEMENTS -----------------------------------------------------------
  
  #### Zip code ---------------------
  ## React to user input and slightly delay response
  zipcode <- reactive(input$zip_box)
  zipcode_d <- debounce(zipcode, millis = 1500)
  
  ## Validation text below zip box
  iv <- InputValidator$new()
      ### Must be 5 numbers
      iv$add_rule("zip_box", function(length) {
        length = nchar(zipcode_d())
        if (length !=5 & length != 0) {
          "Only 5-character entries are permitted."
        }
      })
      ### Must be zip in Kern
      iv$add_rule("zip_box", function(zipcode_d) {
        if (!(zipcode_d() %in% zips_sf$zipcode) & nchar(zipcode_d()) != 0) {
          "Please enter a valid zip code."
        } 
      })
  iv$enable()
  
  
  #### R0 ---------------------------
  # ## Return mean transmission risk based on zip input
  # r0_zip <- reactive({
  #   r0_filtered <- r0_zip_df %>% 
  #     filter(zipcode == zipcode_d())
  #   ## return only transmission value
  #   r0_mean <- round(r0_filtered$trans_eff,3)
  #   return(r0_mean)
  # })
  # 
  # ## Total avg transmission risk for Kern
  # r0_kern <- r0_zip_df %>% 
  #   filter(zipcode == "Kern")
  # 
  # 
  # ## R0 header
  # output$r0_header <- renderText({
  #   ## If invalid zip input, don't show text
  #   if (!(zipcode_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #   } else {
  #     paste0("<h3>", "Transmission (R",'<sub>', '0','</sub>', "):", "</h3>")
  #   }
  # })
  # 
  # ## Reactive R0 text
  # output$r0_value <- renderText({
  #   ## If invalid zip input, don't show text
  #   if(!(zipcode_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #     ## If there is no trans data for zipcode
  #   } else if (is.na(r0_zip())) {
  #     paste0("The average R",'<sub>', '0','</sub>', " across Kern County is: ","<b>",round(r0_kern[1,1],3),"</b>", "<br>", "No transmission data are available for the selected zip code.")
  #   } else {
  #     ## Text for valid zips
  #     paste("The average R",'<sub>', '0','</sub>', " across Kern County is: ", "<b>",round(r0_kern[1,1],3),"</b>", "<br>", 'The average R','<sub>', '0','</sub>', ' within zip code ', zipcode_d(), "is: ", "<b>",r0_zip(),"</b>")
  #   }
  # })
  # 
  # ## Line
  # output$r0_line <- renderUI({
  #   ## If invalid zip input, don't show text
  #   if (!(zipcode_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #   } else {
  #     hr(style = 'border-top: 1.5px solid #2d3e50')
  #   }
  # })
  
  
  #### Date range --------------------
  # ## Date header
  # output$dates_header <- renderText({
  #   ## If invalid zip, don't show text
  #   if(!(zipcode_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #   } else {
  #     paste("<h3>", "Date range:", "</h3>")
  #   }
  # })
  # 
  # ## Risk date range
  # output$risk_dateRange <- renderUI({
  #   if(zipcode_d()=="") {
  #     return(NULL)
  #   } else {
  #     dateRangeInput("risk_dateRange",
  #                    label = NULL,
  #                    start = "2023-01-01",
  #                    end = "2023-07-31")
  #   }
  # })
  
  ## Validation text for risk date range
  ivTemp <- InputValidator$new()
  ### Proper end date
  ivTemp$add_rule("risk_dateRange", function(start, end) {
    start = input$risk_dateRange[1]
    end = input$risk_dateRange[2]
    if (end < start) {
      "End date is earlier than start date."
    }
  })
  ### Dates w/in data range
  ivTemp$add_rule("risk_dateRange", 
                  ~ if(input$risk_dateRange[1] < "2010-04-01") 
                    "Start date must be after 2010-04-01.")
  ivTemp$add_rule("risk_dateRange", 
                  ~ if(input$risk_dateRange[2] > "2023-11-30") 
                    "End date must be before 2023-11-30.")
  
  ivTemp$enable()
  
  ## Line
  output$date_line <- renderUI({
    ## If invalid zip input, don't show text
    if (!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      hr(style = 'border-top: 1.5px solid #2d3e50')
    }
  })
  
  #### Temperature -------------------
  ## Temp header
  output$temp_header <- renderText({
    ## If invalid zip, don't show text
    if(!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste("<h4>", "Temperature:", "</h4>")
    }
  })
  
  # ## Temp date range
  # output$temp_dateRange <- renderUI({
  #   if(!(zipcode_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #   } else {
  #     dateRangeInput("temp_dateRange",
  #                    label = NULL,
  #                    start = "2023-01-01",
  #                    end = "2023-07-31")
  #   }
  # })
  
  ## Filter temp data based on zip input
  temp_zip <- reactive({
    temp_filtered <- temp_zip_df %>% 
      filter(zipcode == zipcode_d(),
             date >= input$risk_dateRange[1] & date <= input$risk_dateRange[2]) 
    
    return(temp_filtered)
  })   
  
  # ## Validation text for temp date range
  # ivTemp <- InputValidator$new()
  #     ### Proper end date
  #     ivTemp$add_rule("temp_dateRange", function(start, end) {
  #       start = input$risk_dateRange[1]
  #       end = input$risk_dateRange[2]
  #       if (end < start) {
  #         "End date is earlier than start date."
  #       }
  #     })
  #     ### Dates w/in data range
  #     ivTemp$add_rule("temp_dateRange", 
  #                     ~ if(input$temp_dateRange[1] < "2018-01-01") 
  #                       "Start date must be after 2018-01-01.")
  #     ivTemp$add_rule("temp_dateRange", 
  #                     ~ if(input$temp_dateRange[2] > "2023-07-31") 
  #                       "End date must be before 2023-07-31.")
  # 
  # ivTemp$enable()
  
  ## Number of days at optimal temp
  tempOptDays_int <- reactive({
    days = sum(temp_zip()$cx_opt == "optimal")
    
    return(days)
  })
  ## Percent of days at optimal temp
  tempOptDays_per <- reactive({
    percent = round((tempOptDays_int()/(nrow(temp_zip())))*100, 2)
    
    return(percent)
  })
  
  ## Number of days in temp range
  tempRangeDays_int <- reactive({
    days = sum(temp_zip()$cx_opt == "in range")
    
    return(days)
  })
  ## Percent of days in temp range
  tempRangeDays_per <- reactive({
    percent = round(((tempRangeDays_int()+tempOptDays_int())/(nrow(temp_zip())))*100, 2)
    
    return(percent)
  })
  
  output$tempDays_text <- renderText({
    ## If invalid zip, don't show text
    if(!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste0("In this time period, ", "<b>",tempOptDays_int(), " days ","</b>",  "(", tempOptDays_per(), "%)",  " fell within the optimal temperature range (red) and ", tempRangeDays_int(), " days (", tempRangeDays_per(),"%) fell within the thermal limits (orange) for WNV transmission by ", "<i>","Culex","</i>", " mosquitoes.")
    }
  })
  
  ## Daily temp plot
  output$temp_plot <- renderPlot({
    if (!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else if (difftime(input$risk_dateRange[2],
                        input$risk_dateRange[1]) <= 31) {
      ggplot(data = temp_zip(), aes(x = date, y = tmean_f)) +
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 89.42, ymin = 78.6,
                  alpha = 0.05, fill = "gray89")+
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 78.6, ymin = 73.2,
                  alpha = 0.05, fill = "gray81")+
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 73.2, ymin = 53.78,
                  alpha = 0.05, fill = "gray89")+
        geom_point(size = 3, 
                   alpha = 0.7,
                   aes(color = cx_opt)) +
        scale_color_manual(name = "",
                           values = c("firebrick2","goldenrod3", "dodgerblue"))+
        geom_line(linewidth = 0.7) +
        labs(y = "Mean daily temp (F)",
             x = element_blank()) +
        scale_x_date(date_labels = "%d\n%b") +
        geom_hline(yintercept = 89.42, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 78.6, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 73.2, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 53.78, linetype = "dashed", color = "gray50")+
        theme_classic() +
        theme(
          axis.title.y = element_text(vjust = 2, size = 14),
          axis.title.x = element_text(vjust = -1, size = 14),
          axis.text = element_text(size = 13),
          legend.position = "none")
    } else {
      ggplot(data = temp_zip(), aes(x = date, y = tmean_f)) +
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 89.42, ymin = 78.6,
                  alpha = 0.05, fill = "gray89")+
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 78.6, ymin = 73.2,
                  alpha = 0.05, fill = "gray81")+
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 73.2, ymin = 53.78,
                  alpha = 0.05, fill = "gray89")+
        geom_point(size = 3, 
                   alpha = 0.7,
                   aes(color = cx_opt)) +
        scale_color_manual(name = "",
                           values = c("firebrick2","goldenrod3", "dodgerblue"))+
        geom_line(linewidth = 0.7) +
        labs(y = "Mean daily temp (F)",
             x = element_blank()) +
        scale_x_date(date_labels = "%b %y") +
        geom_hline(yintercept = 89.42, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 78.6, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 73.2, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 53.78, linetype = "dashed", color = "gray50")+
        theme_classic() +
        theme(
          axis.title.y = element_text(vjust = 2, size = 14),
          axis.title.x = element_text(vjust = -1, size = 14),
          axis.text = element_text(size = 13),
          legend.position = "none")
    }
  })
  
  ## Line
  output$temp_line <- renderUI({
    ## If invalid zip input, don't show text
    if (!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      hr(style = 'border-top: 1.5px solid #2d3e50')
    }
  })
  
  
  #### Water ----------------------------
  ## Standing water header
  output$water_header <- renderText({
    if(!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste("<h4>", "Standing water:", "</h4>")
    }
  })
  
  
  # ## Water date range
  # output$water_dateRange <- renderUI({
  #   if(!(zipcode_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #   } else {
  #     dateRangeInput("water_dateRange",
  #                    label = NULL,
  #                    start = "2023-01-01",
  #                    end = "2023-07-31")
  #   }
  # })
  
  ## Filter water data based on zip input
  water_zip <- reactive({
    water_filtered <- water_zip_df %>% 
      filter(zipcode == zipcode_d(),
             date >= input$risk_dateRange[1] & date <= input$risk_dateRange[2]) 
    
    return(water_filtered)
  })   
  
  ## Standing water time series
  output$water_plot <- renderPlot({
   if (!(zipcode_d() %in% zips_sf$zipcode)) {
     return(NULL)
   } else if (length(water_zip()$acres_int)==0) {
     strong("No water data available for this time.")
   } else {
    ggplot(data = water_zip(), aes(x = date, y = acres_int)) +
      geom_point(color = "dodgerblue3", size = 4, alpha = 0.6) +
      geom_line(linewidth = 0.6, color = "dodgerblue4") +
      labs(y = "Surface water (acres)",
           x = element_blank()) +
      ## customize axis with cont 'date' class data
      # scale_x_date(limits = as.Date(c('2023-05-07', '2023-06-25')),
      #              date_breaks = "2 week",
      #              date_labels = "%b %d") +
      theme_classic() +
      theme(
        # axis.title.x = element_text(face = "bold", vjust = -1),
        axis.title.y = element_text(vjust = 2, size = 14),
        axis.text = element_text(size = 13)
      )
   }
  })
  


  
  
  
  ### LEAFLET MAP --------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      #### Add rasters -------------------------------
      ## Transmission efficiency
      # addRasterImage(wnv_trans, colors = pal, 
      #                project = FALSE, group = "R0") %>%
      # ## Standing water
      # addRasterImage(water, colors = 'dodgerblue4',
      #                project = FALSE, group = "Standing Water") %>%
      
      #### Add vectors ---------------------------------
      ## Zip codes
      addPolygons(data = zips_sf, color = "#343434",
                  weight = 2, fillOpacity = 0.1,
                  label = paste0("Zip code: ", zips_sf$zipcode),
                  labelOptions = labelOptions(textsize = "11px"),
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes",
                  layerId = ~zipcode) %>%  
    
      ## Kern county
      addPolylines(data = kern_sf,
                   color = 'black', weight = 4, fillOpacity = 0,
                   group = "Kern county") %>% 
    
      ## Central valley
      addPolylines(data = valley_sf,
                   color = 'blue', weight = 2.5, fillOpacity = 0,
                   group = "Central Valley") %>%
      
      #### Create map groups -----------------------------
      addLayersControl(
        baseGroups = c("OpenStreetMaps", "World Imagery"),
        overlayGroups = c("R0", "Standing Water", "Zip codes", "Kern county", "Central Valley"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      
      ## Don't show all layers by default
      hideGroup(c("R0", "Standing Water", "Central Valley")) %>% 
      
      #### Add map inset ---------------------------------
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        zoomLevelOffset = -5,
        position = 'bottomleft',
        toggleDisplay = TRUE) %>% 
      
      #### Add legend ------------------------------------
      # addLegend(pal = pal, values = values(wnv_trans),
      #           position = "topleft",
      #           title = "WNV Transmission </br> Efficiency") %>% 
      setView(-119.2, 35.38, zoom = 10)
  }) ## END LEAFLET
  
  
  #### Interactive Leaflet elements: --------------------
  
  ## Click on zip code polygon to input value in text box
  observe({
    event <- input$map_shape_click
    if(is.null(event$id))
      return()
    
    ## change text box value
    updateTextInput(session, 
                    inputId = "zip_box", 
                    value = event$id)
  })

  ## Zoom and highlight zip code
  observe({

    ## establish zip code boundaries
    geom <- zips_sf %>%
      dplyr::filter(zipcode == input$zip_box)
    bounds <- geom %>%
      st_bbox() %>%
      as.character()

    ## Update map
    leafletProxy("map") %>%
      clearGroup("highlighted_polygon") %>%
      ## zoom to zip
      flyToBounds(lng1 = bounds[1], lat1 = bounds[2],
                  lng2 = bounds[3], lat2 = bounds[4]) %>%
      ## highlight selected zip
      addPolylines(stroke=TRUE, weight = 5, color="yellow",
                   fill = TRUE, fillColor = "white", fillOpacity = 0.4,
                   data = geom, group = "highlighted_polygon")
  })

  
  
  # TAB 3 - Standing Water ##################################################

  ### Zip code box ----------------
  ## React to user input and slightly delay response
  zipcodeWater <- reactive(input$zip_box_water)
  zipcodeWater_d <- debounce(zipcodeWater, millis = 1500)
  
  ## Validation text below zip box
  ivTrap <- InputValidator$new()
  ### Must be 5 numbers
  ivTrap$add_rule("zip_box_water", function(length) {
    length = nchar(zipcodeWater_d())
    if (length !=5 & length != 0) {
      "Only 5-character entries are permitted."
    }
  })
  ### Must be zip in Kern
  ivTrap$add_rule("zip_box_water", function(zipcodeWater_d) {
    if (!(zipcodeWater_d() %in% zips_sf$zipcode) & nchar(zipcodeWater_d()) != 0) {
      "Please enter a valid zip code."
    } 
  })
  ivTrap$enable()
  
  ## WATER MAP ------------------------------------------------------------
  output$waterMap <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      ### Zip codes
      addPolygons(data = zips_sf, color = "#343434",
                  weight = 2, fillOpacity = 0.1,
                  label = paste0("Zip code: ", zips_sf$zipcode),
                  labelOptions = labelOptions(textsize = "11px"),
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes",
                  layerId = ~zipcode) %>%  
      
      ### Kern county
      addPolylines(data = kern_sf,
                   color = 'black', weight = 4, fillOpacity = 0,
                   group = "Kern county") %>% 
      
      ### Central valley
      addPolylines(data = valley_sf,
                   color = 'blue', weight = 2.5, fillOpacity = 0,
                   group = "Central Valley") %>%
      
      ## Create map groups
      addLayersControl(
        baseGroups = c("OpenStreetMaps", "World Imagery"),
        overlayGroups = c("Zip codes", "Kern county", "Central Valley"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      
      ## Don't show all layers by default
      hideGroup(c("Kern county")) %>%

      setView(-119.2, 35.38, zoom = 9)
  }) ##END LEAFLET
  
  ### Interactive Leaflet elements: 
  ## Click on zip code polygon to input value in text box
  observe({
    event <- input$waterMap_shape_click
    if(is.null(event$id))
      return()
    
    ## change text box value
    updateTextInput(session, 
                    inputId = "zip_box_water", 
                    value = event$id)
  })
  
  ## Zoom and highlight zip code
  observe({
    ## establish zip code boundaries
    geom <- zips_sf %>%
      dplyr::filter(zipcode == input$zip_box_water)
    bounds <- geom %>%
      st_bbox() %>%
      as.character()
    
    ## Update map
    leafletProxy("waterMap") %>%
      clearGroup("highlighted_polygon") %>%
      ## zoom to zip
      flyToBounds(lng1 = bounds[1], lat1 = bounds[2],
                  lng2 = bounds[3], lat2 = bounds[4]) %>%
      ## highlight selected zip
      addPolylines(stroke=TRUE, weight = 5, color="yellow",
                   fill = TRUE, fillColor = "white", fillOpacity = 0.4,
                   data = geom, group = "highlighted_polygon")
  })
  

  output$water_test <- renderUI({
    if(!(zipcodeWater_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      card(
        full_screen = TRUE,
        layout_columns(
          col_widths = c(6,6),
          tags$video(src = paste0("vids/zip_", zipcodeWater_d(),
                                  "_2022_2023.mp4"),
                     # height = "440px", 
                     autoplay = TRUE, controls = TRUE),
          plotlyOutput("waterTab_plot")
        )
      )
    }
  })
  ## Water video ------------------
  # output$waterVid <- renderUI({
  #     ## Select gif based on zipcode and year
  #     vid <- paste0("vids/zip_", zipcodeWater_d(),
  #                   "_2022_2023.mp4")
  #     tags$video(src = vid,
  #         height = "440px", autoplay = TRUE, controls = TRUE)
  #   }
  # })
  # 
  
  ## Standing water time series ----------------
  
  ## Filter water data based on zip input
  waterTab_data <- reactive({
    water_filtered <- water_zip_df %>% 
      filter(zipcode == zipcodeWater_d())
    return(water_filtered)
  })  
  
  ## Plot
  output$waterTab_plot <- renderPlotly({
        ggplotly(
          ggplot(data = waterTab_data(), aes(x = date, y = acres_int)) +
          geom_point(color = "dodgerblue3", size = 2, alpha = 0.6,
                     aes(text = paste0("Week:  ", date, "<br>",
                                      "Acres:  ", round(acres_int,2)))
                     ) +
          geom_line(linewidth = 0.6, color = "dodgerblue4") +
          labs(y = "Surface water (acres)",
               x = element_blank()) +
          scale_x_date(date_breaks = "2 month",
                       date_labels = "%b\n%Y")+
          theme_minimal() +
          theme(
            axis.title.y = element_text(vjust = 3.5, size = 12),
            axis.text = element_text(size = 10)
          ),
          tooltip = c("text")
        ) %>% 
          config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("select2d", "lasso2d", "resetScale2d",
                                            "hoverClosestCartesian",
                                            "hoverCompareCartesian"))
      })

  #old one
  # output$waterTab_plot <- renderUI({
  #   if (!(zipcodeWater_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #   } else if (length(waterTab_data()$acres_int)==0) {
  #     strong("No water data available for this time.")
  #   } else {
  #     renderGirafe({
  #     waterPlot <- ggplot(data = waterTab_data(), aes(x = date, y = acres_int)) +
  #         geom_point_interactive(color = "dodgerblue3", size = 4, alpha = 0.6,
  #                    ## interactive elements
  #                    aes(tooltip = paste0(waterTab_data()$date_plot,
  #                                         "\n", round(waterTab_data()$acres_int,2),
  #                                         " acres"), 
  #                        size = 1.5,
  #                        tooltip_fill = "dodgerblue4",
  #                        hover_nearest=TRUE)
  #                    ) +
  #         geom_line_interactive(linewidth = 0.6, color = "dodgerblue4") +
  #         labs(y = "Surface water (acres)",
  #              x = element_blank()) +
  #         ## customize axis with cont 'date' class data
  #         scale_x_date(date_breaks = "2 month",
  #                      date_labels = "%b\n%Y")+
  #         theme_minimal() +
  #         theme(
  #           # axis.title.x = element_text(face = "bold", vjust = -1),
  #           axis.title.y = element_text(vjust = 2, size = 12),
  #           axis.text = element_text(size = 10)
  #         )
  #     x <- girafe(ggobj = waterPlot)
  #            # height_svg = 7, width_svg = 9)
  #     x <- girafe_options(x, opts_zoom(min = 1, max = 2.5),
  #                         opts_tooltip(use_fill=TRUE, opacity = 0.8))
  #     })
  #   }
  # })

  
  
  # TAB 4 - Info ##################################################

  
} ### END SERVER
