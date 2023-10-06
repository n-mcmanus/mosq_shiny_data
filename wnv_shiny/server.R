### READ IN DATA AND PACKAGES

library(shiny)
library(shinyalert)    ## Modals
library(shinyvalidate) ## Text box validation messages
library(tidyverse)     ## Always
library(here)          ## Easier reading/writing data
library(lubridate)     ## Wrangling/plotting dates
library(leaflet)       ## Interactive map
library(raster)        ## Leaflet-friendly raster pkg
library(sf)            ## Leaflet-friendly vector pkg

## Rasters
wnv_trans <- raster(here('data/Kern_transmission_raster_wgs84.tif'))
water <- raster(here('data/water/water_reproj.tif'))

## Vectors
zips_sf <- st_read(here('data/zipcodes/kern_zips.shp'))
kern_sf <- st_read(here('data/counties_ca/kern.shp'))
valley_sf <- st_read(here('data/central_valley/valley.shp'))

## Data frames
water_zip_df <- read_csv(here('data/water/water_acre_zipcode.csv'))
r0_zip_df <- read_csv(here('data/transmission_efficiency_zipcodes.csv'))
wnv_df <- read_csv(here('data/traps/wnvMIR_plotting.csv'))
abund_df <- read_csv(here('data/traps/abundance_plotting.csv')) %>% 
  janitor::clean_names()
temp_zip_df <- read_csv(here('data/temp/kern_tmean_20180101_20230731.csv')) %>% 
  mutate(cx_opt = factor(cx_opt),
         cx_opt = fct_relevel(cx_opt, levels = c("optimal", "in range", "out range")))

## Raster color palette
pal <- colorNumeric(palette = 'viridis', domain = values(wnv_trans),
                    reverse = TRUE,
                    na.color = "transparent")




# START SERVER
function(input, output, session) {
  
  ## WELCOME MODAL -----------------------------------------------------------
  
  ## Initial modal on app launch
  shinyalert(
    title = "Welcome",
    text = paste0("This app let's you explore the risks and hazards associated with West Nile Virus (WNV) transmission in Kern County, California. Information by zip code* can be explored by either manually typing the zip code at the upper right, or by clicking the location on the map.", "<br>", "<br>", "For more information on WNV transmission and the data used for this app, select the 'More Info' button or click on the information icon at the top of the page.", "<br>", "<br>", "<span style='font-size: 13px;'>", "<i>", "*(Note: Zip code boundaries have been limited to the extent present within both Kern County and the California Central Valley. These boundaries can be visually toggled by (un)selecting layers listed at the top left of the map.", "</i>", ")",  "</span>"),
    size = "m", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "",
    showConfirmButton = TRUE,
    showCancelButton = TRUE,
    confirmButtonText = "More Info",
    confirmButtonCol = "#AEDEF4",
    cancelButtonText = "View Map",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )

  ## Go to "info" tab if button pressed
  observeEvent(input$shinyalert, {
    if (input$shinyalert == TRUE) {
      updateNavbarPage(session, "nav", selected = "tab4")
    }
  })
  
  
  # TAB 1 - Trap Data ##########################################################
  
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
                           end = paste(input$trapYear, "07-31", sep = "-"))
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
  
  
  # ## Trap month box
  # output$trapMonth <- renderUI({
  #   if (input$trapTime != "monthly") {
  #     return(NULL)
  #   } else {
  #     selectInput("trapMonth", label = "Select month:",
  #                 choices = list("March" = 3,
  #                                "April" = 4,
  #                                "May" = 5,
  #                                "June" = 6,
  #                                "July" = 7,
  #                                "August" = 8,
  #                                "September" = 9,
  #                                "October" = 10,
  #                                "November" = 11),
  #                 selected = 8)
  #   }
  # })
  
  # ### Custom date range
  # output$trap_dateRange <- renderUI({
  #   if (input$trapTime != "custom") {
  #     return(NULL)
  #   } else {
  #     dateRangeInput("trap_dateRange", 
  #                    label = "Date range:",
  #                    start = "2023-01-01",
  #                    end = "2023-07-31")
  #   }
  # })
  
  ## TRAP MAP ------------------------------------------------------------
  output$trapMap <- renderLeaflet({
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
                   color = 'blue', weight = 3, fillOpacity = 0,
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
      
      ## Add legend 
      # addLegend(pal = pal, values = values(wnv_trans),
      #           position = "topleft",
      #           title = "WNV Transmission </br> Efficiency") %>% 
      setView(-119.2, 35.38, zoom = 9)
  }) ## END LEAFLET
  
  
  ### Interactive Leaflet elements: 
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
  
  
  ## Reactive caption for map
  output$trapMap_caption <- renderText({
    paste("<b>","Figure 1:","</b>","Interactive map of zip codes within the Central Valley and Kern County, California. The selected zip code is outline in yellow, and the border for the central valley is shown in blue. Borders for Kern County and the central valley can be toggled using the map options in the upper left corner.")
  })
  
  
  
  ### Filter Trap Data ----------------------------------------------------
  ## Abundance 
  ### Filtered abundance by time and zip
  abund_data <- reactive ({
    df <- abund_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(cases = mean(mos_per_trap_night, na.rm = TRUE))
    
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
  
  
  ## WNV MIR 
  ### Filtered abundance by time and zip
  wnv_data <- reactive ({
    df <- wnv_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date >= input$trap_dateRange[1] & date <= input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(cases = mean(mir_all, na.rm = TRUE))
    return(df)
  })
  
  ### Avg MIR for zip (all time)
  avgWnv_zip <- reactive ({
    x <- wnv_df %>% 
      filter(zipcode == zipcodeTrap_d())
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  ### Avg abund for time (all Kern)
  avgWnv_kern <- reactive ({
    x <- wnv_df %>% 
      filter(date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2])
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  
  ### Plots ----------------------------------------------------
  ## Abundance plot
  output$abund_plot <- renderUI({
    if (!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
      ## If no data, no plot
    } else if (length(abund_data()$cases)==0) {
      strong("No abundance data available for this location and time.")
      ## If plotting under two months
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 62) {
      renderPlot({
        ggplot(data = abund_data(), aes(x = date, y = cases)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          
          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 3, alpha = 0.8) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%d %b %y",
                       breaks = unique(abund_data()$date)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
      ## If plotting between 2 months to a year
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 365) {
      renderPlot({
        ggplot(data = abund_data(), aes(x = date, y = cases)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 3, alpha = 0.8) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%b %y",
                       date_breaks = "1 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
      ## If plotting between a year and two years
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 730) {
      renderPlot({
        ggplot(data = abund_data(), aes(x = date, y = cases)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 2, alpha = 0.8) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "2 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
      ## If plot time is greater than two years
    }  else {
      renderPlot({
        ggplot(data = abund_data(), aes(x = date, y = cases)) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgAbund_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgAbund_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          ## Filtered zip/time data
          geom_line(color = "seagreen4", linewidth = 0.8) +
          geom_point(color = "seagreen3", size = 1, alpha = 0.8) +
          labs(y = "Average weekly abundance",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "3 month") +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
    }
  })
  
  
  ## WNV plot
  output$wnv_plot <- renderUI({
    if (!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else if (all(is.na(wnv_data()$cases))) {
      strong("No infection data available for this location and time.")
      ## If plot time is under two months
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 62) {
      renderPlot({
        ggplot(data = wnv_data(), aes(x = date, y = cases)) +
          ## Filtered to zip/time
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          labs(y = "Weekly MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%d %b %y")+
          # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
      ## Plot time between two months to a year
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 365) {
      renderPlot({
        ggplot(data = wnv_data(), aes(x = date, y = cases)) +
          ## Filtered to zip/time
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          labs(y = "Average weekly MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b %y",
                       date_breaks = "1 month")+
          # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
      ## Plot time between a year to two years
    } else if (difftime(input$trap_dateRange[2],
                        input$trap_dateRange[1]) <= 730) {
      renderPlot({
        ggplot(data = wnv_data(), aes(x = date, y = cases)) +
          ## Filtered to zip/time
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          labs(y = "Average weekly MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "2 month") +
          # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
      ## Plot time over two years
    } else {
      renderPlot({
        ggplot(data = wnv_data(), aes(x = date, y = cases)) +
          ## Filtered to zip/time
          geom_col(fill = "sienna2", color = "sienna4", alpha = 0.7) +
          ## Kern avg (in time period):
          geom_hline(yintercept = avgWnv_kern(),
                     color = "black", linetype = "dashed", linewidth = 0.8) +
          ## Zip avg (all time):
          geom_hline(yintercept = avgWnv_zip(),
                     color = "purple", linetype = "dashed", linewidth = 0.8) +
          labs(y = "Average weekly MIR",
               x = element_blank()) +
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "3 month") +
          # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          theme_classic() +
          theme(
            axis.title.y = element_text(vjust = 2, size = 14),
            axis.title.x = element_text(vjust = -1, size = 14),
            axis.text = element_text(size = 13)) 
      })
    }
  })
  
  
  ## Reactive captions for map
  output$abundPlot_caption <- renderText({
    ## If invalid zip, don't show text
    if(!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste("<b>","Fig. 2A:","</b>","Average mosquito abundance within Kern County. The green line plot displays average weekly abundance within zip code ", zipcodeTrap_d(), " between ", input$trap_dateRange[1], " and ", input$trap_dateRange[2], ". The dashed black line represents the average mosquito abundance within this time period across all Kern zip codes, while the dashed purple line shows average abundance in zip code ", zipcodeTrap_d(), " from 2018 to present.")
    }
  })
  
  output$wnvPlot_caption <- renderText({
    if(!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste("<b>","Fig. 2B:","</b>","WNV average minimum infection rate (MIR) within Kern County. The orange bar graph displays the average weekly MIR within zip code ", zipcodeTrap_d(), " between ", input$trap_dateRange[1], " and ", input$trap_dateRange[2], ". The dashed black line represents the average MIR within this time period across all Kern zip codes, while the dashed purple line shows average MIR in zip code ", zipcodeTrap_d(), " from 2018 to present.")
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
  ## Return mean transmission risk based on zip input
  r0_zip <- reactive({
    r0_filtered <- r0_zip_df %>% 
      filter(zipcode == zipcode_d())
    ## return only transmission value
    r0_mean <- round(r0_filtered$trans_eff,3)
    return(r0_mean)
  })
  
  ## Total avg transmission risk for Kern
  r0_kern <- r0_zip_df %>% 
    filter(zipcode == "Kern")
  
  
  ## R0 header
  output$r0_header <- renderText({
    ## If invalid zip input, don't show text
    if (!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste0("<h3>", "Transmission (R",'<sub>', '0','</sub>', "):", "</h3>")
    }
  })
  
  ## Reactive R0 text
  output$r0_value <- renderText({
    ## If invalid zip input, don't show text
    if(!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
      ## If there is no trans data for zipcode
    } else if (is.na(r0_zip())) {
      paste0("The average R",'<sub>', '0','</sub>', " across Kern County is: ","<b>",round(r0_kern[1,1],3),"</b>", "<br>", "No transmission data are available for the selected zip code.")
    } else {
      ## Text for valid zips
      paste("The average R",'<sub>', '0','</sub>', " across Kern County is: ", "<b>",round(r0_kern[1,1],3),"</b>", "<br>", 'The average R','<sub>', '0','</sub>', ' within zip code ', zipcode_d(), "is: ", "<b>",r0_zip(),"</b>")
    }
  })
  
  ## Line
  output$r0_line <- renderUI({
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
      paste("<h3>", "Temperature:", "</h3>")
    }
  })
  
  ## Temp date range
  output$temp_dateRange <- renderUI({
    if(!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      dateRangeInput("temp_dateRange",
                     label = NULL,
                     start = "2023-01-01",
                     end = "2023-07-31")
    }
  })
  
  ## Filter temp data based on zip input
  temp_zip <- reactive({
    temp_filtered <- temp_zip_df %>% 
      filter(zipcode == zipcode_d(),
             date >= input$temp_dateRange[1] & date <= input$temp_dateRange[2]) 
    
    return(temp_filtered)
  })   
  
  ## Validation text for temp date range
  ivTemp <- InputValidator$new()
      ### Proper end date
      ivTemp$add_rule("temp_dateRange", function(start, end) {
        start = input$temp_dateRange[1]
        end = input$temp_dateRange[2]
        if (end < start) {
          "End date is earlier than start date."
        }
      })
      ### Dates w/in data range
      ivTemp$add_rule("temp_dateRange", 
                      ~ if(input$temp_dateRange[1] < "2018-01-01") 
                        "Start date must be after 2018-01-01.")
      ivTemp$add_rule("temp_dateRange", 
                      ~ if(input$temp_dateRange[2] > "2023-07-31") 
                        "End date must be before 2023-07-31.")

  ivTemp$enable()
  
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
      paste0("In this time period, ", "<b>",tempOptDays_int(), " days ","</b>",  "(", tempOptDays_per(), "%)",  " fell within the optimal temperature range (red) and ", tempRangeDays_int(), " days (", tempRangeDays_per(),"%) fell within the thermal limits (orange) for WNV transmission by ", "<i>","Culex","</i>", " mosquitos.")
    }
  })
  
  ## Daily temp plot
  output$temp_plot <- renderPlot({
    if (!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else if (difftime(input$temp_dateRange[2],
                        input$temp_dateRange[1]) <= 31) {
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
      paste("<h3>", "Standing water:", "</h3>")
    }
  })
  
  ## Filter water data based on zip input
  water_zip <- reactive({
    water_filtered <- water_zip_df %>% 
      filter(zipcode == zipcode_d()) 
    
    return(water_filtered)
  })   
  
  ## Standing water time series
  output$water_plot <- renderPlot({
   if (!(zipcode_d() %in% zips_sf$zipcode))
     return(NULL)
     
    ggplot(data = water_zip(), aes(x = date, y = water_acres)) +
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
   
  })
  


  
  
  
  ### LEAFLET MAP --------------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      #### Add rasters -------------------------------
      ## Transmission efficiency
      addRasterImage(wnv_trans, colors = pal, 
                     project = FALSE, group = "R0") %>%
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
                   color = 'blue', weight = 3, fillOpacity = 0,
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

  
  
} ### END SERVER
