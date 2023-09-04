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
cases_kern_df <- read_csv(here('data/traps/wnv_kern.csv'))
temp_zip_df <- read_csv(here('data/temp/kern_tmean_20180101_20230731.csv')) %>% 
  mutate(cxTar_opt = factor(cxTar_opt),
         cxTar_opt = fct_relevel(cxTar_opt, levels = c("TRUE", "FALSE")))

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
    text = paste0("This app let's you explore the risks and hazards associated with West Nile Virus (WNV) in Kern County, California. Information by zip code* can be viewed by either manually typing the zip code at the upper right, or clicking the zip code on the map.", "<br>", "<br>", "For more information on WNV transmission and the data used for this app, select the 'More Info' button or click on the information icon at the top of the page.", "<br>", "<br>", "<span style='font-size: 13px;'>", "<i>", "*(Note: Zip code boundaries have been limited to the extent present within both Kern County and the California Central Valley. These boundaries can be visually toggled by (un)selecting layers listed at the top left of the map.", "</i>", ")",  "</span>"),
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

  ## TAB 1 - Interactive Map ##################################################
  
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
        if (!(zipcode_d() %in% zips_sf$GEOID10) & nchar(zipcode_d()) != 0) {
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
    if (!(zipcode_d() %in% zips_sf$GEOID10)) {
      return(NULL)
    } else {
      paste0("<h3>", "Transmission (R",'<sub>', '0','</sub>', "):", "</h3>")
    }
  })
  
  ## Reactive R0 text
  output$r0_value <- renderText({
    ## If invalid zip input, don't show text
    if(!(zipcode_d() %in% zips_sf$GEOID10)) {
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
    if (!(zipcode_d() %in% zips_sf$GEOID10)) {
      return(NULL)
    } else {
      hr(style = 'border-top: 1.5px solid #2d3e50')
    }
  })
  
  
  #### Temperature -------------------
  ## Temp header
  output$temp_header <- renderText({
    ## If invalid zip, don't show text
    if(!(zipcode_d() %in% zips_sf$GEOID10)) {
      return(NULL)
    } else {
      paste("<h3>", "Temperature:", "</h3>")
    }
  })
  
  ## Temp date range
  output$temp_dateRange <- renderUI({
    if(!(zipcode_d() %in% zips_sf$GEOID10)) {
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
      ### Range of 2+ weeks
      ivTemp$add_rule("temp_dateRange", 
                      ~ if(difftime(input$temp_dateRange[2],
                                    input$temp_dateRange[1])<14)
                        "Date range less than 14 days")
  ivTemp$enable()
  
  ## Number of days (and percentage) in range
  tempDays_int <- reactive({
    days = sum(temp_zip()$cxTar_opt == "TRUE")
    
    return(days)
  })
  
  ## Percent of days in range
  tempDays_per <- reactive({
    percent = round((tempDays_int()/(nrow(temp_zip())))*100, 2)
    
    return(percent)
  })
  
  output$tempDays_text <- renderText({
    ## If invalid zip, don't show text
    if(!(zipcode_d() %in% zips_sf$GEOID10)) {
      return(NULL)
    } else {
      paste0("In this time period, ", "<b>",tempDays_int(), " days ","</b>",  "(", tempDays_per(), "%)",  " fell within the optimal temperature range for WNV transmission by ", "<i>","Culex tarsalis","</i>", " mosquitos.")
    }
  })
  
  ## Daily temp plot
  output$temp_plot <- renderPlot({
    if (!(zipcode_d() %in% zips_sf$GEOID10)) {
      return(NULL)
    } else {
      ggplot(data = temp_zip(), aes(x = date, y = tmean_f)) +
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 78.6, ymin = 73.2,
                  alpha = 0.05, fill = "gray85")+
        geom_point(size = 3, 
                   alpha = 0.7,
                   aes(color = cxTar_opt)) +
        scale_color_manual(name = "",
                           values = c("firebrick2", "dodgerblue"))+
        geom_line(linewidth = 0.7) +
        labs(y = "Mean daily temp (F)",
             x = "Date") +
        scale_x_date(date_labels = "%b %y") +
        geom_hline(yintercept = 73.2, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 78.6, linetype = "dashed", color = "gray50")+
        # annotate("text", y = 74, x = as.Date(temp_zip()$date[1]),
        #          hjust = 0,
        #          label = "Optimal range for\nWNV transmission",
        #          size = 3,
        #          fontface = "bold") +
        theme_classic() +
        theme(
          # axis.title.x = element_text(face = "bold", vjust = -1),
          axis.title.y = element_text(vjust = 2, size = 14),
          axis.title.x = element_text(vjust = -1, size = 14),
          axis.text = element_text(size = 13),
          legend.position = "none"
        )
    }
  })
  
  ## Line
  output$temp_line <- renderUI({
    ## If invalid zip input, don't show text
    if (!(zipcode_d() %in% zips_sf$GEOID10)) {
      return(NULL)
    } else {
      hr(style = 'border-top: 1.5px solid #2d3e50')
    }
  })
  
  
  #### Water ----------------------------
  ## Standing water header
  output$water_header <- renderText({
    if(!(zipcode_d() %in% zips_sf$GEOID10)) {
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
   if (!(zipcode_d() %in% zips_sf$GEOID10))
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
                  weight = 2, fillOpacity = 0.2,
                  label = paste0("Zip code: ", zips_sf$GEOID10),
                  labelOptions = labelOptions(textsize = "11px"),
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes",
                  layerId = ~GEOID10) %>%  
    
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
      dplyr::filter(GEOID10 == input$zip_box)
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
      addPolylines(stroke=TRUE, weight = 5,color="yellow",
                   data = geom, group = "highlighted_polygon")
  })

  ## TAB 2 - Trap Data ##########################################################
  
  ## RESPONSIVE SIDE PANEL WIDGETS --------------------
  
  ## Zip code box
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
      if (!(zipcodeTrap_d() %in% zips_sf$GEOID10) & nchar(zipcodeTrap_d()) != 0) {
        "Please enter a valid zip code."
      } 
    })
    ivTrap$enable()
  

  ## Trap month box
  output$trapMonth <- renderUI({
    if (input$trapTime != "monthly") {
      return(NULL)
    } else {
      selectInput("trapMonth", label = "Select month:",
                  choices = list("March" = "Mar",
                                 "April" = "Apr",
                                 "May" = "May",
                                 "June" = "Jun",
                                 "July" = "Jul",
                                 "August" = "Aug",
                                 "September" = "Sep",
                                 "October" = "Oct",
                                 "November" = "Nov"),
                  selected = "Aug")
    }
  })
  
  ## Custom date range
  output$trapDates <- renderUI({
    if (input$trapTime != "custom") {
      return(NULL)
    } else {
      dateRangeInput("trapDates", 
                     label = "Date range:",
                     start = "2023-01-01",
                     end = "2023-07-31")
    }
  })
  
  ### TRAP MAP ------------------------------------------------------------
  output$trapMap <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  
      ### Zip codes
      addPolygons(data = zips_sf, color = "#343434",
                  weight = 2, fillOpacity = 0.2,
                  label = paste0("Zip code: ", zips_sf$GEOID10),
                  labelOptions = labelOptions(textsize = "11px"),
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes",
                  layerId = ~GEOID10) %>%  
        
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
          
          ## Add legend ------------------------------------
        # addLegend(pal = pal, values = values(wnv_trans),
        #           position = "topleft",
        #           title = "WNV Transmission </br> Efficiency") %>% 
        setView(-119.2, 35.38, zoom = 9)
  }) ## END LEAFLET
  
  
  ### Interactive Leaflet elements: 
  ## Click on zip code polygon to input value in text box
  ## and zoom to zip code
  observe({
    event <- input$trapMap_shape_click
    if(is.null(event$id))
      return()
    
    ## change text box value
    updateTextInput(session, 
                    inputId = "zip_box_trap", 
                    value = event$id)
  })
  
  
  ## Reactive caption for map
  output$trapMap_caption <- renderText({
    paste("<b>","Figure 1:","</b>","Interactive map of mosquito and WNV abundance by zip codes within the Central Valley and Kern County, California. In this selected scenario, darker zip codes represent areas with greater ", "<b>","(select: abundance/WNV)","</b>", ". Across all zip codes,", "<b>","(select: abundance/WNV)","</b>","(select:between/for date range)", "equaled", "<b>","X","</b>")
  })
  
  
  
  ### FILTER TRAP DATA ----------------------------------------------------
  ## Filter trap data by user input
  trap_data <- reactive ({
    
    if(input$trapTime == "annual") {
      filtered_year <- cases_kern_df %>% 
        filter(GEOID10 == zipcodeTrap_d()) %>% 
        group_by(Year) %>% 
        summarize(cases = sum(Count))
    } else if (input$trapTime == "monthly") {
      filtered_month <- cases_kern_df %>% 
        filter(GEOID10 == zipcodeTrap_d(),
               Month == input$trapMonth) %>% 
        group_by(Year) %>% 
        summarize(cases = sum(Count))
    }
    # } else {
    #   filtered_custom <- cases_kern_df %>% 
    #     filter(GEOID10 == zipcodeTrap_d(),
    #            date >= input$temp_dateRange[1] & date <= input$temp_dateRange[2])
    # } 
  })
  
 
   trap_data_county <- reactive ({
    if(input$trapTime == "annual") {
      filtered_year <- cases_kern_df %>%
        group_by(Year) %>% 
        summarize(cases = sum(Count))
    } else if (input$trapTime == "monthly") {
      filtered_month <- cases_kern_df %>% 
        filter(Month == input$trapMonth) %>% 
        group_by(Year) %>% 
        summarize(cases = sum(Count))
    }
  })
  
  
 
  
  
  ## Trap plot
  output$trap_plot <- renderPlot({
    if (!(zipcodeTrap_d() %in% zips_sf$GEOID10)) {
      return(NULL)
    } else if (input$trapTime == "annual") {
      ggplot() +
        ## By zip code
        geom_point(data = trap_data(), 
                   aes(x = Year, y = cases),
                   color = "sienna2", size = 3, alpha = 0.6) +
        geom_line(data = trap_data(), 
                  aes(x = Year, y = cases),
                  linewidth = 0.6, color = "sienna4") +
        ## All county
        geom_point(data = trap_data_county(),
                   aes(x = Year, y = cases),
                   color = "forestgreen", size = 3, alpha = 0.6) +
        geom_line(data = trap_data_county(),
                  aes(x = Year, y = cases),
                  linewidth = 0.6, color = "darkgreen") +
        labs(y = "Annual trapped cases",
             x = "Year") +
        theme_classic() +
        theme(
          # axis.title.x = element_text(face = "bold", vjust = -1),
          axis.title.y = element_text(vjust = 2, size = 14),
          axis.title.x = element_text(vjust = -1, size = 14),
          axis.text = element_text(size = 13)
        ) 
    } else {
      ggplot(data = trap_data(), aes(x = Year, y = cases)) +
        geom_point(color = "sienna2", size = 3, alpha = 0.6) +
        geom_line(linewidth = 0.6, color = "sienna4") +
        labs(y = "Monthly trapped cases",
             x = "Year") +
        theme_classic() +
        theme(
          # axis.title.x = element_text(face = "bold", vjust = -1),
          axis.title.y = element_text(vjust = 2, size = 14),
          axis.title.x = element_text(vjust = -1, size = 14),
          axis.text = element_text(size = 13)
        ) 
    }
  })
  
  
  
  
  
} ### END SERVER
