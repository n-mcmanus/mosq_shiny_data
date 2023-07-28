### READ IN DATA AND PACKAGES

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(raster)
library(sf)
library(here)

## Rasters
wnv_risk <- raster(here('data/Kern_transmission_raster_wgs84.tif'))
water <- raster(here('data/water/water_reproj.tif'))

## Vectors
zips_sf <- read_sf(here("data/zipcodes/kern_zips.shp"))

## Data frames
water_test_df <- read_csv(here('data/water/water_test_data.csv')) %>% 
  mutate(date = mdy(date))
trans_zip_df <- read_csv(here('data/transmission_zipcodes.csv'))
cases_kern_df <- read_csv(here('data/wnv_cases/wnv_kern.csv'))

## Raster color palette
pal <- colorNumeric(palette = 'viridis', domain = values(wnv_risk),
                    reverse = TRUE,
                    na.color = "transparent")




### START SERVER
function(input, output, session) {

  
  ### Zip code input -----------------------------------------
  
  ## React to user input and slightly delay response
  zipcode <- reactive(input$zip_box)
  zipcode_d <- debounce(zipcode, millis = 1500)
  
  ## Return error if user submits a number
  ## with length other than 5 or 0 (e.g. no entry)
  observeEvent(zipcode_d(), {
    if (nchar(zipcode_d()) != 5 & nchar(zipcode_d()) != 0)
    {
      updateTextInput(session, 'zip_box', value = NA)
      showModal(modalDialog(
          title = "Error!",
          "Only 5-character entries are permitted.",
          easyClose = TRUE)
      )
    }
  })
  
  
  ## Filter water data based on zip input
  water_zip <- reactive({
    water_filtered <- water_test_df %>% 
      filter(zipcode == zipcode_d()) 
    
    return(water_filtered)
  })    
  
  
  ## Return mean transmission risk based on zip input
  trans_zip <- reactive({
    trans_filtered <- trans_zip_df %>% 
      filter(zipcode == zipcode_d())
    
    ## return only transmission value
    trans_mean <- round(trans_filtered$trans_risk,3)
    return(trans_mean)
  })
  
  
  ## Filter trap data by user input
  trap_data <- reactive ({
    
    month_selected <- input$trap_month
    
    x <- if(input$trap_time == "annual") {
      filtered_year <- cases_kern_df %>% 
        filter(GEOID10 == zipcode_d()) %>% 
        group_by(Year) %>% 
        summarize(cases = sum(Count))
    } else {
      filtered_month <- cases_kern_df %>% 
        filter(GEOID10 == zipcode_d(),
               Month == month_selected) %>% 
        group_by(Year) %>% 
        summarize(cases = sum(Count))
    }
    return(x)
  })
  
  
  
  ### Panel elements -----------------------------------------
  
  ## Transmission Risk header
  output$trans_header <- renderText({
    ## If invalid zip input, don't show text
    if (nchar(zipcode_d()) != 5) {
      return(NULL)
    } else {
      paste("<h3>", "Transmission Risk:", "</h3>")
    }
  })
  
  ## Reactive transmission text
  output$zip_risk <- renderText({
    ## If invalid zip input, don't show text
    if(nchar(zipcode_d()) != 5) {
      return(NULL)
      ## If there is no trans data for zipcode
    } else if (is.na(trans_zip())) {
      print("Transmission data unavailable for this zip code.")
    } else {
      ## Text for valid zips
      paste('The average transmission risk for zip code', "<i>",zipcode_d(),"</i>","is:", "<b>",trans_zip(),"</b>")
    }
  })
  
  
  
  ## Standing water header
  output$water_header <- renderText({
    if(nchar(zipcode_d()) != 5) {
      return(NULL)
    } else {
      paste("<h3>", "Standing water:", "</h3>")
    }
  })
  
  ## Standing water time series
  output$water_plot <- renderPlot({
   if (nchar(zipcode_d()) != 5)
     return(NULL)
     
    ggplot(data = water_zip(), aes(x = date, y = water_ha)) +
      geom_point(color = "dodgerblue3", size = 4, alpha = 0.6) +
      geom_line(linewidth = 0.6, color = "dodgerblue4") +
      labs(y = "Surface water (ha)",
           x = element_blank()) +
      ## customize axis with cont 'date' class data
      scale_x_date(limits = as.Date(c('2023-05-07', '2023-06-25')),
                   date_breaks = "2 week",
                   date_labels = "%b %d") +
      theme_classic() +
      theme(
        # axis.title.x = element_text(face = "bold", vjust = -1),
        axis.title.y = element_text(vjust = 3, size = 14),
        axis.text = element_text(size = 13)
      )
   
  })
  
  
  ## Trap header
  output$trap_header <- renderText({
    ## If no zip input, don't show text
    if(nchar(zipcode_d()) != 5) {
      return(NULL)
    } else {
      paste("<h3>", "Trap Data:", "</h3>")
    }
  })
  
  ## Trap year/month box
  output$trap_time <- renderUI({
    if(nchar(zipcode_d()) != 5) {
      return(NULL)
    } else {
      selectInput("trap_time", label = "Select timeframe:",
                  choices = list("Annual" = "annual",
                                 "Monthly" = "monthly"),
                  selected = "annual")
    } 
  })
  
  ## Trap month box
  output$trap_month <- renderUI({
    if(nchar(zipcode_d())!=5) {
      return(NULL)
      ## only appear if correct zip input AND 
      ## first box is monthly
    } else if (input$trap_time == "annual") {
      return(NULL)
    } else {
      selectInput("trap_month", label = "Select month:",
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
  
  
  ## Standing water time series plots
  output$trap_plot <- renderPlot({
    if (nchar(zipcode_d()) != 5) {
      return(NULL)
    } else if (input$trap_time == "annual") {
      ggplot(data = trap_data(), aes(x = Year, y = cases)) +
        geom_point(color = "sienna2", size = 3, alpha = 0.6) +
        geom_line(linewidth = 0.6, color = "sienna4") +
        labs(y = "Annual trapped cases",
             x = "Year") +
        theme_classic() +
        theme(
          # axis.title.x = element_text(face = "bold", vjust = -1),
          axis.title.y = element_text(vjust = 3, size = 14),
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
          axis.title.y = element_text(vjust = 3, size = 14),
          axis.title.x = element_text(vjust = -1, size = 14),
          axis.text = element_text(size = 13)
        ) 
    }
  })
  


  
  
  
  ### Leaflet map ---------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      ## Add rasters
      addRasterImage(wnv_risk, colors = pal, 
                     project = FALSE, group = "WNV Risk") %>%
      # addRasterImage(water, colors = 'dodgerblue4',
      #                project = FALSE, group = "Standing Water") %>%
      
      ## Add polygons
      addPolygons(data = zips_sf, color = "#343434", 
                  weight = 3, fillOpacity = 0.1,
                  label = paste0("Zip code: ", zips_sf$GEOID10),
                  labelOptions = labelOptions(textsize = "11px"),
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes",
                  layerId = ~GEOID10) %>% 
      
      ## Create map groups
      addLayersControl(
        baseGroups = c("OpenStreetMaps", "World Imagery"),
        overlayGroups = c("WNV Risk", "Zip codes", "Standing Water"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      ## Only raster shows by default
      hideGroup(c("Zip codes", "Standing Water")) %>% 
      
      ## Add map inset
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        zoomLevelOffset = -5,
        position = 'bottomleft',
        toggleDisplay = TRUE) %>% 
      
      ## Add legend to map
      addLegend(pal = pal, values = values(wnv_risk),
                position = "topleft",
                title = "WNV Transmission </br> Efficiency") %>% 
      setView(-119.3, 35.55, zoom = 10)
  })
  
  
  ## Click on zip code polygon
  ## to input value in text box
  observe({
    event <- input$map_shape_click
    
    updateTextInput(session, 
                    inputId = "zip_box", 
                    value = event$id)
  })
  


} ### END SERVER
