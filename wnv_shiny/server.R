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
zips <- read_sf(here("data/zipcodes/kern_zips.shp"))
zips_trans<- st_transform(zips, crs = "+proj=longlat +datum=WGS84")

## Data frames
water_test_data <- read_csv(here('data/water/water_test_data.csv')) %>% 
  mutate(date = mdy(date))

## Raster color palette
pal <- colorNumeric(palette = 'viridis', domain = values(wnv_risk),
                    reverse = TRUE,
                    na.color = "transparent")




### START SERVER
function(input, output, session) {

  
  ### Zip code input -----------------------------------------
  
  ## React to user input and slightly delay response
  zipcode <- reactive(input$zip_box)
  zipcode_d <- debounce(zipcode, millis = 2000)
  
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
    data_filtered <- water_test_data %>% 
      filter(zipcode == zipcode_d()) 
    
    return(data_filtered)
  })           

  
  ### Panel plots --------------------------------------------
  
  ## Standing water time series
  output$water_plot <- renderPlot({
   if (is.na(zipcode()))
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
        axis.title.y = element_text(face = 'bold', vjust = 3, size = 15),
        axis.text = element_text(size = 13)
      )
   
  })
  
  
  
  ### Leaflet map ---------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      ## Add rasters
      addRasterImage(wnv_risk, colors = pal, 
                     project = FALSE, group = "WNV Risk") %>%
      # addRasterImage(water_reproj, colors = 'dodgerblue4', 
      #                project = FALSE, group = "Standing Water") %>% 
      ## Add polygons
      addPolygons(data = zips_trans, color = "#343434", 
                  weight = 2, fillOpacity = 0.1,
                  label = paste0("Zip code: ", zips_trans$GEOID10),
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes") %>% 
      ## Add groups to map
      addLayersControl(
        baseGroups = c("OpenStreetMaps", "Toner Lite"),
        overlayGroups = c("WNV Risk", "Zip codes", "Standing Water"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      ## Only raster shows by default
      hideGroup(c("Zip codes", "Standing Water")) %>% 
      ## Add legend to map
      addLegend(pal = pal, values = values(wnv_risk),
                position = "bottomleft",
                title = "WNV Transmission Risk") %>% 
      setView(-119.3, 35.55, zoom = 10)
  })
  
  
  


} ### END SERVER
