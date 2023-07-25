#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(raster)
library(sf)
library(here)

wnv_risk <- raster(here('data/Kern_transmission_raster_wgs84.tif'))
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(trans_r),
                    na.color = "transparent")
water_test_data <- read_csv(here('data/water/water_test_data.csv')) %>% 
  mutate(date = mdy(date))
zips <- read_sf(here("data/zipcodes/kern_zips.shp"))
zips_trans<- st_transform(zips, crs = "+proj=longlat +datum=WGS84")

# Define server logic required to draw a histogram
function(input, output, session) {

  
  ## Zipcode input
  
  zipcode <- reactive(input$zip_box)
  zipcode_d <- debounce(zipcode, millis = 2000)
  
  observeEvent(zipcode_d(), {
    if (nchar(zipcode_d()) != 5)
    {
      updateTextInput(session, 'zip_box')
      showModal(modalDialog(
          title = "Error!",
          "Only 5-character entries are permitted.",
          easyClose = TRUE
        )
      )
    }
  })
  
  ## Filter water data based on zipcode
  water_zip <- reactive({
    
    water_data_filtered <- water_test_data %>% 
      filter(zipcode == zipcode_d()) 
    
    return(water_data_filtered)
  })           
  
  ## Water plot
  output$water_plot <- renderPlot({
   if (is.na(zipcode_d()))
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
  
  
  
  ## Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps
      addTiles(group = "OSM (default)") %>% 
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      ## Add rasters
      addRasterImage(wnv_risk, colors = pal, project = FALSE, group = "WNV Risk") %>%
      ## Add polygons
      addPolygons(data = zips_trans, color = "#343434", 
                  weight = 2, fillOpacity = 0.1,
                  label = paste0("Zip code: ", zips_trans$GEOID10),
                  group = "Zip codes") %>% 
      ## Add groups to map
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner Lite"),
        overlayGroups = c("WNV Risk", "Zip codes"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  
  
  
    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # 
    # })

}
