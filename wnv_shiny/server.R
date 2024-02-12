### READ IN DATA AND PACKAGES

library(shiny)
library(bslib)
library(bsicons)
library(shinyalert)    ## Modals
library(shinyvalidate) ## Text box validation messages
library(tidyverse)     ## Always
library(here)          ## Easier reading/writing data
library(lubridate)     ## Wrangling/plotting dates
library(leaflet)       ## Interactive map
library(raster)        ## Leaflet-friendly raster pkg
library(sf)            ## Leaflet-friendly vector pkg

## Rasters
water_r <- raster(here('data/water/summed_water_90m_2022_2023.tif'))
# r0_r <- raster(here('data/r0.tif'))
## Raster color palettes
water_pal <- colorBin(palette = 'viridis', 
                      domain = values(water_r),
                      na.color = "transparent")

## Vectors
zips_sf <- st_read(here('data/zipcodes/kern_zips.shp'))
kern_sf <- st_read(here('data/counties_ca/kern.shp'))
valley_sf <- st_read(here('data/central_valley/valley.shp'))

## Data frames
water_zip_df <- read_csv(here("data/water/water_acre_zipcode.csv"))
# r0_zip_df <- read_csv(here("data/r0.csv"))
wnv_df <- read_csv(here("data/traps/plotting/wnvMIR_plotting.csv"))
slev_df <- read_csv(here("data/traps/plotting/slevMIR_plotting.csv"))
abund_df <- read_csv(here('data/traps/plotting/abundance_plotting.csv')) %>% 
  janitor::clean_names()
temp_zip_df <- read_csv(here('data/temp/kern_tmean_20100401_20230930.csv')) %>% 
  mutate(cx_opt = factor(cx_opt),
         cx_opt = fct_relevel(cx_opt, "optimal", "in range", "out range"))


# START SERVER
function(input, output, session) {
  
  # WELCOME MODAL -----------------------------------------------------------
  
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
  
  # TAB 1: TRAP DATA ##########################################################
  
  ## Example for how to hyperlink to different tab
  ## Can use later if needed
  # observeEvent(input$link_to_info, {
  #   newValue <- "tab4"
  #   updateTabItems(session, "nav", newValue)
  # })
  
  
  ## MAP SIDE PANEL WIDGETS ----------------------
  
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
  
  
  ### Trap date selection -----------
  observe({
    ## Last year (2023) incomplete so limit through Sep
    if (input$trapMonth == "none" & input$trapYear == "2023") {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, "01-01", sep = "-"),
                           end = paste(input$trapYear, "09-30", sep = "-"))
      ## First year (2010) only has data from March onward
    } else if (input$trapMonth == "none" & input$trapYear == "2010") {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, "03-01", sep = "-"),
                           end = paste(input$trapYear, "12-31", sep = "-"))
      ## For all other years, list entire year
    } else if (input$trapMonth == "none") {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, "01-01", sep = "-"),
                           end = paste(input$trapYear, "12-31", sep = "-"))
      ## If month is selected, then stick to just that year/month
    } else {
      updateDateRangeInput(session, "trap_dateRange",
                           start = paste(input$trapYear, 
                                         input$trapMonth, 
                                         "01", 
                                         sep = "-"),
                           end = paste(input$trapYear, 
                                       input$trapMonth, 
                                       "31", 
                                       sep = "-"))
    }
  })  
  
  
  ## TRAP MAP ------------------------------------------------------------
  output$trapMap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      ## place zoom ctrl buttons on upper right
      ## otherwize blocks sidebar button
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>% 
      
      ## Add background maps, default OSM
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      ### Kern zip codes
      addPolygons(data = zips_sf, color = "#343434",
                  weight = 2, fillOpacity = 0.05,
                  ## Hover label
                  label = paste0("Zip code: ", zips_sf$zipcode),
                  labelOptions = labelOptions(textsize = "11px"),
                  ## Hover highlight
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
      
      ## Map groups/layers box
      addLayersControl(
        baseGroups = c("OpenStreetMaps", "World Imagery"),
        overlayGroups = c("Zip codes", "Kern county", "Central Valley"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topright") %>% 
      ## Don't show all layers by default
      hideGroup(c("Kern county")) %>% 
      
      ## Map inset
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        zoomLevelOffset = -5,
        position = 'bottomright',
        toggleDisplay = TRUE) %>% 
      
      ## Set map to AOI
      setView(-119.2, 35.38, zoom = 9)
  }) ## END LEAFLET MAP
  
  
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
  
  ## Highlight and zoom to zip code
  observe({
    ## First establish zip code boundaries
    geom <- zips_sf %>%
      dplyr::filter(zipcode == input$zip_box_trap)
    bounds <- geom %>%
      st_bbox() %>%
      as.character()
    
    ## Update map
    leafletProxy("trapMap") %>%
      ## remove previously selected zip
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
  
  
  
  
  ## FILTER DATA ----------------------------------------------------
  ### Abundance -------------------
  ## Filtered abundance by time and zip
  abund_data <- reactive ({
    df <- abund_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(abund_woy_avg = mean(mos_per_trap_night, na.rm = TRUE))
    
    return(df)
  })
  
  ## Avg abund for zip (all time)
  avgAbund_zip <- reactive ({
    x <- abund_df %>% 
      filter(zipcode == zipcodeTrap_d())
    
    xMean <- mean(x$mos_per_trap_night, na.rm = TRUE)
    
    return(xMean)
  })
  
  ## Avg abund for time period (all Kern)
  avgAbund_kern <- reactive ({
    x <- abund_df %>% 
      filter(date >= input$trap_dateRange[1] & date <= input$trap_dateRange[2])
    
    xMean <- mean(x$mos_per_trap_night, na.rm = TRUE)
    
    return(xMean)
  })
  
  
  ### WNV MIR  --------------------
  ## Filtered MIR by time and zip
  wnv_data <- reactive ({
    df <- wnv_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date >= input$trap_dateRange[1] & date <= input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(mir_avg = mean(mir_all, na.rm = TRUE))
    return(df)
  })
  
  ## Avg MIR for zip (all time)
  avgWnv_zip <- reactive ({
    x <- wnv_df %>% 
      filter(zipcode == zipcodeTrap_d())
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  ## Avg MIR for time (all Kern)
  avgWnv_kern <- reactive ({
    x <- wnv_df %>% 
      filter(date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2])
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  
  ### SLEV MIR  --------------------
  ## Filtered slev by time and zip
  slev_data <- reactive ({
    df <- slev_df %>% 
      filter(zipcode == zipcodeTrap_d(),
             date >= input$trap_dateRange[1] & date <= input$trap_dateRange[2]) %>% 
      group_by(year, date) %>% 
      summarize(mir_avg = mean(mir_all, na.rm = TRUE))
    return(df)
  })
  
  ## Avg SLEV for zip (all time)
  avgSlev_zip <- reactive ({
    x <- slev_df %>% 
      filter(zipcode == zipcodeTrap_d())
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  ## Avg abund for time (all Kern)
  avgSlev_kern <- reactive ({
    x <- slev_df %>% 
      filter(date>=input$trap_dateRange[1] & date<=input$trap_dateRange[2])
    
    xMean <- mean(x$mir_all, na.rm = TRUE)
    
    return(xMean)
  })
  
  
  ## PLOTS ----------------------------------------------------
  output$trapPlots <- renderUI({
    ## Don't generate until zip selected
    if (!(zipcodeTrap_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      ## Makes 2 equally sized cards
      layout_column_wrap(
        width = 1/2,
        height = "640px",
        ## Abundance plot
        card(
          full_screen = TRUE,
          ## Header with popover text
          card_header(
            "Mosquito abundance:",
            popover(
              trigger = bs_icon("question-circle"),
              "Due to the large number of traps in Kern county and variability in collection frequency, mosquito abundance is standardized as “mosquitoes per trap night”, or:",
              withMathJax("$$\\scriptsize{\\frac{\\text{Total number individuals}}{\\text{Number nights since last collection}}}$$"),
              "This value is then averaged across all traps in a given area (zip code or county) by week. As such, reported average abundance values do not represent the total number of mosquitoes present in a week, but rather a standardized metric that can be used to track relative changes in abundance over time."
            ),
            class = "d-flex justify-content-between"
          ),
          ## Body w/reactive plot
          card_body(
            ## If no data, don't make plot
            if (length(abund_data()$abund_woy_avg) == 0) {
              strong("No abundance data available for this location and time.")
            } else {
              plotlyOutput("abund_plot")
            }),
          ## Footer w/reactive text
          card_footer(htmlOutput("abundPlot_caption"))
        ),
        ## MIR plots
        card(
          full_screen = TRUE,
          ## Header w/popover text
          card_header(
            "Infection rates:",
            popover(
              trigger = bs_icon("question-circle"),
              placement = "right",
              "Minimum infection rate (MIR) is a common method used to estimate infection rates in mosquito populations. Mosquitoes are trapped and tested in groups, or “pools”. MIR assumes that if a tested pool comes back positive for a MBD, only one mosquito in the pool is infected. Reported MIR values can be interpreted as:  “At minimum, X number of mosquitoes were infected during week Y.” ",
              withMathJax("$$\\scriptsize{MIR = \\frac{\\text{Number Positive Pools}} {\\text{Total Number Individuals}}*1000}$$"),
            ),
            class = "d-flex justify-content-between"
          ),
          ## body w/plot
          card_body(
            ## Don't plot if no data
            if (length(wnv_data()$mir_avg) == 0) {
              strong("No infection data available for this location and time.")
            } else {
              plotlyOutput("mir_plots")
            }),
          ## footer w/reactive text
          card_footer(htmlOutput("mirPlot_caption"))
        )
      )##End column wrap
    }
  })##End plot
  
  
  
  ### Abundance plot ------------------
  output$abund_plot <- renderPlotly({
    p <- ggplot(data = abund_data(),
                aes(x = date, y = abund_woy_avg)) +
      ## Kern avg (in time period):
      geom_hline(
        yintercept = avgAbund_kern(),
        color = "black",
        linetype = "dashed",
        linewidth = 0.5
      ) +
      ## Zip avg (all time):
      geom_hline(
        yintercept = avgAbund_zip(),
        color = "purple",
        linetype = "dashed",
        linewidth = 0.5
      ) +
      ## Filtered abund data
      geom_line(color = "seagreen4", linewidth = 0.8) +
      ## change point size based on time scale
      (if (difftime(input$trap_dateRange[2],
                    input$trap_dateRange[1]) <= 62) {
        geom_point(
          color = "seagreen3",
          size = 3,
          alpha = 0.8,
          aes(text = paste0(
            "Week:  ",
            date,
            "<br>",
            "Average abundance:  ",
            round(abund_woy_avg, 2)
          ))
        )
      } else if (difftime(input$trap_dateRange[2],
                          input$trap_dateRange[1]) <= 365) {
        geom_point(
          color = "seagreen3",
          size = 3,
          alpha = 0.8,
          aes(text = paste0(
            "Week:  ",
            date,
            "<br>",
            "Average abundance:  ",
            round(abund_woy_avg, 2)
          ))
        )
      } else if (difftime(input$trap_dateRange[2],
                          input$trap_dateRange[1]) <= 730) {
        geom_point(
          color = "seagreen3",
          size = 2,
          alpha = 0.8,
          aes(text = paste0(
            "Week:  ",
            date,
            "<br>",
            "Average abundance:  ",
            round(abund_woy_avg, 2)
          ))
        )
      } else if (difftime(input$trap_dateRange[2],
                          input$trap_dateRange[1]) <= 1461) {
        geom_point(
          color = "seagreen3",
          size = 1.5,
          alpha = 0.8,
          aes(text = paste0(
            "Week:  ",
            date,
            "<br>",
            "Average abundance:  ",
            round(abund_woy_avg, 2)
          ))
        )
      } else {
        geom_point(
          color = "seagreen3",
          size = 1,
          alpha = 0.8,
          aes(text = paste0(
            "Week:  ",
            date,
            "<br>",
            "Average abundance:  ",
            round(abund_woy_avg, 2)
          ))
        )
      }) +
      ## show full date and tick for each week if short time period
      (if (difftime(input$trap_dateRange[2],
                    input$trap_dateRange[1]) <= 62) {
        scale_x_date(date_labels = "%d %b %y",
                     breaks = unique(abund_data()$date))
      } else if (difftime(input$trap_dateRange[2],
                          input$trap_dateRange[1]) <= 365) {
        scale_x_date(date_labels = "%b %y",
                     date_breaks = "1 month")
      } else if (difftime(input$trap_dateRange[2],
                          input$trap_dateRange[1]) <= 730) {
        scale_x_date(date_labels = "%b\n%y",
                     date_breaks = "2 month")
      } else if (difftime(input$trap_dateRange[2],
                          input$trap_dateRange[1]) <= 1461) {
        scale_x_date(date_labels = "%b\n%y",
                     date_breaks = "3 month")
      } else {
        scale_x_date(date_labels = "%b\n%y",
                     date_breaks = "6 month")
      }) +
      ## Plot labels/aesthetics
      labs(y = "Average weekly abundance",
           x = element_blank()) +
      theme_classic() +
      theme(axis.title.y = element_text(vjust = 2, size = 12),
            axis.text = element_text(size = 10))
    
    ## Wrap the whole thing in plotly
    ggplotly(p, tooltip = c("text")) %>%
      config(
        displaylogo = FALSE,
        ## Customize menu buttons
        modeBarButtonsToRemove = c(
          "select2d",
          "lasso2d",
          "resetScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  }) ##END abund plot
  
  
  ### MIR Plots ---------------------------
  # ## Combine WNV and SLEV into one reactive plot.
  # ## ggplot format wrapped in plotly
  output$mir_plots <- renderPlotly({
    ## WNV plot
    wnv <- ggplotly(
      ggplot(data = wnv_data(),
             aes(
               x = date,
               y = mir_avg,
               ## Text to appear in tooltip
               text = paste0("Week: ", date,
                             "<br>",
                             "Average MIR: ",
                             round(mir_avg, 3))
             )) +
        ## Filtered to zip/time
        geom_col(
          fill = "sienna2",
          color = "sienna4",
          alpha = 0.7
        ) +
        ## Kern avg (in time period):
        geom_hline(
          yintercept = avgWnv_kern(),
          color = "black",
          linetype = "dashed",
          linewidth = 0.5
        ) +
        ## Zip avg (all time):
        geom_hline(
          yintercept = avgWnv_zip(),
          color = "purple",
          linetype = "dashed",
          linewidth = 0.5
        ) +
        # Change X axis labels and breaks depending on
        # length of time period
        (if (difftime(input$trap_dateRange[2],
                      input$trap_dateRange[1]) <= 62) {
          scale_x_date(date_labels = "%d %b %y",
                       breaks = unique(wnv_data()$date))
        } else if (difftime(input$trap_dateRange[2],
                            input$trap_dateRange[1]) <= 365) {
          scale_x_date(date_labels = "%b %y",
                       date_breaks = "1 month")
        } else if (difftime(input$trap_dateRange[2],
                            input$trap_dateRange[1]) <= 730) {
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "2 month")
        } else if (difftime(input$trap_dateRange[2],
                            input$trap_dateRange[1]) <= 1461) {
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "3 month")
        } else {
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "6 month")
        }) +
        ## Labels and text options
        labs(y = "Weekly WNV MIR",
             x = element_blank()) +
        theme_classic() +
        theme(
          axis.title.y = element_text(vjust = 2, size = 12),
          axis.text = element_text(size = 10)
        ),
      ## Define hover text
      tooltip = c("text")
    ) %>% 
      ## Customize plotly menu
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "select2d",
          "lasso2d",
          "resetScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
    
    ## SLEV plot
    slev <- ggplotly(
      ggplot(data = slev_data(),
             aes(
               x = date,
               y = mir_avg,
               ## Text to appear in tooltip
               text = paste0("Week:  ", date,
                             "<br>",
                             "Average MIR:  ",
                             round(mir_avg, 3))
             )) +
        ## Filtered to zip/time
        geom_col(fill = "#fff9ae",
                 color = "#a98600",
                 alpha = 0.7) +
        ## Kern avg (in time period):
        geom_hline(
          yintercept = avgSlev_kern(),
          color = "black",
          linetype = "dashed",
          linewidth = 0.5
        ) +
        ## Zip avg (all time):
        geom_hline(
          yintercept = avgSlev_zip(),
          color = "purple",
          linetype = "dashed",
          linewidth = 0.5
        ) +
        ## Change X axis labels and breaks depending on
        ## length of time period
        (if (difftime(input$trap_dateRange[2],
                      input$trap_dateRange[1]) <= 62) {
          scale_x_date(date_labels = "%d %b %y",
                       breaks = unique(slev_data()$date))
        } else if (difftime(input$trap_dateRange[2],
                            input$trap_dateRange[1]) <= 365) {
          scale_x_date(date_labels = "%b %y",
                       date_breaks = "1 month")
        } else if (difftime(input$trap_dateRange[2],
                            input$trap_dateRange[1]) <= 730) {
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "2 month")
        } else if (difftime(input$trap_dateRange[2],
                            input$trap_dateRange[1]) <= 1461) {
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "3 month")
        } else {
          scale_x_date(date_labels = "%b\n%y",
                       date_breaks = "6 month")
        }) +
        ## Labels/text options
        labs(y = "Weekly SLEV MIR",
             x = element_blank()) +
        theme_classic() +
        theme(axis.title.y = element_text(vjust = 2, size = 12),
              axis.text = element_text(size = 10)),
      ## Define hover text
      tooltip = c("text")
    ) %>% 
      ## Customize plotly menu
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "select2d",
          "lasso2d",
          "resetScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
    
    ## Combine MIR plots
    subplot(wnv, slev, nrows = 2, margin = 0.05, titleY = TRUE)
    
  }) ##END MIR plots
  
  
  
  ## CAPTIONS ----------------------------------------------------
  
  ## Map caption
  output$trapMap_caption <- renderText({
    paste("Interactive map of zip codes within the Central Valley and Kern County, California. First, select a zip code on the map or manually enter it into the side menu. Then, you may visualize data by month or year, or select a custom date range. The selected zip code is outlined in yellow. Borders for Kern County and the Central Valley can be toggled using the map options in the upper right corner.")
  })
  
  ## Abundance plot caption
  output$abundPlot_caption <- renderText({
    paste0("The green line displays average weekly abundance within zip code ", zipcodeTrap_d(), " between the selected dates. The dashed black line represents the average mosquito abundance within this time period across all Kern zip codes (", round(avgAbund_kern(),2), ") while the dashed purple line shows average abundance in zip code ", zipcodeTrap_d(), " from 2010 to present (", round(avgAbund_zip(),2), ").")
  })
  
  ## MIR plot caption
  output$mirPlot_caption <- renderText({
    paste0("The orange and yellow bars display average weekly MIR for WNV and SLEV, respectivley, within zip code ", zipcodeTrap_d(), " between the selected dates. The dashed black lines represent the average MIR across all Kern within this time period (WNV: ", round(avgWnv_kern(),2), "; SLEV: ", round(avgSlev_kern(),2), "). The dashed purple lines show the average MIR in zip code ", zipcodeTrap_d(), " from 2010 to present (WNV: ", round(avgWnv_zip(),2), "; SLEV: (", round(avgSlev_zip(),2), ").")
  })
  
  
  
  # TAB 2: Interactive Map ##################################################
  
  ###PANEL ELEMENTS -----------------------------------
  
  #### Zip code -----------------
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
  
  #### Date range -----------------
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
  
  
  #### R0 ---------------------------
  # ## Return mean R0 based on zip input
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
  
  ## test accordion
  # output$riskAccordion <- renderUI({
  #   if(!(zipcode_d() %in% zips_sf$zipcode)) {
  #     return(NULL)
  #   } else {
  #   accordion(
  #     accordion_panel(
  #       "Temperature:",
  #       htmlOutput("tempDays_text"),
  #       plotOutput("temp_plot", height = 180)
  #     ),
  #     accordion_panel(
  #       "Standing Water:",
  #       plotOutput("water_plot", height = 170)
  #     )
  #   )
  #   }
  #   
  # })
  
  
  
  
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
  
  ## Filter temp data based on zip input
  temp_zip <- reactive({
    temp_filtered <- temp_zip_df %>% 
      filter(zipcode == zipcode_d(),
             date >= input$risk_dateRange[1] & date <= input$risk_dateRange[2]) 
    
    return(temp_filtered)
  })   
  ## Number of days at optimal temp
  optDays_int <- reactive({
    days = sum(temp_zip()$cx_opt == "optimal")
    return(days)
  })
  ## Percent of days at optimal temp
  optDays_per <- reactive({
    percent = round((optDays_int()/(nrow(temp_zip())))*100, 2)
    return(percent)
  })
  ## Number off days in Culex temp range
  rangeDays_int <- reactive({
    days = sum(temp_zip()$cx_opt == "in range")
    return(days)
  })
  ## Percent of days in Culex temp range
  rangeDays_per <- reactive({
    percent = round(((rangeDays_int()+optDays_int())/(nrow(temp_zip())))*100, 2)
    return(percent)
  })
  
  ## Reactive temp text
  output$tempDays_text <- renderText({
    ## If invalid zip, don't show text
    if(!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      paste0("In this time period, ", "<b>",optDays_int(), " days ","</b>",  "(", optDays_per(), "%)",  " fell within the optimal temperature range (red) and ", rangeDays_int(), " days (", rangeDays_per(),"%) fell within the thermal limits (orange) for WNV transmission by ", "<i>","Culex","</i>", " mosquitoes.")
    }
  })
  
  ## Daily temp plot
  output$temp_plot <- renderPlot({
    ## Doesn't appear until zip selected
    if (!(zipcode_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      ggplot(data = temp_zip(), aes(x = date, y = tmean_f)) +
        ## boxes to rep optimal Culex temp zones
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 89.42, ymin = 78.6,
                  alpha = 0.05, fill = "gray89")+
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 78.6, ymin = 73.2,
                  alpha = 0.05, fill = "gray81")+
        geom_rect(xmin = -Inf, xmax = Inf, ymax = 73.2, ymin = 53.78,
                  alpha = 0.05, fill = "gray89")+
        ## lines separating optimal zones
        geom_hline(yintercept = 89.42, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 78.6, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 73.2, linetype = "dashed", color = "gray50")+
        geom_hline(yintercept = 53.78, linetype = "dashed", color = "gray50")+
        ## filtered temp data
        geom_point(size = 3, 
                   alpha = 0.7,
                   aes(color = cx_opt)) +
        ## set colors based on optimal temp
        scale_color_manual(name = "",
                           values = c("firebrick2","goldenrod3", "dodgerblue"))+
        geom_line(linewidth = 0.7) +
        ## change labels/breaks based on time range
        (if (difftime(input$risk_dateRange[2],
                      input$risk_dateRange[1]) <= 31) {
          scale_x_date(date_labels = "%d\n%b")
        } else { 
          scale_x_date(date_labels = "%b %y")
        }) +
        ## labels/text options
        labs(y = "Mean daily temp (F)",
             x = element_blank()) +
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
        geom_point(color = "dodgerblue3",
                   size = 4,
                   alpha = 0.6) +
        geom_line(linewidth = 0.6, color = "dodgerblue4") +
        ## change labels/breaks based on time range
        (if (difftime(input$risk_dateRange[2],
                      input$risk_dateRange[1]) <= 31) {
          scale_x_date(date_labels = "%d\n%b")
        } else {
          scale_x_date(date_labels = "%b %y")
        }) +
        labs(y = "Surface water (acres)",
             x = element_blank()) +
        theme_classic() +
        theme(
          axis.title.y = element_text(vjust = 2, size = 14),
          axis.text = element_text(size = 13)
        )
    }
  })
  
  
  
  ### RISK MAP ---------------------------------------
  output$riskMap <- renderLeaflet({
    leaflet() %>% 
      ## Add background maps (OSM default)
      addTiles(group = "OpenStreetMaps") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      
      #### Add rasters
      ## Transmission efficiency
      # addRasterImage(r0_r, colors = r0_pal, 
      #                project = FALSE, group = "R0") %>%
      
      
      #### Add vectors 
      ## Zip codes
      addPolygons(data = zips_sf, color = "#343434",
                  weight = 2, fillOpacity = 0.1,
                  ## hover text
                  label = paste0("Zip code: ", zips_sf$zipcode),
                  labelOptions = labelOptions(textsize = "11px"),
                  ## hover highlight
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
      
      #### Create map groups
      addLayersControl(
        baseGroups = c("OpenStreetMaps", "World Imagery"),
        ## Add R0 layer here later
        overlayGroups = c("Zip codes", "Kern county", "Central Valley"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      
      ## Don't show all layers by default
      hideGroup(c("Central Valley")) %>% 
      
      #### Add map inset
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        zoomLevelOffset = -5,
        position = 'bottomleft',
        toggleDisplay = TRUE) %>% 
      
      setView(-119.2, 35.38, zoom = 10)
    
    #### Add legend 
    # addLegend(pal = r0_pal, values = values(r0_r),
    #           position = "topleft",
    #           title = "WNV Transmission </br> Efficiency") 
  }) ## END LEAFLET
  
  
  #### Interactive Leaflet elements: --------------------
  
  ## Click on zip code polygon to input value in text box
  observe({
    event <- input$riskMap_shape_click
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
    leafletProxy("riskMap") %>%
      ## remove previously selected zip
      clearGroup("highlighted_polygon") %>%
      ## zoom to zip
      flyToBounds(lng1 = bounds[1], lat1 = bounds[2],
                  lng2 = bounds[3], lat2 = bounds[4]) %>%
      ## highlight selected zip
      addPolylines(stroke=TRUE, weight = 5, color="yellow",
                   fill = TRUE, fillColor = "white", fillOpacity = 0.4,
                   data = geom, group = "highlighted_polygon")
  })
  
  
  
  
  # TAB 3: Standing Water ##################################################
  
  ## Zip code box ----------------
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
      ## Add background maps (satellite default)
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      addTiles(group = "OpenStreetMaps") %>%
      
      ## Zip codes
      addPolygons(data = zips_sf, color = "#1c1c1c",
                  weight = 2, fillOpacity = 0.1,
                  ## hover label
                  label = paste0("Zip code: ", zips_sf$zipcode),
                  labelOptions = labelOptions(textsize = "11px"),
                  ## hoever highlight
                  highlight = highlightOptions(weight = 5,
                                               color = "white",
                                               bringToFront = TRUE),
                  group = "Zip codes",
                  layerId = ~zipcode) %>% 
      
      ## Standing water
      addRasterImage(water_r, colors = water_pal,
                     # project = FALSE,
                     group = "Surface water") %>%
      
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
        baseGroups = c("World Imagery", "OpenStreetMaps"),
        overlayGroups = c("Surface water", 
                          "Zip codes", 
                          "Kern county", 
                          "Central Valley"),
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") %>% 
      
      ## Don't show all layers by default
      hideGroup(c("Kern county", "Central Valley")) %>%
      
      ## Add legend 
      addLegend(pal = water_pal, values = values(water_r),
                position = "bottomleft",
                opacity = 0.8,
                title = "Standing water </br> length (weeks):") %>% 
      
      setView(-119.2, 35.38, zoom = 9)
  }) ##END LEAFLET
  
  ### Interactive Leaflet elements: ---------
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
                   fill = TRUE, fillColor = "white", fillOpacity = 0.2,
                   data = geom, group = "highlighted_polygon")
  })
  
  
  
  ## VIDEO & PLOT ----------------------
  ## Put video and plot in same card
  ## seems to be best way to ensure equal height/width
  output$water_card <- renderUI({
    ## don't show unless zip selected
    if (!(zipcodeWater_d() %in% zips_sf$zipcode)) {
      return(NULL)
    } else {
      card(
        full_screen = TRUE,
        fill = TRUE,
        ## Split card into two columns
        layout_columns(
          col_widths = c(6, 6),
          ## video goes on left side
          tags$video(
            ## select by zip input
            src = paste0("vids/zip_",
                         zipcodeWater_d(),
                         "_2022_2023.mp4"),
            autoplay = TRUE,
            controls = TRUE,
            ## center vid in column when card is full-screen
            style="margin-top: auto; margin-bottom: auto;"),
          ## plot goes on right side
          plotlyOutput("waterTab_plot")
        ),##end columns
        ## insert responsive caption
        card_footer(htmlOutput("waterCard_caption"))
      )##End card
    }
  })
  
  ### Filter data -------------
  waterTab_data <- reactive({
    water_filtered <- water_zip_df %>% 
      filter(zipcode == zipcodeWater_d())
    return(water_filtered)
  })
  
  ### Water plot --------------
  output$waterTab_plot <- renderPlotly({
    ## wrap in plotly
    ggplotly(
      ggplot(data = waterTab_data(), 
             aes(x = date, y = acres_int)) +
        geom_point(
          color = "dodgerblue3",
          size = 2,
          alpha = 0.6,
          ## Define tooltip text
          aes(text = paste0(
            "Week:  ", date, 
            "<br>",
            "Acres:  ", round(acres_int, 2)
          ))
        ) +
        geom_line(linewidth = 0.6, color = "dodgerblue4") +
        labs(y = "Surface water (acres)",
             x = element_blank()) +
        scale_x_date(date_breaks = "2 month",
                     date_labels = "%b\n%Y") +
        theme_minimal() +
        theme(
          axis.title.y = element_text(vjust = 2, size = 12),
          axis.text = element_text(size = 10)
        ),
      tooltip = c("text")
    ) %>%
      ## customize plotly tool menu
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "select2d",
          "lasso2d",
          "resetScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      )
  })##End water plot
  
  ## CAPTIONS ----------------------
  ## Video & plot caption
  output$waterCard_caption <- renderText({
    paste0("Changes in acreage and location of surface water in zip code ", zipcodeWater_d(), " between 2022-2023. The video (left) displays where surface water was detected (blue) for each date with data. The plot (right) quantifies how surface water differed over time in zip code ", zipcodeWater_d(), ".")
  })
  
  
  # No server elements for Tab 4 
  
} ### END SERVER
