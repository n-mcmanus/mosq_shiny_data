# wnv_shiny_ERI:
This project contains all the data and code necessary for running the Kern MBD Shiny app. Additionally, it contains the script (`data_prep.Rmd`) used for generating the data used in the Shiny. For reasons I haven't yet solved, the Shiny cannot be properly hosted if the project contains this data prep script. Therefore, all data and code necessary for running the shiny is duplicated into a separate repository (link here) to be properly hosted. As a result, this project is primarily used for generating/wrangling data, while the (other project) is primarily used for hosting the shiny. It's recommended any future changes to the Shiny be duplicated in both repositories to avoid confusion. 
 

## File structure:
* `data_prep.Rmd`: the R markdown used for generating tabular, raster, and video data used in the Shiny app.
* `wnv_shiny/`: this directory contains all the script necessary for running the Shiny, which includes the `server.R`, `ui.R`, and a custom CSS file (`styles.css`). 
    * `www/`: folder with images and videos included in the Shiny.
* `data/`:  contains the raw and generated data used in the Shiny
    * `central_valley/`: contains the associated SHP files for the entire CA central valley (Alluvial_Bnd.shp) as well as the portion of the valley only within Kern County (valley.shp)
    * `counties_ca/`: contains the associated SHP files for all counties in CA (cnty19_1.shp) as well as Kern county (kern.shp)
    * `zipcodes/`: contains the associated SHP files for all zip codes in CA (CA_Zips.shp) as well as the portion of zip codes only located within both Kern County and the CA central valley (kern_zips.shp)
    * `temp/`: contains mean daily air temperature data sourced from PRISM.
        * `GEE_script.txt` is a copy of the JavaScript code used in Google Earth Engine to pull average daily temperature by zip code in Kern county.
        * `kern_tmean_GEE_output.csv` is the output generated from the GEE code.
        * `kerm_tmean_2010_20230930.csv` is a wrangled copy of the temperature data, including categorical information on whether a day falls within the suitable or optimal range for *Culex* spp mosquitoes. This is the file used for temperature plots in the shiny.
    * `traps/`: mosquito abundance and MIR data sourced from the Kern Mosquito & Vector Control District.
        * `andy/`: contains the KMVCD abundance and MIR data that has been processed and provided by Andrew MacDonald
        * `plotting/`: the wrangled version of the abundance and MIR data used for generating plots in the shiny app.
    * `water/`: contains all the surface water data for 2022-2023
        * `Landsat_Dan/`: contains all the water imagery data provided by Dan Souza, which includes Landsat images (30m rasters) processed for surface water ("UnmixedMask") as well as the files used for cropping and masking individual images ("QA_PIXEL"). Landsat images cover rows 35 and 36 inside both paths 41 and path 42; separate files are created for each path/row (e.g. "p042r035"), which are further separated by year (2022 and 2023).
        * `p042_masked_merged/`: contains the masked and merged water images by year (2022 or 2023). Images for row 35 and 36 inside path 42 were merged then cropped to Kern county. Only path 42 was processed, since this covers the current extent of our area of interest.
        * `summed_water_30m_2022_2023.tif`: all the acceptable images (no cloud interference) for 2022-2023 in path 42 were added to create this "heatmap", in which the value of a pixel indicates how many dates surface water was present (max 35). This raster is at 30m resolution.
        * `summed_water_90m_2022_2023.tif`: the previous raster, but upscaled to 90m resolution and then reprojected into a Leaflet-friendly CRS to align with the basemap. This raster is displayed on the surface water map in the shiny.
        * `water_acre_zipcode.csv`: zonal information on the acreage and cell count of surface water by zip code in Kern county. This data is used for generating water plots in the shiny app.
