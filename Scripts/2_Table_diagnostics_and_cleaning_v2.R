
## Table diagnostics, cleaning and export

# Written by Ida M. Mienna
# Date written: October 2024

# The table should have the same structure and colnames as in export_dirty.xlsx.
filepath <- "C:/Users/ida.mienna/OneDrive - NINA/Dokumenter/Prosjekter/Survey123_R_data/Survey123_data/Processed/Effekt_GRUK_processed/Effekt_GRUK_export_dirty.xlsx"

## What should the exported files be marked as?
project <- "Effekt_GRUK"

## Path to folder where the processed directories and files should be exported
outputpath <- "C:/Users/ida.mienna/OneDrive - NINA/Dokumenter/Prosjekter/Survey123_R_data/Survey123_data/Processed/Effekt_GRUK_processed/"

## If you have collected high-precision GPS points, let collctd_hpcoords be TRUE 
## and add the full file path with the coordinates. The coordinates should be in
## a csv format as when exporting the GPS points report. If you do not have 
## collctd_hpcoords, change hpcoords to FALSE.
collctd_hpcoords <- F

# Which crs do the coordinates have? Please write either EPSG:XXXXX (see example)
hpcrs <- "EPSG:25832" # EUREF89 UTM sone 32

# When you have edited all the points above to your liking, click "Source" 
# or Ctrl+Shift+Enter and continue to the next sections.


## ---------------------------------------------------------------------------##
##                           DATA CLEANING 
##                     CLEAN NAMES AND COORDINATES
## ---------------------------------------------------------------------------##

# Here, you should do the cleaning you otherwise would have done in Excel.
# The reason for doing this here is to have a reproducible method that others,
# but also you, can run through the data cleaning and end up with the same
# results as before.

# The data cleaning diagnostics itself will be run below under the section "DO 
# NOT CHANGE BELOW". This is to (hopefully) make it more obvious which parts in 
# the table that needs to be changed. I have added typical themes below but these
# can be further worked on beneath "DO NOT CHANGE BELOW" if needed.

## Export files for diagnostics? (Yes=TRUE, No=FALSE)
export_diagnostics <- TRUE

dat <- readxl::read_excel(filepath)

if(collctd_hpcoords){
  hpcoords <- read.table(hpcoordspath, header = FALSE, sep = ",", 
                         stringsAsFactors = FALSE, fill = TRUE, skip=1)
}



## DO NOT CHANGE (IMPORTING PACKAGES) ---------------------------------------------------------------
Sys.setlocale(locale='no_NB.utf8')

## Packages
packages <- c("dplyr", "readxl", "sf", "tidyr", "openxlsx", "stringr",
              "httr", "jsonlite", "purrr", "data.table", "readr", "mapview")

# Require or install packages function
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

## -----------------------------------------------------------------------------


#### Plots ---------------------------------------------------------------------





#### Coordinates ---------------------------------------------------------------

## Fix plot coordinates

# EXAMPLE:
# If there are rows that should be removed
hppointrem <- c("RTCM0446", "4H1_sw", "4H4_sw", "9L1_sw", "5M3_sw", "10H5_np",
               "6R2_np", "V0104")
hpplotrem <- c(
  "10H1", "10H2", "10H3", "10H4", "10H5", "10L1", "10L2", "10L3", "10L4", "10L5",
  "10M1", "10M2", "10M3", "10M4", "10M5", "10R1", "10R2", "10R3", "10R4", "10R5",
  "11H1", "11H2", "11H3", "11H4", "11H5", "11L1", "11L2", "11L3", "11L4", "11L5",
  "11M1", "11M2", "11M3", "11M4", "11M5", "11R1", "11R2", "11R3", "11R4", "11R5",
  "12H1", "12H2", "12H3", "12H4", "12H5", "12L1", "12L2", "12L3", "12L4", "12L5",
  "12M1", "12M2", "12M3", "12M4", "12M5", "12R1", "12R2", "12R3", "12R4", "12R5",
  "2H1", "2H2", "2H3", "2H4", "2H5", "2L1", "2L2", "2L3", "2L4", "2L5",
  "2M1", "2M2", "2M3", "2M4", "2M5", "2R1", "2R2", "2R3", "2R4", "2R5",
  "3H1", "3H2", "3H3", "3H4", "3H5", "3L1", "3L2", "3L3", "3L4", "3L5",
  "3M1", "3M2", "3M3", "3M4", "3M5", "3R1", "3R2", "3R3", "3R4", "3R5",
  "4H1", "4H2", "4H3", "4H4", "4H5", "4L1", "4L2", "4L3", "4L4", "4L5",
  "4M1", "4M2", "4M3", "4M4", "4M5", "4R1", "4R2", "4R3", "4R4", "4R5",
  "5H1", "5H2", "5H3", "5H4", "5H5", "5L1", "5L2", "5L3", "5L4", "5L5",
  "5M1", "5M2", "5M3", "5M4", "5M5", "5R1", "5R2", "5R3", "5R4", "5R5",
  "6H1", "6H2", "6H3", "6H4", "6H5", "6L1", "6L2", "6L3", "6L4", "6L5",
  "6M1", "6M2", "6M3", "6M4", "6M5", "6R1", "6R2", "6R3", "6R4", "6R5",
  "8H1", "8H2", "8H3", "8H4", "8H5", "8L1", "8L2", "8L3", "8L4", "8L5",
  "8M1", "8M2", "8M3", "8M4", "8M5", "8R1", "8R2", "8R3", "8R4", "8R5",
  "9H1", "9H2", "9H3", "9H4", "9H5", "9L1", "9L2", "9L3", "9L4", "9L5",
  "9M1", "9M2", "9M3", "9M4", "9M5", "9R1", "9R2", "9R3", "9R4", "9R5"
)

hpcoords <- hpcoords %>%
  
  # Rename columns
  setNames( c("Point", "Y", "X", "Elev", "NA")) %>%
  
  # Remove specific rows from a column (here: Point) that have values listed in
  # hppointrem
  filter(! Point %in% hppointrem) %>%
  
  # Remove specific rows from a column (here: Point) that have values that have
  # specific words, numbers etc. that matches what we have listed in hpplotrem.
  filter(! grepl(paste0(hpplotrem, collapse="|"),Point)) %>%
  
  # Fix writing errors in plot names
  mutate(Point = case_when(
    
    # When there is a specific name
    Point == "VR1_sv" ~ "VR101_sv", # Where Point=VR1 it should instead be VR101
    
    # When there is a pattern
    grepl("^VR[1-5]", Point) ~ sub("^VR([1-5])", "VR10\\1", Point), # VR2 to VR5 should be VR102 to VR105
    grepl("^MR[1-5]", Point) ~ sub("^MR([1-5])", "MR10\\1", Point), # MR1 to (same as above)
    grepl("^BR[1-5]", Point) ~ sub("^BR([1-5])", "BR10\\1", Point), # BR1 to (same as above)
    
    grepl("nj", Point) ~ sub("nj", "no", Point), # Should be no and not nj
    
    TRUE ~ Point
    
  ))

# Also check if the accuracy of the coordinates are good enough.


#### Species names -------------------------------------------------------------

## EXAMPLE:
# Fix spelling errors in the dataset.
dat <- dat %>%
  # Rename plot id
  mutate(species = case_when(
    species == "Arctostaphylos_uva-ursi_var._uva-ursi" ~ "Arctostaphylos_uva-ursi",
    species == "Salix_glauca_subsp._glauca" ~ "Salix_glauca",
    
    TRUE ~ species
    
  ))


## ---------------------------------------------------------------------------##
##                            DATA CLEANING 
##                         VALIDATE GIS DATA
## ---------------------------------------------------------------------------##

# When you have cleaned the plot coordinates, you can further check if the plots
# are OK for analyses (e.g. use as ground truth for remote sensing). A map will
# pop up to in your Viewer-console (by default in your bottom right) where you
# can zoom in on your data. Hold the cursor above a plot to see its label.

# Only have check_GIS as TRUE when coordinates are cleaned. If you did not use a 
# high-precision GPS, this step will not work so change check_GIS to FALSE.

# Is the coordinate data ready to be checked for GIS errors? (Yes=TRUE, No=FALSE)
check_GIS <- TRUE


## ---------------------------------------------------------------------------##
##                            DATA CLEANING 
##                        VALIDATE COVERAGE DATA
## ---------------------------------------------------------------------------##

# Fix later?


## ---------------------------------------------------------------------------##
##                             EXPORT DATA                                 
## ---------------------------------------------------------------------------##

# A folder for exports will be created (if not already existing).

##  EXPORT FOR DATA ANALYSES ---------------------------------------------------                           

# Will export two datasets: one for analysing plot data and one for species
# Plot dataset will have coverage data and numbers (nr of species etc.)
# Species dataset will have species as columns and plot IDs as rows.

# Should only by TRUE when data is cleaned.
export_analyses <- TRUE

##  EXPORT FOR DARWIN CORE -----------------------------------------------------                           

# Will export a dataset that is in Darwin core format.
# Should only by TRUE when data is cleaned.
export_darwin <- FALSE

## FIX LATER


## ---------------------------------------------------------------------------##
##                             RUN SCRIPT                                 
## ---------------------------------------------------------------------------##

# When you have edited all the points above to your liking, click "Source" 
# or Ctrl+Shift+Enter.



####------------------------------------------------------------------------####
####                       DO NOT CHANGE BELOW
####------------------------------------------------------------------------####

## ---------------------------------------------------------------------------##
##                            DATA CLEANING                                   ##
##                     CLEAN NAMES AND COORDINATES
## ---------------------------------------------------------------------------##

# Create a folder for diagnostics (if it does not exist)
diagnostics_folder <- paste0(outputpath, "Diagnostics/")
if (!dir.exists(diagnostics_folder)) {
  dir.create(diagnostics_folder)
  cat(paste0("Directory created for diagnostics: ", diagnostics_folder))
}

## Plot IDs ----------------------------------------------------------------- ##

# Should be a table with plot IDs, locality, registration date, how often the ID
# occur in the dataset.

datplot <- dat %>%
  select(any_of(c("plot_id", "eventTime", "hovedoekosystem_punkt", "kartleggingsenhet_1m2"))) %>%
  group_by(across(everything())) %>%
  summarise(plotid_nr=n()) %>%
  unique()
  

#Export files for diagnostics
if(export_diagnostics) {
  
  ## Export README file
  # Define the lines of text
  README_lines <- paste0(c(
    "The file _plot_names.xlsx includes the plot names in the dataset, when they",
    "were registered (date/eventTime), ecosystem type and mapping unit, and number",
    "of times this combination of columns are represented in the dataset. This",
    "will likely represent the number of species registrered in each plot.",
    "",
    "Nr of unique plots present in dataframe:",
    paste0(length(unique(datplot$plot_id)))
  ))
  
  # Write the text to a .txt file
  writeLines(README_lines, con = paste0(diagnostics_folder,"README_plot_names.txt"))
  
  ## Export nortaxa
  datplot %>% write.xlsx(file = paste0(diagnostics_folder, project,"_plot_names.xlsx"), colNames = TRUE)
  
}


## Plot coordinates ---------------------------------------------------------- ##

## Add high-precision coordinates to the plots. If a plot (or all plots) lack
## coordinates, then use coordinates from Survey123.
if(collctd_hpcoords){
  hpcoords <- hpcoords %>%
    # Select only first four columns
    select(1:4) %>%
    # Rename columns
    setNames( c("Point", "Y", "X", "Elev")) %>%
    # Get plot id
    mutate(plot_id=sub("(.+?)(\\_.*)","\\1",Point))
  
  # Combine hpcoords with main dataset
  dathpcoords <- hpcoords %>%
    full_join(dat %>% select(plot_id, gps) %>% unique())
  
  # If any plot_ids are NA, fill in with Survey123. These coordinates are in lat/long
  # and need to be transformed
  
  hpcoordsNA <- unique(dathpcoords$plot_id[is.na(dathpcoords$Y)])
  
  survey123coords <- dat %>%
    filter(plot_id %in% hpcoordsNA) %>%
    select(plot_id, longitude, latitude) %>%
    unique() %>%
    na.omit() %>%
    st_as_sf(coords=c("longitude", "latitude"), crs="EPSG:4326") %>%
    st_transform(crs=hpcrs)
  
  coordssurvey123 <- st_coordinates(survey123coords)
  
  survey123coords <- survey123coords %>%
    cbind(coordssurvey123) %>%
    st_drop_geometry()%>%
    mutate(gps="Survey123")
  
  dathpcoords <- dathpcoords %>%
    full_join(survey123coords)
  
  
  plotcoordsnr <- dathpcoords %>%
    # Calculate number of rows per plot_id (should be two)
    group_by(plot_id) %>%
    summarise(nr_coords = n()) %>%
    ungroup()
  
  dathpcoords <- dathpcoords %>%
    left_join(plotcoordsnr) %>%
    filter(!is.na(Y))
  
  #Export files for diagnostics
  if(export_diagnostics) {
    
    ## Export README file
    # Define the lines of text
    README_lines <- paste0(c(
      "The file _plot_coords.xlsx includes the high precision coordinates",
      "and Survey123 coordinates if the high precision ones were missing.",
      "Check for writing errors in names and if any plot_ids have more than two",
      "coordinates. Also check if the corner coordinates are correct in regards to",
      "sv and no (should not be sw or ne, but if they should be, the scripts below",
      "needs to be edited to match this."
    ))
    
    # Write the text to a .txt file
    writeLines(README_lines, con = paste0(diagnostics_folder,"README_coords.txt"))
    
    ## Export coordinates file
    dathpcoords %>% write.xlsx(file = paste0(diagnostics_folder, project,"_plot_coords.xlsx"), colNames = TRUE)
    
    dathpcoords <- dathpcoords %>%
      rename("UTMost" = X,
             "UTMnord" = Y,
             "minimumElevationInMeters" = "Elev") %>%
      mutate(UTMsone = substr(hpcrs, nchar(hpcrs)-1, nchar(hpcrs)))
    
    # Add coords file to main dat-file
    dat <- dat %>%
      # Remove original gps column
      select(-gps) %>%
      full_join(dathpcoords) %>%
      select(-nr_coords)
    
  }
}

## Species names ------------------------------------------------------------ ##

# Use API to Artsnavnebasen to find accepted species name and popular name

# Function for getting names
nortaxa_names <- function(speciesname) {
  
  # Use tryCatch to handle errors and return a dataframe with original_name and NAs
  tryCatch({
    # Replace underscores with plus signs for URL encoding
    speciesnameURL <- gsub("_", "+", speciesname)
    speciesURL <- paste0("https://nortaxa.artsdatabanken.no/api/v1/TaxonName/Search?Search=", speciesnameURL)
    
    # Make the API call
    r <- GET(url = speciesURL)
    
    # Check if the API request was successful
    if (http_type(r) != "application/json" || status_code(r) != 200) {
      warning(paste("API request failed or did not return JSON for species:", speciesname))
      # Return dataframe with speciesname and NA for other columns
      return(data.frame(original_name = speciesname, taxonId = NA, acceptedScientificName.name = NA, pref_popular_name = NA, stringsAsFactors = FALSE))
    }
    
    # Parse the response content
    r_content <- content(r, as = "text", encoding = "UTF-8")
    
    # Ensure valid JSON is returned
    if (nchar(r_content) == 0) {
      warning(paste("API returned empty content for species:", speciesname))
      # Return dataframe with speciesname and NA for other columns
      return(data.frame(original_name = speciesname, taxonId = NA, acceptedScientificName.name = NA, pref_popular_name = NA, stringsAsFactors = FALSE))
    }
    
    # Convert JSON to dataframe
    df <- fromJSON(r_content, flatten = TRUE)
    
    # Only keep accepted names and filter based on speciesURL + signs and rank
    df_filtered <- df %>%
      filter(
        case_when(
          str_count(speciesURL, "\\+") == 0 ~ acceptedScientificName.rank == "Genus",
          str_count(speciesURL, "\\+") == 1 ~ acceptedScientificName.rank == "Species",
          str_count(speciesURL, "\\+") == 2 ~ acceptedScientificName.rank == "Subspecies",
          str_count(speciesURL, "\\+") == 3 ~ acceptedScientificName.rank == "Variety",
          TRUE ~ FALSE  # Default case if no condition matches
        ),
        acceptedScientificName.isSearchMatch  # Filter for search match
      )
    
    # If no rows pass the filter, return speciesname with NA for other columns
    if (nrow(df_filtered) == 0) {
      return(data.frame(original_name = speciesname, taxonId = NA, acceptedScientificName.name = NA, pref_popular_name = NA, stringsAsFactors = FALSE))
    }
    
    # Add the speciesname and first popular name if it exists
    df_final <- df_filtered %>%
      mutate(pref_popular_name = map_chr(preferredVernacularNames, ~ if (!is.null(.x) && length(.x$name) > 0) {.x$name[1]} else {NA_character_}),
             original_name = speciesname) %>%
      # Reorder the columns
      select(original_name, taxonId, acceptedScientificName.name, pref_popular_name) %>%
      slice(1)
    
    return(df_final)  # Return the processed dataframe
    
  }, error = function(e) {
    # Handle any error that occurs in the tryCatch block
    warning(paste("Error processing species:", speciesname, " - ", e$message))
    # Return a dataframe with original_name and NA for other columns
    return(data.frame(original_name = speciesname, taxonId = NA, acceptedScientificName.name = NA, pref_popular_name = NA, stringsAsFactors = FALSE))
  })
}


# Apply the function to each URL in the list
nortaxa_species <- lapply(dat$species, nortaxa_names) %>%
  # Bind lists together to one dataframe
  bind_rows() %>%
  # Only unique rows
  unique() %>%
  # Order by original name
  arrange(original_name)

# Get number of times the different species occur in the dataset
speciesnr <- dat %>%
  group_by(species) %>%
  summarise(species_nr=n())

nortaxa_species <- nortaxa_species %>%
  left_join(speciesnr %>% rename("original_name" = species)) %>%
  na.omit()


#Export files for diagnostics
if(export_diagnostics) {
  
  ## Export README file
  # Define the lines of text
  README_lines <- paste0(c(
    "The file _species_names.xlsx includes the original name found in the Survey123 dataset",
    "and what name it matches to in Artsdatabanken. Here you can also see which names have writing errors etc."
  ))
  
  # Write the text to a .txt file
  writeLines(README_lines, con = paste0(diagnostics_folder,"README_speciesnames.txt"))
  
  ## Export nortaxa
  nortaxa_species %>% write.xlsx(file = paste0(diagnostics_folder, project,"_species_names.xlsx"), colNames = TRUE)
  
}



## ---------------------------------------------------------------------------##
##                            DATA CLEANING                                   ##
##                            VALIDATE PLOTS
## ---------------------------------------------------------------------------##

# When you have cleaned the plot coordinates, you can further check if the plots
# are ok for analyses (e.g. use as ground truth for remote sensing).

if(check_GIS){
  sfhpcoords <- st_as_sf(hpcoords, coords=c("X", "Y"), crs=hpcrs)
  
  # Get plot corners
  plotcoordcor <- dat %>%
    select(plot_id, Point, UTMnord, UTMost) %>%
    unique() %>%
    mutate(corner=str_extract(Point, "(?<=_).*")) %>%
    select(-Point) 
  
  # Which have only one coordinate and no corners
  plotcoordc <- plotcoordcor %>%
    filter(is.na(corner))
  
  # Make coordinate file into wide format
  plotcoordw <- plotcoordcor %>%
    filter(!is.na(corner)) %>%
    rename("y"=UTMnord,
           "x"=UTMost) %>%
    pivot_wider(names_from = corner, 
                values_from = c(y, x))
  
  
  # Get coordinates for the other corners
  xc <- (plotcoordw$x_no + plotcoordw$x_sv)/2 ; yc <- (plotcoordw$y_no + plotcoordw$y_sv)/2
  xd <- (plotcoordw$x_no - plotcoordw$x_sv)/2 ; yd <- (plotcoordw$y_no - plotcoordw$y_sv)/2
  
  plotcoordw$y_nv <- yc + xd ; plotcoordw$x_nv <- xc - yd
  plotcoordw$y_so <- yc - xd ; plotcoordw$x_so <- xc + yd
  
  # Reshape the table
  plotcoords <- plotcoordw %>%
    pivot_longer(
      cols = starts_with(c("x_", "y_")),
      names_to = c(".value", "corner"),
      names_sep = "_") %>%
    rename("UTMnord"=y,
           "UTMost"=x)
  
  # Make into point file
  sfplotpoints <- st_as_sf(plotcoords, coords = c("UTMost", "UTMnord"), crs = hpcrs)
  
  # Make into polygon
  sfplotpolygons <- sfplotpoints %>%
    #st_as_sf(coords = c("UTMost", "UTMnord")) %>%
    group_by(plot_id) %>%
    summarize(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    left_join(plotcoords %>% select(-corner,-UTMost,-UTMnord), by="plot_id")
  
  sfplotpolygons <- sfplotpolygons %>%
    mutate(area=st_area(sfplotpolygons)) %>%
    unique()
  
  # Plot map
  mapview(sfplotpolygons, 
          col.regions = "darkred",
          label = "plot_id", 
          layer.name = "plot_id",
          legend=FALSE)
  
}

## ---------------------------------------------------------------------------##
##                             EXPORT DATA                                 
## ---------------------------------------------------------------------------##

# Create a folder for diagnostics (if it does not exist)
exports_folder <- paste0(outputpath, "Exports/")
if (!dir.exists(exports_folder)) {
  dir.create(exports_folder)
  cat(paste0("Directory created for exports: ", exports_folder))
}


## DATA ANALYSES -------------------------------------------------------------##
if(export_analyses) {
  
  
  ## Species dataset
  
  # Wide format
  dat_species <- dat %>%
    select(any_of(c("plot_id", "species", "sub_plots", "cover_species"))) %>%
    unique() %>%
    mutate(freq_sub_plots = if ("sub_plots" %in% colnames(.)) {
      str_count(sub_plots, ",") + 1
    } else if ("cover_species" %in% colnames(.)) {
      as.numeric(cover_species) / 100
    } else {
      NA_real_  # Assign NA if neither column exists
    }) %>%
    select(-any_of(c("sub_plots","cover_species"))) %>%
    arrange(species)

  # Assuming your data is in a data frame called df
  dat_speciesw <- dat_species %>%
    pivot_wider(names_from = species, 
                values_from = freq_sub_plots)
  
  
  ## Plot dataset
  
  # Number of species
  datspeciesnr <- dat %>%
    group_by(plot_id) %>%
    summarise(nr_species = n_distinct(species, na.rm=T))
  
  # Dataset
  dat_plot <- dat %>% 
    #select(plot_id, kartleggingsenhet_1m2,
    #       dek_busk_felt_bunn, cover_plants, busksjikt, cover_plants_summed,
    #       cover_bryophytes, cover_litter, cover_rock, cover_bare_soil, cover_crust) %>%
    unique() %>%
    left_join(datspeciesnr)
  
  ## Export datasets
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add sheets to the workbook
  addWorksheet(wb, "plots")
  addWorksheet(wb, "species_long")
  addWorksheet(wb, "species_wide")
  
  
  # Write the data frames to the respective sheets
  writeData(wb, sheet = "plots", dat_plot, na.string = "")
  writeData(wb, sheet = "species_long", dat_species, na.string = "")
  writeData(wb, sheet = "species_wide", dat_speciesw, na.string = "")
  
  # Save the workbook to an Excel file
  wb %>% saveWorkbook(file = paste0(exports_folder, project,"_data_analyses.xlsx"), overwrite = T)
  
  
}


## DARWIN CORE ---------------------------------------------------------------##

if(export_darwin) {

  
}