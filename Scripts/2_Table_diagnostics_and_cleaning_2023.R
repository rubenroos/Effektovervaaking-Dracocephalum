
## Table diagnostics, cleaning and export

# Written by Ida M. Mienna
# Adapted by Ruben E. Roos
# Date written: October 2024

# The table should have the same structure and colnames as in export_dirty.xlsx.
filepath_2023 <- "P:/154027_effektovervaking_av_trua_arter_og_naturtyper_2024/03 Dragehode/Dracocephalum/Effektovervaaking-Dracocephalum/Data/Effekt_dragehode_2023_processed/Effekt_dragehode_2023_export_dirty.xlsx"
filepath_2024 <- "P:/154027_effektovervaking_av_trua_arter_og_naturtyper_2024/03 Dragehode/Dracocephalum/Effektovervaaking-Dracocephalum/Data/Effekt_dragehode_2024_processed/Effekt_dragehode_2024_export_dirty.xlsx"


## What should the exported files be marked as?
project <- "Effekt_dragehode_2023-24"

## Path to folder where the processed directories and files should be exported
outputpath <- "P:/154027_effektovervaking_av_trua_arter_og_naturtyper_2024/03 Dragehode/Dracocephalum/Effektovervaaking-Dracocephalum/Data/Effekt_dragehode_cleaned"

## If you have collected high-precision GPS points, let collctd_hpcoords be TRUE 
## and add the full file path with the coordinates. The coordinates should be in
## a csv format as when exporting the GPS points report. If you do not have 
## collctd_hpcoords, change hpcoords to FALSE.
collctd_hpcoords <- TRUE 
hpcoordspath <- "P:/154027_effektovervaking_av_trua_arter_og_naturtyper_2024/03 Dragehode/Dracocephalum/Effektovervaaking-Dracocephalum/Data/GPS_effektovervåking_dragehode_16102023.csv"

# Which crs do the coordinates have? Please write either EPSG:XXXXX (see example)
hpcrs <- "EPSG:25832" # EUREF89 UTM sone 32N

## Export dataset after cleaning? (Yes=TRUE, No=FALSE)
export_clean <- TRUE


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

dat_2023 <- readxl::read_excel(filepath_2023)
dat_2024 <- readxl::read_excel(filepath_2024)

datasets <- list(dat_2023, dat_2024)

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

#### Select relevant columns

#Store columns we want in a list
selection <- list('parenteventid',
                  'polygon_id', 
                  'plot_id',
                  'treatment',
                  'cover_field_layer', 
                  'antall dragehoder: småplanter',
                  'antall dragehoder: vegetative planter', 
                  'antall dragehoder: fertile', 
                  'cover_bottom_layer', 
                  'cover_litter', 
                  'cover_rock',
                  'cover_bare_soil', 
                  'cover_woody_plants_field_layer', 
                  'cover_shrub_layer', 
                  'cover_tree_layer', 
                  'species', 
                  'species_cover',
                  'year',
                  'month',
                  'day')

# Function to select columns in multiple datasets
select_columns_multiple <- function(datasets, columns_to_select) {
  datasets <- lapply(datasets, function(df) {
    df %>% select(all_of(columns_to_select))
  })

# Assign the modified datasets back to their original names
list2env(setNames(datasets, names(datasets)), envir = .GlobalEnv)
}

# Put the datasets in a named list so they can be re-assigned correctly
datasets <- list(dat_2023 = dat_2023, dat_2024 = dat_2024)

# Apply the function and update the datasets in the global environment
select_columns_multiple(datasets, unlist(selection))

#### Renaming columns

# Store columns to be renamed in list
renames <- list(treatment_realised = 'treatment',
                count_dragehode_seedlings = 'antall dragehoder: småplanter',
                count_dragehode_vegetative = 'antall dragehoder: vegetative planter',
                count_dragehode_fertile = 'antall dragehoder: fertile')

# Function to select columns
select_columns_multiple <- function(datasets, columns_to_select) {
  datasets <- lapply(datasets, function(df) {
    df %>% select(all_of(columns_to_select))
  })
  # Overwrite the original datasets in the global environment
  list2env(setNames(datasets, names(datasets)), envir = .GlobalEnv)
}

# Function to rename columns based on existence
rename_if_exists <- function(df, renames) {
  for (new_name in names(renames)) {
    old_name <- renames[[new_name]][renames[[new_name]] %in% names(df)]
    if (length(old_name) > 0) {
      df <- df %>% rename(!!new_name := !!sym(old_name[1]))
    }
  }
  return(df)
}

# Apply renaming to multiple datasets
rename_if_exists_multiple <- function(datasets, renames) {
  datasets <- lapply(datasets, rename_if_exists, renames = renames)
  # Overwrite the original datasets in the global environment
  list2env(setNames(datasets, names(datasets)), envir = .GlobalEnv)
}

# Execute the selection and renaming functions on both datasets
datasets <- list(dat_2023 = dat_2023, dat_2024 = dat_2024)

# Apply selection
select_columns_multiple(datasets, unlist(selection))

# Apply renaming
rename_if_exists_multiple(datasets, renames)

#### Merge the two datasets from 2023 and 2024                           

#Check if the dataframes have equal structure                  
all.equal(dat_2023, dat_2024) #Only lengths and string values are different

dat <- bind_rows(dat_2023, dat_2024)

#### Cleaning ------------------------------------------------------------------

#Cleaning up
dat <- dat %>% 
  unite(plot_id_full, c("polygon_id", "plot_id"), remove= FALSE) %>% 
  mutate(treatment = case_when(str_detect(plot_id_full, "-IS-") ~ "ingen", 
                               str_detect(plot_id_full, "-S-") ~ "slått", 
                               TRUE ~ "NA"),
         presence_dragehode = case_when(str_detect(plot_id_full, "-ID") ~ "fravær", 
                                        str_detect(plot_id_full, "-D") ~ "present", 
                                        TRUE ~ "NA"), 
         treatment_realised = if_else(year == 2023, "ingen", treatment))
  
#### Coordinates ---------------------------------------------------------------

## Fix plot coordinates

hpcoords <- hpcoords %>%
  
  # Rename columns
  setNames( c("Point", "Y", "X", "Elev"))

# Also check if the accuracy of the coordinates are good enough.


#### Species names -------------------------------------------------------------

#Explore species names

#unique_species <- dat %>% 
#  distinct(species) %>%
#  arrange(species)  

# Fix some wrongly or inconsistently named species
dat <- dat %>%
  # Rename plot id
  mutate(species = case_when(
    species == "Achillea" ~ "Achillea_millefolium",
    species == "Alchemilla" ~ "Alchemilla_sp", 
    species == "Knautia" ~ "Knautia_arvensis", 
    species == "Rosa" ~ "Rosa_sp",
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
##                             Export data                                 
## ---------------------------------------------------------------------------##

#Export xlsx file
write.xlsx(dat, "P:/154027_effektovervaking_av_trua_arter_og_naturtyper_2024/03 Dragehode/Dracocephalum/Effektovervaaking-Dracocephalum/Data/Effekt_dragehode_cleaned/Dragehode_2024_cleaned.xlsx")



################## Not updated below! ##########################################

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
    mutate(plot_id_full=sub("(.+?)(\\_.*)","\\1",Point))
  
  # Combine hpcoords with main dataset
  dathpcoords <- hpcoords %>%
    full_join(dat %>% select(plot_id_full, gps) %>% unique())
  
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