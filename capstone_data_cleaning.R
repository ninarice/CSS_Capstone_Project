
# inner join
library(sf)
library(dplyr)

census_sf <- st_read("~/Documents/Capstone/census_data/CES4 Final Shapefile.shp")
sdi <- read.csv("~/Documents/Capstone/sdi_data.csv")

# Rename FIPS column for merging
census_sf <- census_sf %>%
  rename(CENSUSTRACT_FIPS = Tract)

# Merge census shapefile with SDI data using inner join
merged_sf <- census_sf %>%
  inner_join(sdi, by = "CENSUSTRACT_FIPS")

#colnames(merged_sf)
merged_sf <- merged_sf[c("CENSUSTRACT_FIPS", "ZIP", "County", "ApproxLoc", "TotPop19", 
                      "Poverty", "PovertyP", "Hispanic", "White", "AfricanAm", "NativeAm",
                      "OtherMult", "SDI_score", "sdi", "geometry")]

# Ensure the FIPS codes are treated as characters first, especially if they might lose leading zeros
merged_sf$CENSUSTRACT_FIPS <- as.character(merged_sf$CENSUSTRACT_FIPS)

# Now format with leading zeros using sprintf
merged_sf$CENSUSTRACT_FIPS <- sprintf("%011s", merged_sf$CENSUSTRACT_FIPS)

# Save merged file
st_write(merged_sf, "~/Documents/Capstone/sdiandsomecensus.shp")

########################
### PULLING API DATA ###
########################


library(httr)
library(jsonlite)
library(pbapply)

# Constants
base_url <- "https://api.census.gov/data/2019/acs/acs5"

# Variables updated to include population under 21, median HH income, and non-Hispanic white population
variables <- c(
  "B01001_003E", "B01001_004E", "B01001_005E", "B01001_006E",  # Males under 17
  "B01001_007E", "B01001_008E",  # Males 18 to 20
  "B01001_027E", "B01001_028E", "B01001_029E", "B01001_030E",  # Females under 17
  "B01001_031E", "B01001_032E",  # Females 18 to 20
  "B19013_001E",  # Median household income
  "B03002_003E"   # Non-Hispanic White population
)

var_str <- paste(variables, collapse = ",")
api_key <- "0a21a704689f6a096666e597aeab0330ad4d3b02"

# Helper function to fetch and process data
get_census_data <- function(fips) {
  url <- paste0(base_url, "?get=", var_str, "&for=tract:", substr(fips, 6, 11),
                "&in=state:", substr(fips, 1, 2), "&in=county:", substr(fips, 3, 5),
                "&key=", api_key)
  response <- GET(url, timeout(60))
  Sys.sleep(1)  # Avoid hitting rate limit
  
  if (status_code(response) == 200) {
    content_text <- content(response, "text", encoding = "UTF-8")
    df <- tryCatch({
      fromJSON(content_text, flatten = TRUE)
    }, error = function(e) {
      message(paste("Error parsing JSON for FIPS:", fips, ":", e$message))
      NULL
    })
    
    if (!is.null(df) && nrow(df) > 0) {
      # Remove the first row which is the header from the API
      df <- df[-1, ]
      if (length(df) == length(variables) + 3) {  # +3 for state, county, tract
        names(df) <- c(variables, "state", "county", "tract")
      } else {
        message("Column mismatch, check variable count and API response structure.")
      }
      return(df)
    } else {
      message(paste("No data returned for FIPS:", fips))
      return(data.frame())
    }
  } else {
    message(paste("Failed to fetch data for FIPS:", fips, "with status:", status_code(response)))
    return(data.frame())
  }
}

# Fetch and combine data for all FIPS codes
fips_codes <- merged_sf$CENSUSTRACT_FIPS  # Use all FIPS codes instead of limiting to 5
census_data <- pblapply(fips_codes, get_census_data)
census_data_df <- bind_rows(census_data)

# Transform the DataFrame
census_data_df <- census_data_df %>%
  mutate(
    # Combine male and female population under 21 into one column
    PopulationUnder21 = as.numeric(B01001_003E) + as.numeric(B01001_004E) +
      as.numeric(B01001_005E) + as.numeric(B01001_006E) +
      as.numeric(B01001_007E) + as.numeric(B01001_008E) +
      as.numeric(B01001_027E) + as.numeric(B01001_028E) +
      as.numeric(B01001_029E) + as.numeric(B01001_030E) +
      as.numeric(B01001_031E) + as.numeric(B01001_032E),
    # Concatenate state, county, and tract into a single column
    FIPScode = paste0(state, county, tract)
  ) %>%
  # Select and rename columns
  select(
    FIPScode,
    PopulationUnder21,
    MedianHHincome = B19013_001E,
    NonHispanicWhite = B03002_003E
  )

# Output the transformed DataFrame
print(census_data_df)

write_csv(census_data_df, "~/Documents/Capstone/census_byfips.csv")

(as.numeric(census_data_df$NonHispanicWhite[1])) / as.numeric(merged_sf$TotPop19[1])
  
##### merging all census and sdi together  

# Rename FIPS column for merging
census_data_df <- census_data_df %>%
  rename(CENSUSTRACT_FIPS = FIPScode)

SE_sf <- merged_sf %>%
  inner_join(census_data_df, by = "CENSUSTRACT_FIPS")
SE_sf

st_write(SE_sf, "~/Documents/Capstone/SE.shp")


savetest <- st_read("~/Documents/Capstone/SE.shp")


###################
#NORMALIZE SE DATA#
###################

SE_sf <- st_read("~/Documents/GitHub/CSS_Capstone_Project/SE_data/SE.shp")
normalized_SE_sf <- SE_sf %>%
  mutate(
    Under21_per_cap = PpltU21 / TotPp19,
  )

st_write(normalized_SE_sf, "~/Documents/GitHub/CSS_Capstone_Project/normalized_SE_sf.shp")
savetest <- st_read("~/Documents/GitHub/CSS_Capstone_Project/normalized_SE_sf.shp")

#####################
# PILOT DATA merged #
#####################


# Read the retailer data from CSV
pilot <- read.csv("~/Downloads/Finalized E-commerce Vape Shops After Filtering B&C - List of Brick Retailers.csv")

# Convert to sf object, assuming columns are named 'lat' and 'lon'
pilot_ecomm <- st_as_sf(pilot, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Transform the retailer data to match the CRS of the socioeconomic data
pilot_ecomm <- st_transform(pilot_ecomm, st_crs(normalized_SE_sf))

# Optionally isolate rows where e-commerce = 1
# pilot_ecomm <- pilot_ecomm[pilot_ecomm$ecommerce_flag == 1, ]

# Find intersections between e-commerce locations and SE data
intersections <- st_intersects(pilot_ecomm, normalized_SE_sf)

# Notify if some intersections are empty
if (any(lengths(intersections) == 0)) {
  message("Some retailers do not intersect with any census tracts.")
}

# Generate index dataframe for mapping manually
index_df <- do.call(rbind, lapply(seq_along(intersections), function(i) {
  if (length(intersections[[i]]) > 0) {
    data.frame(retailer_index = i, tract_index = intersections[[i]])
  }
}))

# Merge attributes from socioeconomic data
attributes <- normalized_SE_sf %>%
  mutate(tract_index = 1:n())  # Adding an index to join on

# Join attributes to the index dataframe
mapped_data <- merge(index_df, attributes, by = "tract_index")

# Join the mapped data with the retailer data
poster_data <- merge(pilot_ecomm, mapped_data, by.x = "row.names", by.y = "retailer_index")

# Identify point vs poly geoms
poster_data <- poster_data %>%
  rename(retailer_geometry = geometry.x,
         tract_geometry = geometry.y)

# Remove empty geometry column
poster_data$geometry <- NULL

# Convert to well-known text to preserve geometries in csv format 

poster_data_wkt <- poster_data %>%
  mutate(
    retailer_wkt = st_as_text(retailer_geometry),
    tract_wkt = st_as_text(tract_geometry)
  ) %>%
  select(-retailer_geometry, -tract_geometry)

write.csv(poster_data_wkt, "~/Documents/GitHub/CSS_Capstone_Project/pilot_ready.csv", row.names = FALSE)










