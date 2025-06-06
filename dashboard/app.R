# ---- Load Required Libraries ----
# Geographic and mapping libraries
library(tigris)         # Access to US Census geographic data
library(leaflet)        # Interactive web maps
library(sf)             # Spatial data processing
library(leaflet.extras) # Additional leaflet functionality

# Data manipulation and UI libraries
library(dplyr)          # Data manipulation
library(scales)         # Formatting functions (comma, etc.)
library(htmltools)      # HTML generation
library(readxl)         # Excel file reading
library(htmlwidgets)    # HTML widget framework
library(shiny)          # Web application framework

# Configure tigris library for better performance
options(tigris_use_cache = TRUE)  # Cache downloaded geographic data
options(tigris_class = "sf")      # Return spatial features as sf objects

# ---- Files ----
retailer_path <- "Finalized E-commerce Vape Shops After Filtering B&C.xlsx"
zipcode_path <- "normalized_SE_sf.shp"
unincorporated_path <- "CMTY_PLAN_CN"

# ---- Column Name Variables ----
# These store the exact column names from the Excel file
col_website_online <- "Do they have a website listed that allows you to buy vaping products online? (Allows for mail delivery, curbside pick up orders, etc.)"
col_confirms_vape <- "Can you confirm that this shop sells vaping products from the yelp or google maps urls or from its website?"

# ---- Helper Functions ----

# Function to create HTML popup content for map markers
# Input: Single row of retailer data
# Output: HTML string for popup display
create_popup_content <- function(retailer) {
  # Start with basic retailer name
  content <- paste0(
    "<div style='min-width: 200px;'>",
    "<b>Name:</b> ", retailer$name, "<br>"
  )
  
  # Add website link if available (check for non-NA values)
  if (!is.na(retailer$website)) {
    content <- paste0(content, "<b>Website:</b> <a href='", retailer$website, "' target='_blank'>", retailer$website, "</a><br>")
  }
  
  # Add Google Maps link if available
  if (!is.na(retailer$place_url)) {
    content <- paste0(content, "<b>Google Maps:</b> <a href='", retailer$place_url, "' target='_blank'>View on Google Maps</a><br>")
  }
  
  # Add Yelp link if available
  if (!is.na(retailer$yelp_url)) {
    content <- paste0(content, "<b>Yelp:</b> <a href='", retailer$yelp_url, "' target='_blank'>View on Yelp</a><br>")
  }
  
  # Add online sales information (1 = Yes, 0 = No)
  has_online_sales <- retailer[[col_website_online]] == 1
  content <- paste0(content, "<b>Online Sales:</b> ", ifelse(has_online_sales, "Yes", "No"), "<br></div>")
  return(content)
}

# ---- Load and Process Geographic Data ----

# Load San Diego County boundary from US Census
# This creates the county outline shown on the map
county_boundary <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  filter(NAME == "San Diego") %>%           # Filter to only San Diego County
  st_transform(crs = 4326)                  # Transform to WGS84 coordinate system (standard for web maps)

# Calculate map boundaries with padding for better display
sd_bounds <- st_bbox(county_boundary)       # Get bounding box coordinates
padding_factor <- 0.2                      # Add 20% padding around county

# Load ZIP code areas from shapefile
# This provides the geographic boundaries for ZIP code searches
zipcode_data <- st_read(zipcode_path) %>%
  filter(County == "San Diego") %>%         # Filter to San Diego County only
  st_transform(crs = 4326)                  # Ensure consistent coordinate system

# Create sorted list of ZIP codes for dropdown menu
sd_zip_codes <- sort(unique(zipcode_data$ZIP))

# Load unincorporated areas from shapefile
# These are areas not within city boundaries
unincorporated_areas <- st_read(unincorporated_path) %>%
  st_transform(crs = 4326) %>%              # Transform coordinate system
  st_make_valid()                           # Fix any geometric issues

# Create list of all searchable areas (cities + unincorporated option)
original_cities <- setdiff(sort(unique(zipcode_data$ApprxLc)), "Unincorporated San Diego County area")
all_areas <- c(original_cities, "All Unincorporated Areas")

# ---- Load and Process Retailer Data ----

# Read retailer data from Excel file and clean/standardize column names
retailer_data <- read_excel(retailer_path) %>%
  mutate(
    # Create standardized 'name' field from either Google Places or Yelp data
    name = coalesce(place_name, yelp_name),
    
    # Create standardized 'website' field, checking multiple possible columns
    website = coalesce(`place_website (hyperlinked)`, place_website, `yelp_website (hyperlinked)`, yelp_website),
    
    # Standardize URL fields
    place_url = coalesce(`place_url (hyperlinked)`, place_url),
    yelp_url = coalesce(`yelp_url (hyperlinked)`, yelp_url),
    
    # Ensure coordinates are numeric
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  ) %>%
  # Remove any records missing coordinates (can't be mapped)
  filter(!is.na(lon) & !is.na(lat))

# Filter to only confirmed vape retailers and add website flag
vape_retailers <- retailer_data %>%
  filter(!!sym(col_confirms_vape) == 1) %>%          # Only confirmed vape retailers
  mutate(has_website = !!sym(col_website_online) == 1) # Create boolean flag for website availability

# ---- Map Styling and Icons ----

# Define blue marker icon for all retailers
# Using external icon service for consistent appearance
blueIcon <- icons(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
  iconWidth = 20, iconHeight = 32,           # Icon dimensions
  iconAnchorX = 10, iconAnchorY = 32,        # Where icon "points" to coordinate
  popupAnchorX = 0, popupAnchorY = -30       # Where popup appears relative to icon
)

# Define clustering options for when markers are close together
# This improves performance and readability when zoomed out
blueClusterOptions <- markerClusterOptions(
  iconCreateFunction = JS("
    function(cluster) {
      return new L.DivIcon({
        html: '<div style=\"background-color: rgba(0,123,255,0.7); border-radius: 50%; width:40px; height:40px; display:flex; align-items:center; justify-content:center; color:white; font-weight:bold;\">' + cluster.getChildCount() + '</div>',
        className: 'marker-cluster'
      });
    }
  ")
)

# ---- User Interface (UI) Definition ----
ui <- fluidPage(
  # Custom CSS styling for the application
  tags$head(
    tags$style(HTML("
      /* Make map fill most of the screen height */
      .leaflet-container {
        height: calc(100vh - 160px) !important;
      }
      
      /* Fix dropdown menu z-index issues with map controls */
      .selectize-dropdown {
        z-index: 1001 !important;  /* Above map controls */
      }
      .selectize-input {
        z-index: 700 !important;
      }
      .selectize-dropdown-content {
        z-index: 1001 !important;
      }
      .leaflet-control-zoom {
        z-index: 999 !important;   /* Below dropdowns */
      }
      
      /* Styling for filter panel */
      .filter-panel {
        background-color: white;
        padding: 10px;
        border-radius: 4px;
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .filter-title {
        font-weight: bold;
        margin-bottom: 5px;
      }
      .website-filter {
        margin-top: 10px;
      }
      .toggle-icon {
        display: inline-block;
        width: 15px;
        font-size: 10px;
      }
      
      /* Download button styling */
      .download-controls {
        padding: 0;
      }
      .download-btn {
        background-color: #4CAF50;
        color: white;
        border: none;
        padding: 8px 16px;
        border-radius: 4px;
        cursor: pointer;
      }
      .download-btn:hover {
        background-color: #45a049;
      }
      
      /* Footer disclaimer styling */
      .disclaimer-footer {
        background-color: #f8f9fa;
        padding: 15px;
        text-align: center;
        border-top: 1px solid #dee2e6;
        margin-top: 20px;
        font-size: 14px;
        color: #6c757d;
        font-style: italic;
      }
    "))
  ),
  
  # JavaScript function for collapsible website filter
  tags$script("
    function toggleWebsiteOptions() {
      var options = document.getElementById('website-options');
      var icon = document.querySelector('.toggle-icon');
      
      if (options.style.display === 'none') {
        options.style.display = 'block';
        icon.innerHTML = '▼';
      } else {
        options.style.display = 'none';
        icon.innerHTML = '▶';
      }
    }
  "),
  
  # Top row: Search controls and download button
  fluidRow(
    # ZIP Code search dropdown
    column(4,
           selectizeInput("zip_search", "Search by ZIP Code:", 
                          choices = c("None" = "none", sd_zip_codes),  # Include "None" option plus all ZIP codes
                          selected = "none",                           # Default to no selection
                          options = list(
                            placeholder = 'Type ZIP code or select None',
                            onInitialize = I('function() { this.setValue("none"); }')  # Ensure "None" is selected on load
                          )
           )
    ),
    
    # City/Area search dropdown
    column(4,
           selectizeInput("city_search", "Search by City:", 
                          choices = c("None" = "none", all_areas),     # Include "None" option plus all cities/areas
                          selected = "none",
                          options = list(
                            placeholder = 'Select city/area or None',
                            onInitialize = I('function() { this.setValue("none"); }')
                          )
           )
    ),
    
    # Download button (with padding to make it narrower)
    column(4,
           div(class = "download-controls",
               div(style = "padding: 0 20px;",              # Horizontal padding to narrow the button
                   downloadButton("download_csv", "Download CSV", class = "download-btn", style = "width: 100%; margin-top: 25px;")
               )
           )
    )
  ),
  
  # Filter options panel (collapsible for website filter)
  fluidRow(
    column(12, 
           div(class = "filter-panel",
               div(class = "filter-title", "Filter Options:"),
               
               # Checkbox for showing all retailers (default state)
               checkboxInput("show_all_retailers", "All Retailers", value = TRUE),
               
               # Collapsible website filter section
               div(class = "website-filter",
                   # Clickable header that toggles visibility
                   div(
                     style = "cursor: pointer; margin-bottom: 5px;",
                     onClick = "toggleWebsiteOptions();",     # JavaScript function call
                     tags$span(class = "toggle-icon", "▶"),   # Arrow icon (changes direction when expanded)
                     tags$span("Does the retailer have a website?")
                   ),
                   
                   # Hidden by default radio button options
                   div(id = "website-options", style = "display: none; margin-left: 15px;",
                       radioButtons("website_filter", "",
                                    choices = list("Yes" = "yes", "No" = "no"),
                                    selected = character(0))               # No default selection
                   )
               )
           )
    )
  ),
  
  # Main map display
  leafletOutput("map"),
  
  # Footer with data disclaimer
  div(class = "disclaimer-footer",
      "Data were collected and cleaned in February 2023. Please check Yelp or Google Maps record to confirm retailer is in operation."
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  
  # Reactive values to store application state
  # This allows different parts of the app to share and update data
  mapState <- reactiveValues(
    selected_area = NULL,                    # Currently selected geographic area (ZIP/city)
    filtered_retailers = vape_retailers      # Currently filtered set of retailers
  )
  
  # ---- Create Base Map ----
  # This runs once when the app starts and creates the initial map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      preferCanvas = TRUE,                   # Use canvas rendering for better performance
      minZoom = 7,                          # Prevent zooming out too far
      maxZoom = 30,                         # Allow detailed zoom
      # Set map boundaries with padding to keep focus on San Diego County
      maxBounds = list(
        list(sd_bounds[2] - padding_factor, sd_bounds[1] - padding_factor),  # Southwest corner
        list(sd_bounds[4] + padding_factor, sd_bounds[3] + padding_factor)   # Northeast corner
      ),
      zoomControl = TRUE                     # Show default zoom buttons
    )) %>%
      # Add base map tiles (CartoDB provides clean, readable maps)
      addProviderTiles("CartoDB.Positron") %>%
      
      # Create map layers with different z-index values (controls what appears on top)
      addMapPane("retailers", zIndex = 420) %>%    # Retailers on top
      addMapPane("boundary", zIndex = 415) %>%     # County boundary in middle  
      addMapPane("zipcode", zIndex = 410) %>%      # ZIP/city areas on bottom
      
      # Add San Diego County boundary as gray lines
      addPolylines(
        data = county_boundary,
        color = "#888888",                   # Gray color
        weight = 1.5,                       # Line thickness
        opacity = 0.8,                      # Slightly transparent
        options = pathOptions(pane = "boundary")  # Place in boundary layer
      ) %>%
      
      # Set initial map view (centered on San Diego County)
      setView(lng = -117.05, lat = 32.95, zoom = 10.3)
  })
  
  # ---- Filter Logic Observers ----
  # These watch for changes in filter inputs and update other inputs accordingly
  
  # When "All Retailers" is checked, clear website filter
  observeEvent(input$show_all_retailers, {
    if (input$show_all_retailers) {
      updateRadioButtons(session, "website_filter", selected = character(0))
    }
  })
  
  # When website filter is selected, uncheck "All Retailers"
  observeEvent(input$website_filter, {
    if (!is.null(input$website_filter) && input$website_filter != "") {
      updateCheckboxInput(session, "show_all_retailers", value = FALSE)
    }
  })
  
  # ---- Update Map Markers Based on Filters ----
  # This observer runs whenever filter inputs change and updates the map markers
  observe({
    # Get current filter states
    show_all <- input$show_all_retailers
    website_filter <- input$website_filter
    
    # Apply filters to determine which retailers to show
    retailers_to_show <- if (show_all) {
      vape_retailers                         # Show all retailers
    } else if (!is.null(website_filter) && website_filter == "yes") {
      vape_retailers %>% filter(has_website == TRUE)    # Only retailers with websites
    } else if (!is.null(website_filter) && website_filter == "no") {
      vape_retailers %>% filter(has_website == FALSE)   # Only retailers without websites
    } else {
      vape_retailers %>% filter(1 == 0)     # Empty set if no valid filter selected
    }
    
    # Store filtered retailers for download function
    mapState$filtered_retailers <- retailers_to_show
    
    # Remove existing retailer markers from map
    leafletProxy("map") %>%
      clearGroup("retailers")
    
    # Add new markers if any retailers match the filter
    if (nrow(retailers_to_show) > 0) {
      # Create popup content for each retailer
      popups <- lapply(1:nrow(retailers_to_show), function(i) {
        create_popup_content(retailers_to_show[i,])
      })
      
      # Add markers to map with clustering
      leafletProxy("map") %>%
        addMarkers(
          data = retailers_to_show,
          ~lon, ~lat,                        # Longitude and latitude columns
          icon = blueIcon,                   # Use blue marker icon
          popup = popups,                    # Popup content for each marker
          group = "retailers",               # Group name for easy removal/update
          options = pathOptions(pane = "retailers"),  # Place in retailers layer
          popupOptions = popupOptions(offset = c(0, -30)),  # Position popup above marker
          clusterOptions = blueClusterOptions # Enable clustering when markers are close
        )
    }
  })
  
  # ---- ZIP Code Search Observer ----
  # Handles ZIP code selection and map navigation
  observeEvent(input$zip_search, {
    if (input$zip_search != "none" && input$zip_search != "") {
      # Clear city search when ZIP is selected (prevent conflicts)
      updateSelectizeInput(session, "city_search", selected = "none")
      
      # Find the selected ZIP code area
      selected_zip <- zipcode_data %>% filter(ZIP == input$zip_search)
      
      if (nrow(selected_zip) > 0) {
        # Calculate bounding box for map navigation
        selected_bbox <- st_bbox(selected_zip)
        
        # Store selected area information for download function
        mapState$selected_area <- list(
          type = "zip",
          id = input$zip_search,
          boundary = selected_zip,
          bbox = selected_bbox
        )
        
        # Update map: add highlighted area and zoom to it
        leafletProxy("map") %>%
          clearGroup("highlighted_area") %>%  # Remove any previous highlights
          addPolygons(
            data = selected_zip,
            fillColor = "yellow",            # Yellow fill for ZIP areas
            weight = 1.5,                   # Border thickness
            opacity = 0.8,                  # Border opacity
            color = "#FFA500",              # Orange border color
            fillOpacity = 0.2,              # Transparent fill
            group = "highlighted_area",      # Group for easy removal
            options = pathOptions(pane = "zipcode")  # Place in zipcode layer
          ) %>%
          # Smoothly fly to the selected area
          flyToBounds(
            selected_bbox[["xmin"]], selected_bbox[["ymin"]],  # Southwest corner
            selected_bbox[["xmax"]], selected_bbox[["ymax"]]   # Northeast corner
          )
      }
    } else {
      # "None" was selected: clear highlighting and reset view
      leafletProxy("map") %>% clearGroup("highlighted_area")
      mapState$selected_area <- NULL
      leafletProxy("map") %>% setView(lng = -117.05, lat = 32.95, zoom = 10.3)
    }
  })
  
  # ---- City/Area Search Observer ----
  # Handles city and unincorporated area selection
  observeEvent(input$city_search, {
    if (input$city_search != "none" && input$city_search != "") {
      # Clear ZIP search when city is selected (prevent conflicts)
      updateSelectizeInput(session, "zip_search", selected = "none")
      
      # Special handling for "All Unincorporated Areas" option
      if (input$city_search == "All Unincorporated Areas") {
        # Calculate bounding box that encompasses all unincorporated areas
        selected_bbox <- st_bbox(unincorporated_areas)
        
        # Store area information
        mapState$selected_area <- list(
          type = "unincorporated",
          id = "All Unincorporated Areas",
          boundary = unincorporated_areas,   # Keep all individual areas for filtering
          bbox = selected_bbox
        )
        
        # Add all unincorporated areas to map with green highlighting
        leafletProxy("map") %>%
          clearGroup("highlighted_area") %>%
          addPolygons(
            data = unincorporated_areas,
            fillColor = "lightgreen",        # Green fill for unincorporated areas
            weight = 1.5,
            opacity = 0.8,
            color = "#3CB371",              # Medium sea green border
            fillOpacity = 0.2,
            group = "highlighted_area",
            options = pathOptions(pane = "zipcode")
          ) %>%
          flyToBounds(
            selected_bbox[["xmin"]], selected_bbox[["ymin"]],
            selected_bbox[["xmax"]], selected_bbox[["ymax"]]
          )
        
      } else {
        # Regular city selection
        selected_city <- zipcode_data %>% filter(ApprxLc == input$city_search)
        
        if (nrow(selected_city) > 0) {
          selected_bbox <- st_bbox(selected_city)
          
          mapState$selected_area <- list(
            type = "city",
            id = input$city_search,
            boundary = selected_city,
            bbox = selected_bbox
          )
          
          # Add city area with blue highlighting
          leafletProxy("map") %>%
            clearGroup("highlighted_area") %>%
            addPolygons(
              data = selected_city,
              fillColor = "lightblue",       # Blue fill for cities
              weight = 1.5,
              opacity = 0.8,
              color = "#6495ED",            # Light blue border
              fillOpacity = 0.2,
              group = "highlighted_area",
              options = pathOptions(pane = "zipcode")
            ) %>%
            flyToBounds(
              selected_bbox[["xmin"]], selected_bbox[["ymin"]],
              selected_bbox[["xmax"]], selected_bbox[["ymax"]]
            )
        }
      }
    } else {
      # "None" was selected: clear highlighting and reset view
      leafletProxy("map") %>% clearGroup("highlighted_area")
      mapState$selected_area <- NULL
      leafletProxy("map") %>% setView(lng = -117.05, lat = 32.95, zoom = 10.3)
    }
  })
  
  # ---- Reset Highlighting Observer ----
  # Clear area highlighting when both searches are set to "None"
  observe({
    if (input$zip_search == "none" && input$city_search == "none") {
      leafletProxy("map") %>% clearGroup("highlighted_area")
      mapState$selected_area <- NULL
    }
  })
  
  # ---- Function to Get Retailers in Selected Area ----
  # This function applies both filter and geographic constraints
  get_retailers_in_selected_area <- function() {
    # Start with currently filtered retailers (based on website filter)
    retailers <- mapState$filtered_retailers
    
    # If a geographic area is selected, filter retailers within that area
    if (!is.null(mapState$selected_area)) {
      area_boundary <- mapState$selected_area$boundary
      
      # Convert retailer coordinates to spatial features for geographic analysis
      retailers_sf <- st_as_sf(retailers, coords = c("lon", "lat"), crs = 4326)
      
      # Special handling for unincorporated areas (multiple separate polygons)
      if (mapState$selected_area$type == "unincorporated" && 
          mapState$selected_area$id == "All Unincorporated Areas") {
        
        # Check each retailer against all unincorporated area polygons
        retailers_in_any_area <- rep(FALSE, nrow(retailers_sf))
        
        # Loop through each unincorporated area polygon
        for (i in 1:nrow(area_boundary)) {
          single_area <- area_boundary[i, ]
          # Check which retailers fall within this specific area
          intersects <- st_intersects(retailers_sf, st_union(single_area))
          # Update master list: TRUE if retailer is in ANY unincorporated area
          retailers_in_any_area <- retailers_in_any_area | lengths(intersects) > 0
        }
        
        retailers_in_area_indices <- which(retailers_in_any_area)
        
      } else {
        # Standard case: single area (ZIP code or city)
        # Find retailers that intersect with the combined area boundary
        retailers_in_area <- st_intersects(retailers_sf, st_union(area_boundary))
        retailers_in_area_indices <- which(lengths(retailers_in_area) > 0)
      }
      
      # Filter original retailer data to only include those in the selected area
      if (length(retailers_in_area_indices) > 0) {
        retailers <- retailers[retailers_in_area_indices, ]
      } else {
        retailers <- retailers[0, ]          # Return empty dataframe if no matches
      }
    }
    
    return(retailers)
  }
  
  # ---- CSV Download Handler ----
  # Generates and downloads CSV file based on current filters and area selection
  output$download_csv <- downloadHandler(
    # Dynamic filename based on current selections
    filename = function() {
      filename_parts <- c("SanDiego_VapeRetailers")
      
      # Add filter information to filename
      if (!input$show_all_retailers) {
        if (!is.null(input$website_filter) && input$website_filter == "yes") {
          filename_parts <- c(filename_parts, "with_websites")
        } else if (!is.null(input$website_filter) && input$website_filter == "no") {
          filename_parts <- c(filename_parts, "no_websites")
        }
      }
      
      # Add geographic area to filename if selected
      if (!is.null(mapState$selected_area)) {
        area_text <- paste0(
          mapState$selected_area$type, "_", 
          gsub(" ", "_", mapState$selected_area$id)  # Replace spaces with underscores
        )
        filename_parts <- c(filename_parts, area_text)
      }
      
      # Combine parts with underscores and add date and extension
      paste0(paste(filename_parts, collapse = "_"), "_", 
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    
    # Function that creates the actual CSV content
    content = function(file) {
      # Get retailers matching current filters and area selection
      retailers_in_area <- get_retailers_in_selected_area()
      
      if (nrow(retailers_in_area) > 0) {
        # Create clean dataframe for CSV export
        csv_data <- data.frame(
          Latitude = retailers_in_area$lat,
          Longitude = retailers_in_area$lon,
          Name = retailers_in_area$name,
          # Clean website field: replace NA, empty, or "more" with empty string
          Website = ifelse(is.na(retailers_in_area$website) | retailers_in_area$website == "" | retailers_in_area$website == "more", "", retailers_in_area$website),
          # Clean URL fields: replace NA or empty with empty string
          Google_Maps_URL = ifelse(is.na(retailers_in_area$place_url) | retailers_in_area$place_url == "", "", retailers_in_area$place_url),
          Yelp_URL = ifelse(is.na(retailers_in_area$yelp_url) | retailers_in_area$yelp_url == "", "", retailers_in_area$yelp_url),
          # Convert 1/0 to Yes/No for online sales
          Has_Online_Sales = ifelse(retailers_in_area[[col_website_online]] == 1, "Yes", "No"),
          stringsAsFactors = FALSE           # Prevent automatic factor conversion
        )
      } else {
        # Create empty dataframe with same structure if no retailers found
        csv_data <- data.frame(
          Latitude = numeric(0),
          Longitude = numeric(0),
          Name = character(0),
          Website = character(0),
          Google_Maps_URL = character(0),
          Yelp_URL = character(0),
          Has_Online_Sales = character(0),
          stringsAsFactors = FALSE
        )
      }
      
      # Write dataframe to CSV file
      write.csv(csv_data, file, row.names = FALSE)
    },
    contentType = "text/csv"                 # Ensure browser recognizes as CSV
  )
}

# ---- Launch the Application ----
# This line starts the Shiny web server and opens the app
shinyApp(ui, server)