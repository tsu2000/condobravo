library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(gtools)
library(lubridate)
library(mapsapi)
library(plotly)
library(sf)

# Read in necessary data from CSV files
condos <- read.csv("data/unique_projects_final.csv")
transactions <- read.csv("data/properties.csv")
district_locs <- read.csv("data/district_names.csv")

### 1. DATA PREPROCESSING FOR INTRODUCTION

# Read in shapefile for polygons relating to URA postal districts
districts <- st_read("data/district_polygons/district_polygons.shp", crs = 4326) 

# Calculate the centroid of the multipolygon (For text labels on map)
centroids <- st_centroid(districts$geometry)
centroid_df <- data.frame(label = 1:28)

# Extract the longitude and latitude coordinates of the centroids
centroid_df$lon <- st_coordinates(centroids)[, "X"]
centroid_df$lat <- st_coordinates(centroids)[, "Y"]
centroid_df <- st_as_sf(centroid_df, coords = c("lon", "lat"))

# Convert monthly rent to numeric column
transactions$monthly.rent <- as.numeric(gsub(",", "", transactions$"Monthly.Rent...."))

# Convert bedroom column to character
transactions <- transactions %>%
  mutate(No.of.Bedroom = case_when(
    is.na(No.of.Bedroom) | No.of.Bedroom == 0 ~ "Other",
    TRUE ~ paste0(No.of.Bedroom, "-Room")
  ))

# Create summary of transactions
tr_summary <- transactions %>% 
  group_by(Postal.District) %>% 
  summarise(
    count = n(),
    avg.monthly.rent = mean(monthly.rent)
  ) %>% 
  right_join(district_locs, by = c("Postal.District" = "district_num")) %>% 
  arrange(Postal.District)

# Set colors for districts based on region type
cr_colors <- c("CCR" = "#79d9a3", "RCR" = "#d9899d", "OCR" = "#a9b8f5")
pal1 <- unname(cr_colors[districts$region])
pal2 <- colorNumeric(palette = "Blues", domain = tr_summary$count)
pal3 <- colorNumeric(palette = "YlOrRd", domain = tr_summary$avg.monthly.rent)

# Add data for popup
popup <- paste0("<strong>District ", tr_summary$Postal.District, "</strong>: ", tr_summary$general_loc, "<br>",
                "<strong>No. of transactions: </strong>", tr_summary$count, "<br>",
                "<strong>Avg. monthly rent: </strong>", round(tr_summary$avg.monthly.rent, 2))

# Data transformation for data visualisations

# Parse the lease commencement date into a date format
transactions$lease_commencement_date <- dmy(paste0("01-", transactions$Lease.Commencement.Date))

CCR_districts <- c(1:2, 6, 9:11)
RCR_districts <- c(3:5, 7:8, 12:15, 20)
OCR_districts <- c(16:19, 21:28)

transactions <- transactions %>%
  mutate(
    region = case_when(
      Postal.District %in% CCR_districts ~ "Core Central Region (CCR)",
      Postal.District %in% RCR_districts ~ "Rest of Central Region (RCR)",
      Postal.District %in% OCR_districts ~ "Outside Central Region (OCR)",
      TRUE ~ "Unknown"  # If none of the conditions are met
    )
  )

# Calculate the interquartile range for Monthly Rent
iqr_rent <- IQR(transactions$monthly.rent, na.rm = TRUE)

# Calculate the upper whisker: third quartile + 1.5*IQR (common practice for box plots)
upper_limit <- quantile(transactions$monthly.rent, 0.75, na.rm = TRUE) + 1.5 * iqr_rent

# Get a sequence of the first days of the months from the range of your data
date_breaks <- seq(from = floor_date(min(transactions$lease_commencement_date), "month"),
                   to = floor_date(max(transactions$lease_commencement_date), "month"),
                   by = "1 month")

### 2. DATA PREPROCESSING FOR MAP DETAILS

# Overview part

# Read in data for nearby facilities
mrt_data <- read.csv("data/nearby_facilities/mrt_stations.csv")
hospitals_data <- read.csv("data/nearby_facilities/hospitals.csv")
bus_stop_data <- read.csv("data/nearby_facilities/bus_stops.csv")
mall_data <- read.csv("data/nearby_facilities/shopping_mall_coordinates.csv")

# Read the API key from the text file
api_key <- trimws(readLines("secrets/api_key.txt"))

# Get all details for all condos
condos_all_details <- transactions %>% 
  group_by(Project.Name) %>% 
  summarise(dist.no = mean(Postal.District)) %>% 
  right_join(condos, by = "Project.Name") %>% 
  select(-X) %>% 
  left_join(district_locs, by = c("dist.no" = "district_num")) %>% 
  left_join(select(bus_stop_data, c("BusStopCode", "Description")), by = c("nearest_busstop" = "BusStopCode")) %>%
  drop_na(general_loc) %>% 
  mutate(dist.full = paste0("District ", sprintf("%02d", dist.no), " - ", general_loc))

# Create function to get nearby facilities and routes
get_routes <- function(project_name) {
  project <- condos[condos$Project.Name == project_name, ]
  
  if (nrow(project) == 0) {
    return("Project not found")
  }
  
  nearest_mrt_name <- project$nearest_mrt
  nearest_hospital_name <- project$nearest_hospital 
  nearest_mall_name <- project$nearest_mall  
  nearest_bus_stop_name <- project$nearest_busstop 
  
  if (is.na(nearest_mrt_name) || nearest_mrt_name == "") {
    return("Nearest MRT not specified for the project")
  }
  
  if (is.na(nearest_hospital_name) || nearest_hospital_name == "") {
    return("Nearest hospital not specified for the project")
  }
  
  if (is.na(nearest_mall_name) || nearest_mall_name == "") {
    return("Nearest mall not specified for the project")
  }
  
  if (is.na(nearest_bus_stop_name) || nearest_bus_stop_name == "") {
    return("Nearest bus stop not specified for the project")
  }
  
  # Get coordinates of the nearby facilities
  mrt <- mrt_data[mrt_data$STN_NAME == nearest_mrt_name, ]
  hospital <- hospitals_data[hospitals_data$Name == nearest_hospital_name, ]  
  mall <- mall_data[mall_data$Mall.Name == nearest_mall_name, ]  
  bus_stop <- bus_stop_data[bus_stop_data$BusStopCode == nearest_bus_stop_name, ]
  
  if (nrow(mrt) == 0) {
    return("Nearest MRT not found")
  }
  
  if (nrow(hospital) == 0) {
    return("Nearest hospital not found")
  }
  
  if (nrow(mall) == 0) {
    return("Nearest mall not found")
  }
  
  if (nrow(bus_stop) == 0) {
    return("Nearest bus stop not found")
  }
  
  doc_mrt <- mp_directions(
    origin = paste(project$Latitude, project$Longitude, sep = ","),
    destination = paste(mrt$Latitude, mrt$Longitude, sep = ","),
    mode = "walking",
    key = api_key,
    quiet = TRUE
  )
  
  doc_hospital <- mp_directions(
    origin = paste(project$Latitude, project$Longitude, sep = ","),
    destination = paste(hospital$Y, hospital$X, sep = ","),
    mode = "walking",
    key = api_key,
    quiet = TRUE
  )
  
  doc_mall <- mp_directions(
    origin = paste(project$Latitude, project$Longitude, sep = ","),
    destination = paste(mall$LATITUDE, mall$LONGITUDE, sep = ","),
    mode = "walking",
    key = api_key,
    quiet = TRUE
  )
  
  doc_bus_stop <- mp_directions(
    origin = paste(project$Latitude, project$Longitude, sep = ","),
    destination = paste(bus_stop$Latitude, bus_stop$Longitude, sep = ","),
    mode = "walking",
    key = api_key,
    quiet = TRUE
  )
  
  # Check if directions are retrieved successfully
  if (is.null(doc_mrt) || is.null(doc_hospital) || is.null(doc_mall) || is.null(doc_bus_stop)) {
    return("Error retrieving directions")
  }
  
  routes_mrt <- mp_get_routes(doc_mrt)
  routes_hospital <- mp_get_routes(doc_hospital)
  routes_mall <- mp_get_routes(doc_mall)
  routes_bus_stop <- mp_get_routes(doc_bus_stop)
  
  # Check if routes are available
  if (is.null(routes_mrt) || is.null(routes_hospital) || is.null(routes_mall) || is.null(routes_bus_stop)) {
    return("Error retrieving route information")
  }
  
  # Return list of simple feature collections for each route
  return(list("mrt_route" = routes_mrt, "hospital_route" = routes_hospital, "mall_route" = routes_mall, "bus_stop_route" = routes_bus_stop))
}

# Define the UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
    .navbar-brand {
        pointer-events: none;
    }
    
    .info.legend.leaflet-control > div:nth-child(3) > svg {
      margin-right: 12px;
    }
    
    .bs-select-all.hidden, .bs-deselect-all.hidden {
      display: none !important;
    }
    
    .bs-select-all {
      display: none;
    }

    .bs-deselect-all {
      width: 100% !important;
    }
    
    #dynamic_graph {
      max-width: 100%;
      margin: auto;
    }
    
    ")),
    tags$link(rel = "shortcut icon", 
              type = "image/png",
              href = "https://raw.githubusercontent.com/tsu2000/condobravo/main/www/condobravo.png")
  ),
  navbarPage(
    title = div(img(src = "condobravo.png", style = "height: 1.2em;"), "CondoBravo"),
    tabPanel("Introduction",
             h1("👋 Welcome to Singapore! 🏝️"),
             p("Finding a new home in a foreign country can be daunting but CondoBravo is here to help! The app contains transaction data for non-landed private properties in Singapore from January 2022 to September 2023. The Urban Redevelopment Authority (URA) is in charge of all non-government housing in Singapore, and they have divided Singapore into 28 districts for urban planning and development. Take a look at the map below to see where the districts are."),
             HTML("<b>Follow the steps below to get started:</b>"),
             HTML("<ol>
                    <li>On this page, you are able to get an overview of the 28 districts we have in Singapore and some detailed visualisations showing differences between different districts.</li>
                    <li>After shortlisting some districts that are of interest to you, head to Property Viewer.</li>
                    <li>Under Property Viewer, you can filter by district to browse different properties, or simply search for the name of a specific property.</li>
                    <li>Data related to property price and area will be shown, along with a detailed map of all nearby facilities. Click the `Back` button to return to browsing properties.</li>
                  </ol>"),
             tabsetPanel(
               tabPanel("District Overview",
                        icon = "🗺️",
                        sidebarLayout(
                          sidebarPanel(
                            h2("District Map and Overview of Singapore"),
                            br(),
                            selectInput("color_polygons", 
                                        "Choose a color scheme to represent the districts:", 
                                        choices = c("By segmented region", "By number of transactions", "By average monthly rent")),
                            radioButtons(inputId = "district_map_type", 
                                         label = "Choose type of underlying map:",
                                         choices = c("Default", "Esri", "CartoDB"),
                                         inline = TRUE),
                            hr(style = "border-top: 1px solid #000000;"),
                            h4("Which district contains which neighbourhoods?"),
                            HTML(paste0("<strong>District ", tr_summary$Postal.District, "</strong>: ", tr_summary$general_loc, "<br>"))
                          ),
                          mainPanel(
                            leafletOutput("polygons"),
                          )
                        )
               ), 
               tabPanel(
                 "Data Visualisation",
                 icon = "📊",
                 fluidRow(
                   column(width = 2),
                   column(width = 8,
                          br(),
                          h2("Data Visualisation for Non- Landed Private Properties in Singapore (January 2022 - September 2023)"),
                          p("For those interested, this section aims to provide a more comprehensive overview of non-landed private properties in Singapore through more complex data visualisations and in-depth explanations. Some basic statistical knowledge is required to understand these plots."),
                          br(),
                          h4("Distribution of Total No. of Transactions by Postal District"),
                          plotOutput("data_viz1"),
                          markdown("Generally, the distribution of property transactions in Singapore are not uniform across postal districts. Some districts have a much higher number of non-landed private properties available for rental than others, indicating variability in density, development, or availability of properties within these districts. Under the assumption that past transactions are indicative of the supply of non-landed private properties for rental in each district, it is important to consider this in your search for a rental home.

                                   **Highest Number of Transactions**: A high concentration of past rental transactions are in Districts 9, 10 and 15. This could suggest that by focusing your search of available rental properties in these districts, you may have a better probability of finding one that suits your needs."),
                          br(),
                          h4("Average Monthly Rent by Postal District"),
                          plotOutput("data_viz2"),
                          markdown("District 1, 4, 9 and 10 have the highest average monthly rental prices across all districts. District 1, 9 and 10’s high rental prices can mostly be attributed to the district’s geographical attributes, its close proximity to Singapore’s main prime metropolitan area and the Central Business District (CBD).
                                   
                                   These three locations are thus considered under the Core Central Region (CCR). While pricey, properties in these districts give you quick access to work opportunities in the CBD and are thus a crucial factor in deciding which property you would like to rent.
                                   
                                   District 4 (Sentosa and HarbourFront) commands a high price due to its exclusive location, being a renowned island resort, known for its luxury hotels, high-end residential areas, and attractions like Universal Studios Singapore. Properties in Sentosa often come with high-end amenities and services, such as private marinas, golf courses, and exclusive clubs. Its status as a premium lifestyle and tourist destination contributes to the high property values. 
                                   
                                   Generally, we can see that the average rental prices go down as the properties are further away from the CBD."),
                          br(),
                          h4("Range of Monthly Rent by Postal District"),
                          plotOutput("data_viz3"),
                          markdown("Based on the box and whisker plots, District 1, 4, 9 and 10 do not just command the highest median monthly rental prices, they also have the highest variability in prices, with its Interquartile Range (represented by the size of the box) being significantly larger than other districts. These districts also have plenty of outliers, which have been removed for easier viewing.
                                   
                                   An interesting observation is that District 4 has the highest median price of properties in Singapore, due to its exclusive location as previously mentioned, despite not being considered part of the Central Core Region. However, the median price of all the other RCR regions like District 4 tend to be lower than the median prices in the CCR regions, with OCR regions having the consistently lowest median prices. This will be further elaborated on the next plot."),
                          br(),
                          h4("Density Plot of Monthly Rent by Region (OCR, RCR, CCR)"),
                          plotOutput("data_viz4"),
                          markdown("By using a density plot for grouping rental prices by region, we can observe a few attributes regarding the rental prices of non-landed private property:
                                   
                                   **Density**: The y-axis shows the density, which is a measure of probability. So, for a given rental price on the x-axis, the corresponding value on the y-axis indicates how densely packed properties are around that price. Higher density means more properties at that price level.
                                   
                                   **Peaks**: Each region has a peak, which is the point where the curve is highest. This peak represents the most common rental price range for that region. The CCR has its peak at a higher rental price (\\~$4,000) than the other two regions (\\~$3,000), indicating that the most common rental prices are higher in the CCR.
                                   
                                   **Tail**: The tail of each curve extends towards the higher rental prices, with the CCR’s tail stretching the furthest. This indicates that the CCR has more high-priced rental properties compared to the other regions.
                                   
                                   **Skewness**: The curves are skewed to the right, meaning that there are fewer properties at higher rental prices but they still significantly impact the distribution.
                                   
                                   **Overlap**: There’s some overlap between the regions, especially between the OCR and RCR. This indicates that there is a range of rental prices where properties in both these regions compete.
                                   
                                   From this plot, we can infer that the Core Central Region is generally the most expensive area, with most of its rental prices concentrated at a higher range than the other two regions. The Outside Central Region has a wider range of rental prices, indicating a more varied market, and the Rest of Central Region falls somewhere in between the two."),
                          br(),
                          h4("Average Monthly Rent across time"),
                          plotOutput("data_viz5"),
                          markdown("**Trend**: There is a clear upward trend in average rent prices over the period shown. This suggests that the rental market for non-landed private residences has been experiencing growth in terms of price.
                                   
                                   **Stability**: Towards the end of the graph, from around June 2023 to September 2023, the increase in rent prices seems to stabilize somewhat, with less variation between the months compared to the start of the year.
                                   
                                   While price is a consideration, given the relative upward trend Singapore’s property market has been in, it is expected that the trend will continue and rental prices will continue to increase. It is thus important that when choosing a property, to align your needs, time horizon and affordability for rent especially at the time of undertaking the rental contract (e.g. ability to lock in rent prices).")
                   ),
                   column(width = 2)
                 )
               )
             )
    ),
    tabPanel("Property Viewer",
             h3("Property Viewer"),
             sidebarLayout(
               sidebarPanel(
                 h4("Browse unique properties available across multiple districts (maximum 5 districts at once) and click on a property marker to view details for the particular property:"),
                 pickerInput(
                   inputId = "selected_districts",
                   label = "Filter by Districts:",
                   choices = sort(unique(condos_all_details$dist.full)),
                   selected = sort(unique(condos_all_details$dist.full))[1],
                   options = list(`actions-box` = TRUE, "max-options" = 5),
                   multiple = TRUE
                 ),
                 h4("Or, select the name of property to view in detail:"),
                 selectizeInput("condo_selection", "Select a property:",
                                choices = condos_all_details$Project.Name,
                                multiple = TRUE,
                                options = list(placeholder = "e.g. THE INTERLACE",
                                             onInitialize = I('function() { this.setValue(""); }'),
                                             maxItems = 1)
                 ),
                 actionButton("back_button", "Back", disabled = TRUE)
               ),
               mainPanel(
                 leafletOutput("property_map"),
                 # Conditionally show uiOutput and graph
                 br(),
                 conditionalPanel(
                   condition = "input.property_map_marker_click || input.condo_selection",
                   htmlOutput("dynamic_ui"),
                   br(),
                   htmlOutput("dynamic_graph_title"), # Temporary fix for responsive graph title
                   plotlyOutput("dynamic_graph")
                 )
               )
             )
    ),
    tabPanel("FAQ",
             h2("🤔 Frequently Asked Questions"),
             hr(style = "border-top: 1px solid #000000;"),
             markdown(
               "### What is this app about?
This Shiny app helps you explore details of past condominium (and other non-landed private property) rental transactions in Singapore (from January 2022 to September 2023).

### I can't seem to click on anything else in the Property Viewer!
Click on the Back button to return to browsing properties. Clicking on either a property marker on the map OR selecting an input in the property selector will shift the focus to be on the individual property itself.

### Are the condominiums listed here currently available for rent?
All condominiums featured here are subject to availability due to the dynamic nature of daily rental transactions. The condominiums listed for rent here are all based on transactions in the past during the stated time period, and will not be updated to reflect real-time availability of condominium rentals in Singapore.

### Where are the transaction and property data sourced from?
All property data used in the Shiny app was directly taken from the official website of the Urban Redevelopment Authority (URA) of Singapore, found <a href='https://www.ura.gov.sg/property-market-information/pmiResidentialTransactionSearch' target='_blank'>here</a>. Other data sources for other facilities in Singapore were taken from a variety of sources, including public APIs, web scraping and websites such as Kaggle.

### This application is pretty cool! Where can I find the source code?
The source code for this Shiny app is available on <a href='https://github.com/tsu2000/condobravo' target='_blank'>GitHub</a>. You may fork the GitHub repository and use it for your own personal project.

               ")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Render logo
  output$logo <- renderUI({
    tags$img(src = "data/condobravo.png", 
             alt = "CondoBravo Logo",
             style = "display: inline-block; vertical-align: middle; height: 30px; margin-right: 10px;"
    )
  })
  
  # 1. ALL RENDERS FOR INTRODUCTION
  output$polygons <- renderLeaflet({
    
    # Update color palette based on user input
    district_colors <- switch(
      input$color_polygons,
      "By segmented region" = pal1,
      "By number of transactions" = ~pal2(tr_summary$count),
      "By average monthly rent" = ~pal3(tr_summary$avg.monthly.rent)
    )
    
    # Plot district leaflet
    m <- leaflet() %>% 
      addTiles() %>%
      addPolygons(data = districts, 
                  weight = 2, 
                  stroke = TRUE, 
                  smoothFactor = 0.01, 
                  fillOpacity = 0.8, 
                  fillColor = district_colors,
                  color = "black", # Border color
                  popup = popup,
                  group = "Polygons") %>% 
      addLabelOnlyMarkers(
        data = centroid_df,
        label = ~centroid_df$label,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'center',
          textOnly = TRUE,
          style = list(fontWeight = "bold", fontSize = "16px", fontFamily = "Arial")
        )
      ) %>% 
      setView(lng = 103.8196, lat = 1.3521, zoom = 11)
    
      # Add or remove the legend based on user input
      if (input$color_polygons == "By segmented region") {
        m <- m %>% addLegend("bottomright",
                             colors = cr_colors,
                             labels = c("CCR - Core Central Region", "RCR - Rest of Central Region", "OCR - Outside Central Region"),
                             title = "Singapore District Map Distinction"
                             )
      } else if (input$color_polygons == "By number of transactions") {
        m <- m %>% addLegend("bottomright",
                             pal = pal2,
                             values = tr_summary$count,
                             title = "Total No. of Transactions"
                             )
      } else {
        m <- m %>% addLegend("bottomright",
                             pal = pal3,
                             values = tr_summary$avg.monthly.rent,
                             title = "Average Monthly Rent",
                             labFormat = labelFormat(prefix = "$")
                             )
      }
    
      # Change underlying graph
      if (input$district_map_type == "Default") {
        m <- m %>% addTiles("Default")
      } else if (input$district_map_type == "Esri") {
        m <- m %>% addProviderTiles("Esri.WorldImagery")
      } else {
        m <- m %>% addProviderTiles("CartoDB.Positron")
      }
    
      return(m)
  })
  
  # In-depth data visualisation start
  output$data_viz1 <- renderPlot({
    ggplot(transactions, aes(x = factor(Postal.District))) +
      geom_bar(fill = "steelblue") +
      labs(x = "Postal District", y = "Number of Property Transactions", title = "Distribution of Total Property Transactions by Postal District") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0))
  })
  
  output$data_viz2 <- renderPlot({
    # Group the data by 'Postal.District' and calculate the average monthly rent for each district
    transactions %>%
      group_by(Postal.District) %>%
      summarise(Average_Monthly_Rent = mean(monthly.rent, na.rm = TRUE)) %>%
      arrange(Postal.District) %>% 
      ggplot(aes(x = as.factor(Postal.District), y = Average_Monthly_Rent)) +
      geom_bar(stat = "identity", fill = "darkcyan") +
      labs(x = "Postal District", y = "Average Monthly Rent (SGD)", title = "Average Monthly Rent by Postal District") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0))
  })
  
  output$data_viz3 <- renderPlot({
    transactions %>%
      ggplot(aes(x = as.factor(Postal.District), y = monthly.rent, fill = factor(region))) +
      geom_boxplot(outlier.shape = NA) +  # Exclude outliers
      labs(x = "Postal District", y = "Monthly Rent (SGD)", title = "Distribution of Monthly Rent by Postal District", fill = "Region Type") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0)) +
      coord_cartesian(ylim = c(NA, 17500))
  })
  
  output$data_viz4 <- renderPlot({
    transactions %>% 
      ggplot(aes(x = monthly.rent, fill = region)) +
      geom_density(alpha = 0.25, trim = TRUE) +  # trim argument to limit the density within the range of the data
      theme_bw() +
      labs(title = "Density of Monthly Rental Prices by Region",
           x = "Monthly Rent",
           y = "Density",
           fill = "Region Type") +
      scale_x_continuous(labels = scales::dollar, breaks = seq(0, 10000, 1000),  limits = c(NA, upper_limit)) # Limit the x-axis
  })
  
  output$data_viz5 <- renderPlot({
    transactions %>%
      group_by(lease_commencement_date) %>%
      summarize(Average.Rent = mean(monthly.rent, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(lease_commencement_date, Average.Rent)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      theme_bw() +
      labs(title = "Average monthly rentals across time period",
           x = "Date",
           y = "Average Rent (SGD)") +
      scale_x_date(breaks = date_breaks, date_labels = "%b %y") +
      theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "none")
  })
  
  # 2. ALL RENDERS FOR MAP OVERVIEW
  filtered_condos <- reactive({
    # Get condo selection
    chosen_districts <- input$selected_districts
    
    # Check if any district is selected
    if (length(chosen_districts) == 0) {
      return(NULL)  # Return NULL if no districts are selected
    } else {   
      # Filter by district
      filtered_data <- subset(condos_all_details, dist.full %in% chosen_districts)
      
      # Return the filtered data
      return(filtered_data)
    }
  })
  
  # Create icons
  mrt_icon <- makeIcon(iconUrl = "data/icons/mrt-icon.png", iconWidth = 30, iconHeight = 30)
  hospital_icon <- makeIcon(iconUrl = "data/icons/hospital-icon.png", iconWidth = 30, iconHeight = 30)
  shopping_mall_icon <- makeIcon(iconUrl = "data/icons/shopping-mall-icon.png", iconWidth = 30, iconHeight = 30)
  bus_stop_icon <- makeIcon(iconUrl = "data/icons/bus-stop-icon.png", iconWidth = 30, iconHeight = 30)
  
  # Create a map with desired elements
  output$property_map <- renderLeaflet({
    
    # Create base map
    base_map <- leaflet() %>% addTiles() %>% addProviderTiles("CartoDB.Positron")
    
    if (is.null(filtered_condos())) {
      base_map %>%
        addLabelOnlyMarkers(lng = 103.8196, lat = 1.3521, label = "Select districts to view available properties!",
                labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                            style = list("color" = "red", "font-size" = "18px", "font-weight" = "bold", "font-family" = "Arial"))) %>% 
        fitBounds(lng1 = 103.6, lat1 = 1.15, lng2 = 104.05, lat2 = 1.5)
      
    } else if (length(filtered_condos()) >= 1) {
      base_map %>%
        addMarkers(
          data = filtered_condos(),
          lng = ~Longitude,
          lat = ~Latitude,
          label = ~Project.Name
          )
    }
    
  })
  
  # Function to handle marker click or selectInput selection
  plotNearbyFacilities <- function(all_routes, data) {

    # Get coordinates for each of the nearby facilities
    mrt <- mrt_data[mrt_data$STN_NAME == data$nearest_mrt, ]
    hospital <- hospitals_data[hospitals_data$Name == data$nearest_hospital, ]
    mall <- mall_data[mall_data$Mall.Name == data$nearest_mall, ]
    bus_stop <- bus_stop_data[bus_stop_data$BusStopCode == data$nearest_busstop, ] 
    
    # Apply custom label settings for all labels
    label_settings <- labelOptions(noHide = TRUE, direction = "auto", labelAnchor = c(0.5, 0))
    
    # Remove all other markers except one that was clicked
    leafletProxy("property_map") %>%
      clearMarkers() %>%
      addMarkers(
        data = data,
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~Project.Name
      ) %>% 
      addPolylines(data = all_routes[["mrt_route"]], 
                   opacity = 1, 
                   weight = 3, 
                   color = "blue", 
                   label = ~distance_text,
                   labelOptions = label_settings) %>%
      addPolylines(data = all_routes[["hospital_route"]], 
                   opacity = 1, 
                   weight = 3, 
                   color = "red", 
                   label = ~distance_text,
                   labelOptions = label_settings) %>%
      addPolylines(data = all_routes[["mall_route"]], 
                   opacity = 1, 
                   weight = 3, 
                   color = "green", 
                   label = ~distance_text,
                   labelOptions = label_settings) %>%
      addPolylines(data = all_routes[["bus_stop_route"]], 
                   opacity = 1, 
                   weight = 3, 
                   color = "purple", 
                   label = ~distance_text,
                   labelOptions = label_settings) %>%
      addCircleMarkers(data = data,
                       lng = ~Longitude,
                       lat = ~Latitude,
                       radius = 8, 
                       fillOpacity = 1, 
                       color = "darkorange") %>% 
      addMarkers(lat = mrt$Latitude, 
                 lng = mrt$Longitude, 
                 icon = mrt_icon, 
                 label = mrt$STN_NAME) %>% 
      addMarkers(lat = hospital$Y, 
                 lng = hospital$X,
                 icon = hospital_icon, 
                 label = hospital$Name) %>% 
      addMarkers(lat = mall$LATITUDE, 
                 lng = mall$LONGITUDE, 
                 icon = shopping_mall_icon, 
                 label = mall$Mall.Name) %>% 
      addMarkers(lat = bus_stop$Latitude, 
                 lng = bus_stop$Longitude, 
                 icon = bus_stop_icon, 
                 label = bus_stop$Description) %>% 
      fitBounds(lng1 = min(mrt$Longitude, hospital$X, mall$LONGITUDE, bus_stop$Longitude),
                lat1 = min(mrt$Latitude, hospital$Y, mall$LATITUDE, bus_stop$Latitude),
                lng2 = max(mrt$Longitude, hospital$X, mall$LONGITUDE, bus_stop$Longitude),
                lat2 = max(mrt$Latitude, hospital$Y, mall$LATITUDE, bus_stop$Latitude))
    
    # Enable back button
    shinyjs::enable("back_button")
    # Hide the select/deselect all button on pickerInput
    shinyjs::runjs('$(".bs-select-all").addClass("hidden"); $(".bs-deselect-all").addClass("hidden");')
    # Disable UI selections
    shinyjs::disable("selected_districts")
    shinyjs::disable("condo_selection")
    # Show the dynamic UI and graphs
    shinyjs::show("dynamic_ui")
    shinyjs::show("dynamic_graph_title")
    shinyjs::show("dynamic_graph")
  }
  
  # Create a reactive value to track the current property details and route details
  selectedProperty <- reactiveVal(NULL)
  facilityRoutes <- reactiveVal(NULL)
  is_browsing <- reactiveVal(TRUE)
  
  # 1. Define what happens after a particular property marker is clicked
  observeEvent(input$property_map_marker_click, {
    # Check if the Back button is disabled
    if (is_browsing()) {
      # Get coordinates of clicked marker
      click_lat <- input$property_map_marker_click$lat
      click_lng <- input$property_map_marker_click$lng
      
      # Get the clicked marker's data
      clicked_marker_data <- filtered_condos() %>%
        filter(near(Longitude, click_lng, tol = 0.000001) & near(Latitude, click_lat, tol = 0.000001))
      
      # print(clicked_marker_data) # This is for debugging to check if data is filtered correctly
    
      # Obtain all routes for project
      all_routes <- get_routes(clicked_marker_data$Project.Name)
      
      # Plot the nearby facilities
      plotNearbyFacilities(all_routes, clicked_marker_data)
      
      # Update the selected property and route details
      selectedProperty(clicked_marker_data)
      facilityRoutes(all_routes)
      is_browsing(FALSE)
    }
  }) 
  
  # 2. Define what happens after a particular property is selected from condo_selection
  observeEvent(input$condo_selection, {
    
    # Get all data for the particular property in selectInput
    property_data <- condos_all_details %>%
      filter(Project.Name == input$condo_selection)
    
    # Obtain all routes for project
    all_routes <- get_routes(property_data$Project.Name)
    
    # Plot the nearby facilities
    plotNearbyFacilities(all_routes, property_data)

    # Update the selected property and route details
    selectedProperty(property_data)
    facilityRoutes(all_routes)
    is_browsing(FALSE)
  })
  
  # 3. Define what to happen after back button is clicked
  observeEvent(input$back_button, {
    # Return to the original map
    leafletProxy("property_map") %>% 
      clearMarkers() %>%
      clearShapes()
      
    if (is.null(filtered_condos())) {
      leafletProxy("property_map") %>% 
        addLabelOnlyMarkers(lng = 103.8196, lat = 1.3521, label = "Select districts to view available properties!",
                            labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                      style = list("color" = "red", "font-size" = "18px", "font-weight" = "bold", "font-family" = "Arial"))) %>% 
        fitBounds(lng1 = 103.6, lat1 = 1.15, lng2 = 104.05, lat2 = 1.5) 
    } else {
      leafletProxy("property_map") %>% 
        addMarkers(
          data = filtered_condos(),
          lng = ~Longitude,
          lat = ~Latitude,
          label = ~Project.Name
        ) %>% 
        fitBounds(lng1 = min(filtered_condos()$Longitude), 
                  lat1 = min(filtered_condos()$Latitude), 
                  lng2 = max(filtered_condos()$Longitude), 
                  lat2 = max(filtered_condos()$Latitude))      
    }
    
    # Disable the back button
    shinyjs::disable("back_button")
    # Show the select/deselect all button on pickerInput
    shinyjs::runjs('$(".bs-select-all").removeClass("hidden"); $(".bs-deselect-all").removeClass("hidden");')
    # Enable UI selection again
    shinyjs::enable("selected_districts")
    shinyjs::enable("condo_selection")
    # Hide the dynamic UI and graphs
    shinyjs::hide("dynamic_ui")
    shinyjs::hide("dynamic_graph_title")
    shinyjs::hide("dynamic_graph")
    # Back to browsing
    is_browsing(TRUE)
  })
  
  # Conditionally render dynamic UI elements
  output$dynamic_ui <- renderUI({
    # Check if a property marker is clicked or a condo is selected
    if (!is.null(selectedProperty())) {
      
      # Create string for each piece of information in the UI
      header <- selectedProperty()$Project.Name
      district <- selectedProperty()$dist.full
      region <- districts[districts$district == selectedProperty()$dist.no,]$region
      
      # Get names of nearest facilities
      nearest_mrt_name <- selectedProperty()$nearest_mrt
      nearest_hospital_name <- selectedProperty()$nearest_hospital
      nearest_mall_name <- selectedProperty()$nearest_mall
      nearest_bus_stop_name <- paste0(selectedProperty()$Description, " (", selectedProperty()$nearest_busstop, ")")
      
      # Get distance in meters
      nearest_mrt_distance <- facilityRoutes()$mrt_route$distance_m
      nearest_hospital_distance <- facilityRoutes()$hospital_route$distance_m
      nearest_mall_distance <- facilityRoutes()$mall_route$distance_m
      nearest_bus_stop_distance <- facilityRoutes()$bus_stop_route$distance_m
      
      # Transaction information regarding condo
      avg_overall_monthly_rent <- mean(transactions[transactions$Project.Name == header,]$monthly.rent)
      total_no_transactions <- nrow(transactions[transactions$Project.Name == header,])
      
      # Custom CSS for each line
      line_1 <- paste0('<p style="font-size: 26px;"><b>', header, '</b></p>')
      line_2 <- paste0('<p style="font-size: 20px;"><b><i>', district, '</i></b>', ' (', region, ')</p>')
      line_3 <- paste0('<p style="color: blue; font-size: 16px;"><b>🚉 Nearest MRT station:</b> ', nearest_mrt_name, ' - ', nearest_mrt_distance, ' m</p>')
      line_4 <- paste0('<p style="color: red; font-size: 16px;"><b>🏥 Nearest Hospital:</b> ', nearest_hospital_name, ' - ', nearest_hospital_distance, ' m</p>')
      line_5 <- paste0('<p style="color: green; font-size: 16px;"><b>🏬 Nearest Mall:</b> ', nearest_mall_name, ' - ', nearest_mall_distance, ' m</p>')
      line_6 <- paste0('<p style="color: purple; font-size: 16px;"><b>🚏 Nearest Bus Stop:</b> ', nearest_bus_stop_name, ' - ', nearest_bus_stop_distance, ' m</p>')
      line_7 <- paste0('<p style="color: darkorange; font-size: 20px;"><b>Details about specific property for period (January 2022 - September 2023)</b></p>')
      line_8 <- paste0('<p style="font-size: 16px;"><b>Average Overall Monthly Rent:</b> ', round(avg_overall_monthly_rent, 2), '</p>')
      line_9 <- paste0('<p style="font-size: 16px;"><b>Total No. of Transactions:</b> ', total_no_transactions,'</p>')
        
      # Show final HTML Output for UI
      HTML(paste0(line_1, line_2, "<br>", line_3, line_4, line_5, line_6, "<br>", line_7, line_8, line_9))
      
    }
  })
  
  # Conditionally render responsive dynamic graph title
  output$dynamic_graph_title <- renderUI({
    # Check if a property marker is clicked or a condo is selected
    if (!is.null(selectedProperty())) {
      proj.name <- selectedProperty()$Project.Name
      HTML(paste0('<p style="font-size: 20px;"><b>Average Monthly Rent by Bedroom and Floor Area for ', proj.name, '</b></p>'))
    }
  })
  
  # Conditionally render dynamic graph
  output$dynamic_graph <- renderPlotly({
    # Check if a property marker is clicked or a condo is selected
    if (!is.null(selectedProperty())) {
      # Create and return the dynamic graph
      name <- selectedProperty()$Project.Name
      
      # Cleaning and sorting data to find average monthly rent by bedroom and floor area
      df <- transactions %>% 
        filter(Project.Name == name) %>% 
        group_by(No.of.Bedroom, Floor.Area..SQM.,) %>% 
        summarise(mean_rent = mean(monthly.rent),
                  count = n()) %>% 
        arrange(No.of.Bedroom, str_length(Floor.Area..SQM.))
      
      # Convert Floor.Area..SQM. to a factor as well if it's not already
      df$Floor.Area..SQM. <- factor(df$Floor.Area..SQM., levels = mixedsort(unique(df$Floor.Area..SQM.)))
      
      p <- ggplot(df, aes(x = No.of.Bedroom, y = mean_rent, fill = Floor.Area..SQM., label = count,
                          text = paste("<b>Avg Monthly Rent:</b> ", round(mean_rent, 2), "<br><b>Transaction Count:</b> ", count, "<br><b>Floor area (sqm):</b> ", Floor.Area..SQM.))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_viridis_d(direction = -1) + 
        labs(x = "No. of Bedrooms",
             y = "Mean Rent (in SGD)",
             fill = HTML("Area in sqm<sup>2</sup>")) +
        # ggtitle(paste0("<b>Average Monthly Rent by Bedroom and Floor Area for ", name, "</b>")) + # Not responsive to changing width of browser, so ignore
        theme_bw()
      
      # Convert ggplot to Plotly for interactivity
      ggplotly(p, tooltip = "text") %>% 
        layout(
          margin = list(t = 0)  # Adjust the top margin as needed
        )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
