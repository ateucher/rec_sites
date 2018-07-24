library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(httr)

base_url <- "https://openmaps.gov.bc.ca/geo/pub/WHSE_FOREST_TENURE.FTEN_REC_SITE_POINTS_SVW/ows"
response <- httr::GET(base_url,
                      query = list(service = "WFS", 
                                   version = "2.0.0", 
                                   request = "GetFeature", 
                                   typeName = "WHSE_FOREST_TENURE.FTEN_REC_SITE_POINTS_SVW", 
                                   outputFormat = "json", 
                                   SRSNAME = "epsg:4326"))
stop_for_status(response)

rec_sites <- read_sf(content(response, as = "text"))
rec_sites_data <- st_set_geometry(rec_sites, NULL)

access_cols <- names(rec_sites_data)[grepl("^SUB_ACCESS\\d", names(rec_sites_data))]
access_list <- c(
    "Motorhome" = "MH", 
    "2-wheel drive vehicle" = "2W", 
    "4-wheel drive vehicle" = "4W", 
    "Boat (motorized)" = "BM", 
    "Boat (non-motorized)" = "BN", 
    "Trail (motorized)" = "TM", 
    "Trail (non-motorized)" = "TN"
)

activity_cols <- names(rec_sites_data)[grepl("^ACTIVITY_DESC", names(rec_sites_data))]
activities_list <- rec_sites_data %>% 
    select(!!activity_cols) %>% 
    unlist() %>% 
    unname() %>% 
    unique() %>% 
    na.omit() %>% 
    sort()

struct_cols <- names(rec_sites_data)[grepl("^STRUCTURE_DESC", names(rec_sites_data))]
struct_list <- rec_sites_data %>% 
    select(!!struct_cols) %>% 
    unlist() %>% 
    unname() %>% 
    unique() %>% 
    na.omit() %>% 
    sort()

maint_list <- c("Any", na.omit(unique(rec_sites$MAINTAIN_STD_DESC)))

df_row_to_html_list <- function(x) {
    if (is.data.frame(x)) {
        stopifnot(nrow(x) == 1L)
        x <- unlist(x)
    }
    paste0(c("<ul><li>", 
             paste0(na.omit(x), collapse = "</li><li>"),
             "</li></ul>"), 
           collapse = "")
}

addSiteMarkers <- function(map, data) {
    addCircleMarkers(map = map, data = data, radius = 6, color = "#007f00",
                     opacity = 0.8, fillOpacity = 0.5, weight = 3,
                     clusterOptions = markerClusterOptions(), 
                     layerId = ~FOREST_FILE_ID)
}

ui <- fluidPage(
    tags$a(href = "https://github.com/ateucher/rec_sites/", 
           icon("github", "fa-3x"), 
           style = "position: absolute; top: 0; right: 0; border: 0; margin: 20px; color: gray;"),
    
    # Application title
    titlePanel("B.C. Recreation Sites Explorer"),
    p("A tool to help you explore British Columbia's", 
      a(href = "http://www.sitesandtrailsbc.ca/", "Recreation Sites"), 
      "and find a place to go camping!"),
    p("This site is not affiliated with the Government of British Columbia. 
      It uses open data from the", 
      a(href = "https://catalogue.data.gov.bc.ca/dataset/bc37b35d-8d00-45ab-9ab2-57cae75637ad", 
        "B.C. Data Catalogue.")),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("sites",
                        "Number of campsites:",
                        min = 0,
                        max = 120,
                        value = c(0, 120)), 
            hr(),
            selectInput("access", "Access:", 
                        choices = access_list, 
                        multiple = TRUE),
            radioButtons("access_bool", "Combine selected access modes with:", 
                         choices = c("any", "all"), 
                         selected = "any"),
            hr(),
            selectInput("activity", "Activites:", 
                        choices = activities_list, 
                        multiple = TRUE),
            radioButtons("activity_bool", "Combine selected activities with:", 
                         choices = c("any", "all"), 
                         selected = "any"),
            hr(),
            selectInput("struct", "Structures and Amenities:", 
                         choices = struct_list, 
                         multiple = TRUE), 
            radioButtons("struct_bool", "Combine selected structures with:", 
                         choices = c("any", "all"), 
                         selected = "any"),
            hr(),
            radioButtons("maint", "Site Maintenance:", 
                        choices = maint_list, 
                        selected = "Any")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("map", width = 800, height = 600),
           htmlOutput("site_description")
        )
    )
)

server <- function(input, output) {

    output$map <- renderLeaflet({
        
        leaflet() %>% 
            addProviderTiles("Esri.WorldTopoMap") %>%
            addSiteMarkers(data = rec_sites)
    })
    
    observe({
        disp_sites <- rec_sites[rec_sites$NUM_CAMP_SITES >= input$sites[1] & 
                                    rec_sites$NUM_CAMP_SITES <= input$sites[2], ]
        

        # Access
        if (!is.null(input$access)) {
            bool_fun <- switch(input$access_bool, 
                               "any" = any,
                               "all" = all)
            
            rows <- apply(disp_sites[, access_cols], 1, function(x) {
                bool_fun(input$access %in% x)
            })
            disp_sites <- disp_sites[rows, ]   
        }
                
        # Available activities
        if (!is.null(input$activity)) {
            bool_fun <- switch(input$activity_bool, 
                               "any" = any,
                               "all" = all)
            
            rows <- apply(disp_sites[, activity_cols], 1, function(x) {
                bool_fun(input$activity %in% x)
            })
            disp_sites <- disp_sites[rows, ]   
        }
        
        # Structures/features
        if (!is.null(input$struct)) {
            bool_fun <- switch(input$struct_bool, 
                               "any" = any,
                               "all" = all)
            
            rows <- apply(disp_sites[, struct_cols], 1, function(x) {
                bool_fun(input$struct %in% x)
            })
            disp_sites <- disp_sites[rows, ]   
        }

        # Maintenance
        if (input$maint != "Any") {
            disp_sites <- disp_sites[disp_sites$MAINTAIN_STD_DESC == input$maint, ]
        }
        
        leafletProxy("map") %>% 
            clearMarkers() %>% 
            clearMarkerClusters() %>% 
            addSiteMarkers(data = disp_sites)
    })
    
    output$site_description <- renderText({
        req(input$map_marker_click$id)
        
        row <- rec_sites_data[rec_sites_data$FOREST_FILE_ID == input$map_marker_click$id, ]
        
        site_name <- tools::toTitleCase(tolower(
            gsub("\\.", ". ", row$PROJECT_NAME)
        ))
        
        paste0(sprintf("<h3>
                       <a href=http://www.sitesandtrailsbc.ca/search/search-result.aspx?site=%s&type=Site>", 
                       input$map_marker_click$id),
                       site_name, "</a> (", row$SITE_LOCATION, ")</h3>", 
               row$PROJECT_DESCRIPTION,
               "<h4>Number of campsites: ", 
               row[, "NUM_CAMP_SITES", drop = TRUE], 
               "</h4>",
               "<h4>Access:</h4>", 
               df_row_to_html_list(
                   names(access_list)[access_list %in% row[, access_cols]]
               ),
               "<h4>Structures and Amenities:</h4>", 
               df_row_to_html_list(row[, struct_cols]),
               "<h4>Activities:</h4>", 
               df_row_to_html_list(row[, activity_cols]),
               "<h4>Directions:</h4>", 
               row$DRIVING_DIRECTIONS)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

