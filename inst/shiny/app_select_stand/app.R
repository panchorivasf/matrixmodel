library(shiny)
library(shinyjs)
library(shinythemes)
library(sf)
library(leaflet)
library(leaflet.extras)
library(DT)

# UI definition
stand_selector_ui <- function() {
  # Enhanced CSS for better table styling
  tags$head(
    tags$style(HTML("
    /* Target only the selected_table DataTable */
    #selected_table .dataTables_wrapper table.dataTable {
      width: 100% !important;
    }
    #selected_table .dataTables_wrapper table.dataTable thead th {
      background-color: #2d3e50 !important;
      color: white !important;
      border-bottom: 2px solid #dee2e6 !important;
    }
    #selected_table .dataTables_wrapper table.dataTable tbody td {
      background-color: #2d3e50 !important;
      color: white !important;
      border-color: #454d55 !important;
    }
    #selected_table .dataTables_wrapper table.dataTable tbody tr:hover td {
      background-color: #1a2530 !important;
      color: white !important;
    }
    /* Selected rows styling */
    #selected_table .dataTables_wrapper table.dataTable tbody tr.selected td {
      background-color: #3498db !important;
      color: white !important;
    }
    /* DataTables controls styling - only for selected_table */
    #selected_table .dataTables_wrapper .dataTables_paginate .paginate_button {
      color: white !important;
      background: none !important;
      border: 1px solid #454d55 !important;
    }
    #selected_table .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background-color: #1a2530 !important;
      color: white !important;
    }
    #selected_table .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background-color: #3498db !important;
      color: white !important;
      border-color: #3498db !important;
    }
    #selected_table .dataTables_wrapper .dataTables_info {
      color: white !important;
    }
    #selected_table .dataTables_wrapper .dataTables_length label,
    #selected_table .dataTables_wrapper .dataTables_filter label {
      color: white !important;
    }
    #selected_table .dataTables_wrapper .dataTables_length select,
    #selected_table .dataTables_wrapper .dataTables_filter input {
      background-color: #454d55 !important;
      color: white !important;
      border: 1px solid #6c757d !important;
    }
  "))
  )

  fluidPage(
    theme = shinythemes::shinytheme("darkly"),
    shinyjs::useShinyjs(),
    shiny::titlePanel("Select Stands from Map"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),

        shiny::selectInput("map.style", "Map Style:",
                           choices = c(
                             "Classic" = "classic",
                             "Dark" = "dark",
                             "Satellite" = "satellite",
                             "Topographic" = "topo"
                           ),
                           selected = "classic"),

        # Symbology toggle
        shiny::radioButtons("symbology", "Color Points By:",
                            choices = c("None" = "none",
                                        "Basal Area (PrevB)" = "PrevB",
                                        "Density (PrevN)" = "PrevN"),
                            selected = "none"),

        shiny::actionButton("add_button", "Add Selected Points", class = "btn-primary"),
        shiny::actionButton("drop_button", "Drop Selected Rows", class = "btn-danger"),
        shiny::downloadButton("download_button", "Download Table"),
        shiny::actionButton("save_r_button", "Save to R", class = "btn-success"),
        shiny::br(), br(),
        shiny::h4("Instructions:"),
        shiny::p("1. Upload a CSV file with coordinates (lat/lon)"),
        shiny::p("2. Choose symbology for points (optional)"),
        shiny::p("3. Click on individual points or use rectangle tool to select multiple points"),
        shiny::p("4. Click 'Add Selected Points' to add them to the table"),
        shiny::p("5. Select rows in the table and click 'Drop Selected Rows' to remove them"),
        shiny::p("6. Use 'Download Table' to save selected PlotIDs as CSV"),
        shiny::p("7. Use 'Save to R' to save the complete table as a data frame")
      ),

      shiny::mainPanel(
        width = 9,
        leaflet::leafletOutput("map", height = "500px"),
        shiny::br(),
        shiny::h4("Selected Points:"),
        DT::DTOutput("selected_table")
      )
    )
  )
}

# Server logic
stand_selector_server <- function(input, output, session) {

  options(shiny.maxRequestSize = (1000 * 1024 ^ 2))

  # Reactive values to store data
  values <- reactiveValues(
    all_data = NULL,
    selected_data = data.frame(),
    temp_selected = data.frame(),
    draw_count = 0,
    clicked_points = character(),
    color_palette = NULL,
    base_map_created = FALSE
  )

  # Create the initial base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -120, lat = 40, zoom = 5)  # Default view
  })

  # Update the map creation function for better symbology
  updateMap <- function() {
    req(values$all_data)

    data <- values$all_data
    # Define tile providers
    tile_providers <- list(
      dark = providers$CartoDB.DarkMatter,
      satellite = providers$Esri.WorldImagery,
      classic = providers$OpenStreetMap.Mapnik,
      topo = providers$OpenTopoMap
    )

    # Get the selected provider or use default
    selected_provider <- tile_providers[[input$map.style]]
    if (is.null(selected_provider)) {
      selected_provider <- providers$OpenStreetMap.Mapnik  # default
    }

    # Get current view state before updating
    current_map <- leafletProxy("map")

    # Use leafletProxy to update tiles without recreating the entire map
    # or changing the view
    leafletProxy("map") %>%
      clearTiles() %>%
      addProviderTiles(selected_provider)
  }


  # Function to update point symbology
  updatePointSymbology <- function() {
    req(values$all_data)

    data <- values$all_data

    # Clear existing markers and controls
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearControls()

    # Add circle markers with appropriate coloring
    if (input$symbology != "none") {
      var_data <- data[[input$symbology]]

      # Create a better color palette with appropriate domain
      if (is.numeric(var_data) && length(unique(var_data)) > 1) {
        # Use quantile-based breaks for better color distribution
        color_pal <- colorBin(
          palette = "inferno",
          domain = var_data,
          pretty = FALSE
        )

        # Add markers
        leafletProxy("map") %>%
          addCircleMarkers(
            data = data,
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~PlotID,
            radius = 4,
            color = ~color_pal(get(input$symbology)),
            fillColor = ~color_pal(get(input$symbology)),
            fillOpacity = 0.8,
            stroke = TRUE,
            weight = 1,
            label = ~paste(
              "PlotID:", PlotID, "\n",
              "PrevB:", round(PrevB, digits = 1), "\n",
              "PrevN:", round(PrevN, digits = 1), "\n",
              "Hs:", round(Hs, digits = 1), "\n",
              "Hd:", round(Hd, digits = 1), "\n",
              input$symbology, ":", round(get(input$symbology), digits = 1)
            ),
            popup = ~paste0(
              "<strong>PlotID:</strong> ", PlotID, "<br>",
              "<strong>PrevB:</strong> ", round(PrevB, digits = 1), "<br>",
              "<strong>PrevN:</strong> ", round(PrevN, digits = 1), "<br>",
              "<strong>Hs:</strong> ", round(Hs, digits = 1), "<br>",
              "<strong>Hd:</strong> ", round(Hd, digits = 1), "<br>",
              "<strong>", input$symbology, ":</strong> ", round(get(input$symbology), digits = 1)
            )
          )

        # Add legend separately with explicit values
        leafletProxy("map") %>%
          addLegend(
            position = "bottomright",
            pal = color_pal,
            values = var_data,  # Use the actual data vector, not a formula
            title = input$symbology,
            opacity = 1
          )
      } else {
        # Fallback to default if variable isn't suitable for coloring
        leafletProxy("map") %>%
          addCircleMarkers(
            data = data,
            lng = ~Longitude,
            lat = ~Latitude,
            layerId = ~PlotID,
            radius = 4,
            color = "orange",
            fillOpacity = 0.8,
            stroke = FALSE,
            label = ~paste(
              "PlotID:", PlotID, "\n",
              "PrevB:", round(PrevB, digits = 1), "\n",
              "PrevN:", round(PrevN, digits = 1), "\n",
              "Hs:", round(Hs, digits = 1), "\n",
              "Hd:", round(Hd, digits = 1)
            )
          )
      }
    } else {
      # Default orange markers
      leafletProxy("map") %>%
        addCircleMarkers(
          data = data,
          lng = ~Longitude,
          lat = ~Latitude,
          layerId = ~PlotID,
          radius = 2,
          color = "orange",
          fillOpacity = 0.9,
          stroke = FALSE,
          label = ~paste(
            "PlotID:", PlotID, "\n",
            "PrevB:", round(PrevB, digits = 1), "\n",
            "PrevN:", round(PrevN, digits = 1), "\n",
            "Hs:", round(Hs, digits = 1), "\n",
            "Hd:", round(Hd, digits = 1)
          )
        )
    }

    # Re-add draw toolbar
    leafletProxy("map") %>%
      removeDrawToolbar() %>%
      addDrawToolbar(
        targetGroup = 'draw',
        polylineOptions = FALSE,
        polygonOptions = list(
          shapeOptions = list(
            color = '#ff0000',
            weight = 2,
            fillOpacity = 0.3
          ),
          repeatMode = FALSE
        ),
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = list(
          shapeOptions = list(
            color = '#ff0000',
            weight = 2,
            fillOpacity = 0.3
          ),
          repeatMode = FALSE
        ),
        editOptions = FALSE
      )

    # Update highlighting
    updateMapHighlighting()
  }

  # Read uploaded CSV file
  observeEvent(input$file, {
    req(input$file)

    tryCatch({
      data <- read.csv(input$file$datapath)

      # Check if required columns exist
      required_cols <- c("Latitude", "Longitude", "PrevB", "PrevN", "Hs", "Hd", "PlotID")
      missing_cols <- setdiff(required_cols, colnames(data))

      if (length(missing_cols) > 0) {
        showNotification(paste("Missing columns:", paste(missing_cols, collapse = ", ")),
                         type = "error")
        return()
      }

      # Convert PlotID to character to avoid scientific notation
      data$PlotID <- as.character(data$PlotID)

      values$all_data <- data
      values$clicked_points <- character()
      values$temp_selected <- data.frame()

      # Update map with new data
      updatePointSymbology()

      leafletProxy("map") %>%
        setView(lng = mean(data$Longitude, na.rm = TRUE),
                lat = mean(data$Latitude, na.rm = TRUE),
                zoom = 10)
      updateMap()

    }, error = function(e) {
      showNotification("Error reading file. Please check the format.", type = "error")
    })
  })

  # Update map when map style changes
  observeEvent(input$map.style, {
    req(values$all_data)
    updateMap()
  })

  # Update map when symbology changes
  observeEvent(input$symbology, {
    req(values$all_data)
    updatePointSymbology()
  })

  # Function to update map highlighting
  updateMapHighlighting <- function() {
    # Clear previous highlights using a safe approach
    leafletProxy("map") %>% clearGroup("temp_selected")

    if (length(values$clicked_points) > 0 && !is.null(values$all_data)) {
      selected_data <- values$all_data[values$all_data$PlotID %in% values$clicked_points, ]

      if (nrow(selected_data) > 0) {
        # Add highlights with a different group to avoid conflicts
        leafletProxy("map") %>%
          addCircleMarkers(
            data = selected_data,
            lng = ~Longitude,
            lat = ~Latitude,
            group = "temp_selected",
            radius = 10,  # Larger radius for better visibility
            color = "#FF0000",
            fillColor = "#FF0000",
            fillOpacity = 0.9,
            stroke = TRUE,
            weight = 3,
            layerId = ~paste0("highlight_", PlotID)
          )
      }
    }
  }

  # Handle individual point clicks
  observeEvent(input$map_marker_click, {
    req(values$all_data)

    click <- input$map_marker_click
    plot_id <- click$id

    # Remove the "highlight_" prefix if it exists (from previous selections)
    plot_id <- gsub("^highlight_", "", plot_id)

    if (plot_id %in% values$clicked_points) {
      # Remove from selection if already clicked
      values$clicked_points <- setdiff(values$clicked_points, plot_id)
    } else {
      # Add to selection
      values$clicked_points <- c(values$clicked_points, plot_id)
    }

    # Update temp selection
    if (length(values$clicked_points) > 0) {
      values$temp_selected <- values$all_data[values$all_data$PlotID %in% values$clicked_points, ]
    } else {
      values$temp_selected <- data.frame()
    }

    # Update map highlighting
    updateMapHighlighting()
  })

  # Handle rectangle selection from map
  observeEvent(input$map_draw_new_feature, {
    req(values$all_data)

    tryCatch({
      # Increment draw count to track rectangle creation
      values$draw_count <- values$draw_count + 1

      # Get the drawn rectangle
      feature <- input$map_draw_new_feature

      # Extract coordinates from the rectangle
      coords <- unlist(feature$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = TRUE)

      # Ensure the polygon is closed
      if (!all(coords[1,] == coords[nrow(coords),])) {
        coords <- rbind(coords, coords[1,])
      }

      # Create a polygon from the rectangle
      poly <- st_polygon(list(coords)) %>%
        st_sfc(crs = 4326)

      # Convert data to spatial object
      points_sf <- st_as_sf(values$all_data,
                            coords = c("Longitude", "Latitude"),
                            crs = 4326)

      # Find points within the polygon
      selected_points <- st_intersects(points_sf, poly, sparse = FALSE)

      # Get the selected data
      selected_data <- values$all_data[selected_points[, 1], ]

      # Update clicked points with rectangle selection
      if (nrow(selected_data) > 0) {
        values$clicked_points <- unique(c(values$clicked_points, selected_data$PlotID))
        values$temp_selected <- values$all_data[values$all_data$PlotID %in% values$clicked_points, ]
      }

      # Update map highlighting
      updateMapHighlighting()

      # Remove the drawn rectangle immediately after processing
      leafletProxy("map") %>% removeDrawToolbar(clearFeatures = TRUE) %>%
        addDrawToolbar(
          targetGroup = 'draw',
          polylineOptions = FALSE,
          polygonOptions = list(
            shapeOptions = list(
              color = '#ff0000',
              weight = 2,
              fillOpacity = 0.3
            ),
            repeatMode = FALSE
          ),
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          rectangleOptions = list(
            shapeOptions = list(
              color = '#ff0000',
              weight = 2,
              fillOpacity = 0.3
            ),
            repeatMode = FALSE
          ),
          editOptions = FALSE
        )

    }, error = function(e) {
      showNotification(paste("Error selecting points:", e$message), type = "error")
    })
  })

  # Add selected points to the table
  observeEvent(input$add_button, {
    req(values$temp_selected)
    req(nrow(values$temp_selected) > 0)

    # Combine with existing selected data, avoiding duplicates
    if (nrow(values$selected_data) == 0) {
      values$selected_data <- values$temp_selected
    } else {
      # Remove duplicates by PlotID
      new_points <- values$temp_selected[!values$temp_selected$PlotID %in%
                                           values$selected_data$PlotID, ]
      if (nrow(new_points) > 0) {
        values$selected_data <- rbind(values$selected_data, new_points)
      }
    }

    # Clear temporary selection
    values$clicked_points <- character()
    values$temp_selected <- data.frame()
    leafletProxy("map") %>% clearGroup("temp_selected")

    showNotification(paste("Added points to selection"), type = "message")
  })

  # Display selected data table
  output$selected_table <- DT::renderDT({
    req(values$selected_data)
    req(nrow(values$selected_data) > 0)

    DT::datatable(
      values$selected_data,
      selection = 'multiple',
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'ltip',
        searching = TRUE,
        paging = TRUE,
        info = TRUE
      ),
      style = 'bootstrap5'
    ) %>%
      formatStyle(
        columns = 1:ncol(values$selected_data),
        target = 'row',
        backgroundColor = styleEqual(c(TRUE), c('#343a40')),
        color = 'white'
      ) %>%
      formatRound(columns = c('PrevB', 'PrevN'), digits = 2)
  })

  # Drop selected rows from table
  observeEvent(input$drop_button, {
    req(input$selected_table_rows_selected)
    req(nrow(values$selected_data) > 0)

    if (length(input$selected_table_rows_selected) > 0) {
      values$selected_data <- values$selected_data[-input$selected_table_rows_selected, ]

      # If no rows left, reset to empty dataframe
      if (nrow(values$selected_data) == 0) {
        values$selected_data <- data.frame()
      }
    }
  })

  # Download handler - prevent scientific notation
  output$download_button <- downloadHandler(
    filename = function() {
      paste("selected_points_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$selected_data)
      req(nrow(values$selected_data) > 0)

      # Create output with only PlotID as requested, ensuring no scientific notation
      output_data <- data.frame(PlotID = values$selected_data$PlotID)

      # Write CSV without scientific notation and preserving character format
      write.csv(output_data, file, row.names = FALSE, quote = FALSE)
    }
  )

  # Save to R environment
  observeEvent(input$save_r_button, {
    req(values$selected_data)
    req(nrow(values$selected_data) > 0)

    # Assign to global environment
    assign("selected_stands", values$selected_data, envir = .GlobalEnv)

    showNotification(
      paste("Selected data saved as 'selected_stands' in R environment with",
            nrow(values$selected_data), "rows"),
      type = "message"
    )
  })

  # Enable/disable buttons based on state
  observe({
    shinyjs::toggleState("add_button", condition = !is.null(values$temp_selected) && nrow(values$temp_selected) > 0)
    shinyjs::toggleState("drop_button", condition = !is.null(input$selected_table_rows_selected) &&
                           length(input$selected_table_rows_selected) > 0 &&
                           nrow(values$selected_data) > 0)
    shinyjs::toggleState("download_button", condition = !is.null(values$selected_data) && nrow(values$selected_data) > 0)
    shinyjs::toggleState("save_r_button", condition = !is.null(values$selected_data) && nrow(values$selected_data) > 0)
  })
}

# Run the application
shinyApp(ui = stand_selector_ui(), server = stand_selector_server)
