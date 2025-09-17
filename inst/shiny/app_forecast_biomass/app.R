library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(dplyr)
library(glue)

# Custom CSS for dark theme
dark_css <- "
  /* Dark theme styling */
  .content-wrapper, .right-side {
    background-color: #1a2530;
    color: #ffffff;
  }

  .main-header .navbar {
    background-color: #2d3e50 !important;
    border-bottom: 1px solid #4a6572;
  }

  .main-header .navbar-nav > li > a {
    color: #ffffff !important;
  }

  .main-sidebar, .left-side {
    background-color: #2d3e50;
  }

  .sidebar-menu > li.header {
    color: #ffffff;
    background-color: #34495e;
  }

  .sidebar-menu > li > a {
    color: #ffffff;
  }

  .sidebar-menu > li:hover > a {
    background-color: #4a6572;
  }

  .box {
    background-color: #34495e;
    border: 1px solid #4a6572;
    color: #ffffff;
  }

  .box-header {
    background-color: #34495e;
    border-bottom: 1px solid #4a6572;
  }

  .form-control {
    background-color: #2d3e50;
    border: 1px solid #4a6572;
    color: #ffffff;
  }

  .form-control:focus {
    background-color: #34495e;
    border-color: #5dade2;
    box-shadow: 0 0 5px rgba(93, 173, 226, 0.5);
  }

  .btn-primary {
    background-color: #3498db;
    border-color: #3498db;
  }

  .btn-primary:hover {
    background-color: #2980b9;
    border-color: #2980b9;
  }

  .progress-bar {
    background-color: #3498db;
  }

  .info-box {
    background-color: #34495e;
    color: #ffffff;
  }

  .small-box {
    background-color: #34495e !important;
  }

  /* Data table styling */
  .dataTables_wrapper {
    color: #ffffff;
  }

  .dataTables_info, .dataTables_paginate {
    color: #ffffff !important;
  }
"

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Matrix Model"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Forecast Setup", tabName = "setup", icon = icon("tree")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Visualization", tabName = "viz", icon = icon("chart-area"))
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML(dark_css))),

    tabItems(
      # Setup tab
      tabItem(
        tabName = "setup",
        fluidRow(
          box(
            title = "Forecast Configuration",
            status = "primary",
            solidHeader = TRUE,
            width = 6,

            fileInput("data_file",
                      "Upload Data File (.csv)",
                      accept = c(".csv")),

            # In the UI, update the plot selection section:
            fluidRow(
              column(8,
                     selectizeInput("plot_id",
                                    "Select Plot ID:",
                                    choices = NULL,
                                    options = list(
                                      placeholder = 'Select or type plot ID',
                                      maxOptions = 100
                                    ))
              )
            ),

            numericInput("years",
                         "Simulation Years",
                         value = 50,
                         min = 1,
                         max = 100,
                         step = 5),

            checkboxInput("clear_start",
                          "Clear Start",
                          value = FALSE),

            checkboxInput("allow_colon",
                          "Allow colonization",
                          value = FALSE),

            numericInput("clear_year",
                         "Clearcut Year",
                          value = NULL,
                         min = 1,
                         max = 200),

            fileInput("plant_init",
                      "Upload initial plantation file (.csv)",
                      accept = c(".csv")),

            fileInput("plant_post",
                      "Upload midterm plantation file (.csv)",
                      accept = c(".csv")),


            box(
              title = "Selected Plot",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              uiOutput("plot_summary")
            ),

            # OUTPUT
            box(
              title = "Output Settings",
              status = "info",
              solidHeader = TRUE,
              width = 6,

              textInput("output_folder",
                        "Output Folder Name",
                        value = "simulation_output"),

              verbatimTextOutput("output_location_info"),

              helpText("Output will be automatically created in current working directory")
            ),

            br(),

            actionButton("run_forecast",
                         "Run Forecast",
                         class = "btn-primary btn-lg",
                         icon = icon("play"))
          ),

          # box(
          #   title = "Model Files",
          #   status = "info",
          #   solidHeader = TRUE,
          #   width = 6,
          #
          #   textInput("m_model",
          #             "Mortality Model Path",
          #             value = "models/model_mortality.rds"),
          #
          #   textInput("u_model",
          #             "Upgrowth Model Path",
          #             value = "models/model_upgrowth.rds"),
          #
          #   textInput("r_model",
          #             "Recruitment Model Path",
          #             value = "models/model_recruitment.rds")
          #
          # )
        ),

        fluidRow(
          box(
            title = "Data Preview",
            status = "warning",
            solidHeader = TRUE,
            width = 12,

            DT::dataTableOutput("data_preview")
          )
        ),

        fluidRow(
          box(
            title = "Progress",
            status = "success",
            solidHeader = TRUE,
            width = 12,

            verbatimTextOutput("progress_text"),

            conditionalPanel(
              condition = "input.run_forecast > 0",
              br(),
              progressBar(
                id = "forecast_progress",
                value = 0,
                status = "info",
                display_pct = TRUE
              )
            )
          )
        )
      ),

      # Results tab
      tabItem(
        tabName = "results",
        fluidRow(
          valueBoxOutput("total_years"),
          valueBoxOutput("plot_processed"),
          valueBoxOutput("final_ba")
        ),

        fluidRow(
          tabBox(
            title = "Results Tables",
            width = 12,

            tabPanel("Summary by Year",
                     DT::dataTableOutput("year_summary_table")),

            tabPanel("Species Summary",
                     DT::dataTableOutput("species_table")),

            tabPanel("DBH Group Summary",
                     DT::dataTableOutput("dgp_table")),

            tabPanel("Detailed Predictions",
                     DT::dataTableOutput("predictions_table"))
          )
        )
      ),

      # Visualization tab
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            title = "Time Series Plots",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            fluidRow(
              column(6,
                     selectInput("plot_metric",
                                 "Select Metric:",
                                 choices = list("Basal Area" = "BA_total_mean",
                                                "Tree Density" = "N_total_mean"),
                                 selected = "BA_total_mean")
              ),
              column(6,
                     checkboxInput("log_scale", "Log Scale", value = FALSE)
              )
            ),

            plotlyOutput("time_series_plot", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Species Composition Over Time",
            status = "info",
            solidHeader = TRUE,
            width = 12,

            plotlyOutput("species_composition_plot", height = "400px")
          )
        ),

        fluidRow(
          box(
            title = "Download Results",
            status = "success",
            solidHeader = TRUE,
            width = 12,

            p("Download the complete forecast results:"),
            br(),

            fluidRow(
              column(3, downloadButton("download_summary", "Summary CSV", class = "btn-info")),
              column(3, downloadButton("download_species", "Species CSV", class = "btn-info")),
              column(3, downloadButton("download_dgp", "DGP CSV", class = "btn-info")),
              column(3, downloadButton("download_predictions", "Full Data CSV", class = "btn-info"))
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive values to store results
  values <- reactiveValues(
    data = NULL,
    results = NULL,
    progress = 0
  )

  output$models_status <- renderText({
    if (models_loaded()) {
      "All models loaded and ready!"
    } else {
      "Models are still loading. Please wait..."
    }
  })

  # Reactive values to store loaded models
  # models <- reactiveValues(
  #   mortality = NULL,
  #   upgrowth = NULL,
  #   recruitment = NULL
  # )

  # # Load models when needed (with error handling)
  # observe({
  #   tryCatch({
  #     if (is.null(models$mortality)) {
  #       models$mortality <- load_model("mortality")
  #     }
  #     if (is.null(models$upgrowth)) {
  #       models$upgrowth <- load_model("upgrowth")
  #     }
  #     if (is.null(models$recruitment)) {
  #       models$recruitment <- load_model("recruitment")
  #     }
  #   }, error = function(e) {
  #     showNotification(
  #       paste("Error loading models:", e$message),
  #       type = "error",
  #       duration = NULL
  #     )
  #   })
  # })

  # Update your validate_model_path function
  # validate_model_path <- function(model_type) {
  #   req(models[[model_type]])
  #   return(models[[model_type]])
  # }

  # Check if models are loaded (much simpler now!)
  if (!models_loaded()) {
    showNotification("Models are still loading. Please wait...", type = "warning")
    return()
  }


  # # Add this function to validate model paths
  # validate_model_path <- function(model_path) {
  #   expanded_path <- path.expand(model_path)
  #
  #   if (!file.exists(expanded_path)) {
  #     # Try relative to app directory
  #     app_dir <- system.file("shiny", "app_forecast_biomass", package = "matrixmodel")
  #     app_relative_path <- file.path(app_dir, model_path)
  #
  #     if (file.exists(app_relative_path)) {
  #       return(app_relative_path)
  #     } else {
  #       stop("Model file not found: ", expanded_path)
  #     }
  #   }
  #
  #   return(expanded_path)
  # }

  # Data preview
  observeEvent(input$data_file, {
    req(input$data_file)

    tryCatch({
      values$data <- read.csv(input$data_file$datapath)
      showNotification("Data loaded successfully!", type = "success")
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })

  # Show output location info
  output$output_location_info <- renderText({
    output_path <- file.path(getwd(), input$output_folder)
    paste("Output will be saved to:", output_path)
  })

  # Reactive to store available plot IDs
  available_plots <- reactiveVal(character(0))


  observeEvent(values$data, {
    req(values$data)

    # Try to find plot ID column with safer approach
    plot_cols <- c("plot_id", "PlotID", "Plot_ID", "plot", "Plot", "ID", "id")
    plot_col <- NULL

    for (col in plot_cols) {
      if (col %in% names(values$data)) {
        plot_col <- col
        break
      }
    }

    if (!is.null(plot_col)) {
      plot_ids <- unique(values$data[[plot_col]])
      plot_ids <- plot_ids[!is.na(plot_ids) & plot_ids != ""]
      plot_ids <- sort(plot_ids)

      if (length(plot_ids) > 0) {
        available_plots(plot_ids)

        # Update selectize with server-side approach
        updateSelectizeInput(session, "plot_id",
                             choices = plot_ids,
                             server = TRUE,  # Server-side processing for performance
                             selected = ifelse(length(plot_ids) > 0, plot_ids[1], ""))
      } else {
        available_plots(character(0))
        updateSelectizeInput(session, "plot_id", choices = character(0))
      }
    } else {
      # If no plot column found, use row identifiers
      available_plots(paste0("Plot_", 1:min(10, nrow(values$data))))
      updateSelectizeInput(session, "plot_id",
                           choices = available_plots(),
                           server = TRUE,
                           selected = available_plots()[1])

      showNotification("No plot ID column found. Using generated IDs.", type = "warning")
    }
  })

  # Add this helper function for safe value extraction
  safe_value <- function(x, default = NA) {
    if (length(x) == 0 || is.null(x) || is.na(x) || x == "") {
      return(default)
    }
    return(x)
  }

  # # Enhanced refresh button with error handling
  # observeEvent(input$refresh_plots, {
  #   req(values$data)
  #
  #   column_choices <- names(values$data)
  #   if (length(column_choices) == 0) {
  #     showNotification("No columns available in data", type = "error")
  #     return()
  #   }
  #
  #   showModal(modalDialog(
  #     title = "Select Plot ID Column",
  #     selectInput("plot_column",
  #                 "Choose the column that contains Plot IDs:",
  #                 choices = column_choices,
  #                 selected = safe_value(intersect(c("plot_id", "PlotID", "Plot_ID"), column_choices)[1])),
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("confirm_plot_col", "Use Selected Column", class = "btn-success")
  #     )
  #   ))
  # })

  observeEvent(input$confirm_plot_col, {
    req(input$plot_column, values$data)

    if (input$plot_column %in% names(values$data)) {
      plot_ids <- unique(values$data[[input$plot_column]])
      plot_ids <- sort(plot_ids[!is.na(plot_ids)])
      available_plots(plot_ids)

      updateSelectizeInput(session, "plot_id",
                           choices = plot_ids,
                           selected = ifelse(length(plot_ids) > 0, plot_ids[1], ""))

      removeModal()
      showNotification(paste("Using column:", input$plot_column), type = "message")
    }
  })

  # Replace the observeEvent(input$data_preview_cell_clicked) with:
  observeEvent(input$data_preview_cell_clicked, {
    click <- input$data_preview_cell_clicked
    req(click, values$data, click$row > 0)  # Ensure valid row

    # Safely get the row data
    if (click$row <= nrow(values$data)) {
      row_data <- values$data[click$row, ]

      # Try to find plot ID in the clicked row (safe approach)
      plot_cols <- c("plot_id", "PlotID", "Plot_ID", "plot", "Plot", "ID")
      plot_col <- intersect(plot_cols, names(row_data))

      if (length(plot_col) > 0) {
        plot_id <- as.character(row_data[[plot_col[1]]])
        if (!is.na(plot_id) && plot_id != "") {
          updateSelectizeInput(session, "plot_id", selected = plot_id)
          showNotification(paste("Selected plot:", plot_id), type = "message")
        }
      }
    }
  })

  # Enhanced data preview with clickable rows
  output$data_preview <- DT::renderDataTable({
    req(values$data)

    DT::datatable(
      head(values$data, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ltip',
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        # Make rows selectable
        rowCallback = JS(
          "function(row, data) {",
          "  $(row).css('cursor', 'pointer');",
          "  $(row).attr('title', 'Click to select this plot');",
          "}"
        )
      ),
      selection = 'single',
      style = "bootstrap",
      class = "table-bordered table-condensed"
    ) %>%
      formatStyle(columns = names(values$data),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  # Replace the plot summary observer with:
  observeEvent(input$plot_id, {
    req(input$plot_id, values$data)

    # Safe approach to find plot column
    plot_cols <- c("plot_id", "PlotID", "Plot_ID", "plot", "Plot", "ID")
    plot_col <- intersect(plot_cols, names(values$data))

    if (length(plot_col) == 0) return()  # No plot column found

    plot_data <- values$data[values$data[[plot_col[1]]] == input$plot_id, ]

    # Safe summary generation
    output$plot_summary <- renderUI({
      if (nrow(plot_data) == 0) {
        return(tags$p("No data found for selected plot"))
      }

      tagList(
        h5(paste("Plot", input$plot_id))
      )
    })
  })


  # Run forecast - SIMPLIFIED version with fixed notifications
  observeEvent(input$run_forecast, {
    req(values$data)

    if (!models_loaded()) {
      showNotification("Models are still loading. Please wait...",
                       type = "warning")
      return()
    }

    # req(values$data, models$mortality, models$upgrowth, models$recruitment)

    # Reset progress
    updateProgressBar(session, "forecast_progress", value = 0)
    values$progress <- 0

    # Use tryCatch with proper error handling
    tryCatch({
      # Show simple message instead of notification for progress
      output$progress_text <- renderText({"Starting forecast simulation..."})

      # Create output directory in current working directory
      output_dir <- file.path(getwd(), input$output_folder)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        # Use simple message instead of notification
        output$progress_text <- renderText({paste("Created output directory:", output_dir)})
      }

      # validate_model_path <- function(model_path) {
      #   # Extract just the filename from the path
      #   model_name <- basename(model_path)
      #
      #   # Use our new loading system
      #   tryCatch({
      #     load_model(model_name)
      #   }, error = function(e) {
      #     stop("Failed to load model: ", model_name, "\nError: ", e$message)
      #   })
      # }

      # Check if models are loaded
      if (is.null(models$mortality) || is.null(models$upgrowth) || is.null(models$recruitment)) {
        showNotification("Models are still loading. Please wait...", type = "warning")
        return()
      }

      # Update progress
      values$progress <- 30
      updateProgressBar(session, "forecast_progress", value = 30)
      output$progress_text <- renderText({"Loading models..."})

      # Run the forecast
      values$results <- forecast_biomass(
        save_to = output_dir,
        data = values$data,
        plot_id = input$plot_id,
        years = input$years,
        clear_start = input$clear_start,
        clear_year = input$clear_year,
        planting_init = input$planting_init,
        planting_post = input$planting_post,
        allow_colonization = input$allow_colonization,
        # m_model = models$mortality,
        # u_model = models$upgrowth,
        # r_model = models$recruitment,
        output_folder_name = ""
      )

      # Complete progress
      values$progress <- 100
      updateProgressBar(session, "forecast_progress", value = 100)
      output$progress_text <- renderText({paste("Forecast completed for Plot:", input$plot_id)})

      # Show success message (using safe notification)
      showNotification("Forecast completed successfully!", type = "message")
      updateTabItems(session, "tabs", "results")

    }, error = function(e) {
      # Safe error notification
      showNotification(paste("Forecast error:", e$message), type = "error")
      values$progress <- 0
      updateProgressBar(session, "forecast_progress", value = 0)
      output$progress_text <- renderText({paste("Error:", e$message)})
    })
  })

  # Progress text
  output$progress_text <- renderText({
    if (values$progress == 0) {
      "Ready to run forecast. Click 'Run Forecast' to begin."
    } else if (values$progress < 100) {
      paste("Processing forecast... Please wait.")
    } else {
      paste("Forecast completed for Plot:", values$results$plot_id)
    }
  })

  # Value boxes
  output$total_years <- renderValueBox({
    years_val <- if (!is.null(values$results)) input$years else 0
    valueBox(
      value = years_val,
      subtitle = "Simulation Years",
      icon = icon("calendar"),
      color = "blue"
    )
  })

  output$plot_processed <- renderValueBox({
    plot_val <- if (!is.null(values$results)) values$results$plot_id else "None"
    valueBox(
      value = plot_val,
      subtitle = "Plot Processed",
      icon = icon("tree"),
      color = "green"
    )
  })

  output$final_ba <- renderValueBox({
    ba_val <- if (!is.null(values$results)) {
      final_ba <- tail(values$results$summary_by_year$BA_total_mean, 1)
      round(final_ba, 1)
    } else {
      0
    }
    valueBox(
      value = ba_val,
      subtitle = "Final Basal Area (m2/ha)",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })

  # Results tables
  output$year_summary_table <- DT::renderDataTable({
    req(values$results)

    DT::datatable(
      values$results$summary_by_year,
      options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap"
    ) %>%
      formatRound(c("BA_total_mean", "N_total_mean"), 2) %>%
      formatStyle(columns = names(values$results$summary_by_year),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  output$species_table <- DT::renderDataTable({
    req(values$results)

    DT::datatable(
      values$results$species_year,
      options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap"
    ) %>%
      formatRound(c("BA_total", "N_total"), 2) %>%
      formatStyle(columns = names(values$results$species_year),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  output$dgp_table <- DT::renderDataTable({
    req(values$results)

    DT::datatable(
      values$results$dgp_year,
      options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap"
    ) %>%
      formatRound(c("BA_total", "N_total"), 2) %>%
      formatStyle(columns = names(values$results$dgp_year),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  output$predictions_table <- DT::renderDataTable({
    req(values$results)

    DT::datatable(
      values$results$predictions,
      options = list(pageLength = 10, scrollX = TRUE),
      style = "bootstrap"
    ) %>%
      formatStyle(columns = names(values$results$predictions),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  # Time series plot
  output$time_series_plot <- renderPlotly({
    req(values$results)

    y_title <- if (input$plot_metric == "BA_total_mean") "Basal Area (m2/ha)" else "Tree Density (trees/ha)"

    p <- plot_ly(values$results$summary_by_year,
                 x = ~Year,
                 y = ~get(input$plot_metric),
                 type = 'scatter',
                 mode = 'lines+markers',
                 line = list(color = '#3498db', width = 3),
                 marker = list(color = '#e74c3c', size = 8)) %>%
      layout(
        title = paste("Forest", gsub("_", " ",
                                     gsub("_mean", "", input$plot_metric)),
                      "Over Time"),
        xaxis = list(title = "Year", color = "white",
                     gridcolor = "#4a6572"),
        yaxis = list(title = y_title,
                     type = if (input$log_scale) "log" else "linear",
                     color = "white", gridcolor = "#4a6572"),
        plot_bgcolor = "#1a2530",
        paper_bgcolor = "#2d3e50",
        font = list(color = "white")
      )

    p
  })

  # Species composition plot
  output$species_composition_plot <- renderPlotly({
    req(values$results)

    species_data <- values$results$species_year %>%
      group_by(Year) %>%
      mutate(BA_percent = BA_total / sum(BA_total) * 100)

    p <- plot_ly(species_data,
                 x = ~Year,
                 y = ~BA_percent,
                 color = ~SpeciesGroup,
                 colors = "set3",
                 type = 'scatter',
                 mode = 'lines',
                 stackgroup = 'one') %>%
      layout(
        title = "Species Composition Over Time (% Basal Area)",
        xaxis = list(title = "Year", color = "white", gridcolor = "#4a6572"),
        yaxis = list(title = "Percentage (%)", color = "white", gridcolor = "#4a6572"),
        plot_bgcolor = "#1a2530",
        paper_bgcolor = "#2d3e50",
        font = list(color = "white"),
        legend = list(font = list(color = "white"))
      )

    p
  })

  # Download handlers
  output$download_summary <- downloadHandler(
    filename = function() paste0("forecast_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$results)
      write.csv(values$results$summary_by_year, file, row.names = FALSE)
    }
  )

  output$download_species <- downloadHandler(
    filename = function() paste0("species_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$results)
      write.csv(values$results$species_year, file, row.names = FALSE)
    }
  )

  output$download_dgp <- downloadHandler(
    filename = function() paste0("dgp_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$results)
      write.csv(values$results$dgp_year, file, row.names = FALSE)
    }
  )

  output$download_predictions <- downloadHandler(
    filename = function() paste0("full_predictions_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$results)
      write.csv(values$results$predictions, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
