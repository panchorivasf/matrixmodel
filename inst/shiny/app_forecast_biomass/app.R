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
  dashboardHeader(title = "Matrix Model Forest Forecast"),

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
            title = "Data Input",
            status = "primary",
            solidHeader = TRUE,
            width = 6,

            fileInput("data_file",
                      "Upload Data File (.csv)",
                      accept = c(".csv")),

            selectizeInput("plot_id",
                           "Select Plot ID:",
                           choices = NULL,
                           options = list(
                             placeholder = 'Select or type plot ID',
                             maxOptions = 100
                           ))
          ),

          box(
            title = "Simulation Parameters",
            status = "info",
            solidHeader = TRUE,
            width = 6,

            numericInput("years",
                         "Simulation Years",
                         value = 50,
                         min = 1,
                         max = 200,
                         step = 1),

            checkboxInput("clear_start",
                          "Start with clearcut",
                          value = FALSE),

            conditionalPanel(
              condition = "!input.clear_start",
              numericInput("clear_year",
                           "Clearcut Year (optional)",
                           value = NA,
                           min = 1,
                           max = 200)
            ),

            checkboxInput("allow_colonization",
                          "Allow species colonization",
                          value = TRUE)
          )
        ),

        fluidRow(
          box(
            title = "Planting Configuration",
            status = "warning",
            solidHeader = TRUE,
            width = 6,

            conditionalPanel(
              condition = "input.clear_start",
              fileInput("plant_init",
                        "Initial Plantation File (.csv)",
                        accept = c(".csv")),
              helpText("CSV should have columns: SPCD, TPH")
            ),

            conditionalPanel(
              condition = "input.clear_year",
              fileInput("plant_post",
                        "Post-clearcut Plantation File (.csv)",
                        accept = c(".csv")),
              helpText("CSV should have columns: SPCD, TPH")
            )

          ),

          box(
            title = "Output Settings",
            status = "success",
            solidHeader = TRUE,
            width = 6,

            textInput("output_folder",
                      "Output Folder Name",
                      value = "simulation_output"),

            verbatimTextOutput("output_location_info"),

            helpText("Files will be saved in your current working directory"),

            # br(),
            # actionButton("run_forecast",
            #              "Run Forest Forecast",
            #              class = "btn-primary btn-lg",
            #              icon = icon("play"))
          ),

          # box(
          #   status = "success",
          #   solidHeader = FALSE,
          #   with = 6,

            actionButton("run_forecast",
                         "Run Forest Forecast",
                         class = "btn-primary btn-lg",
                         icon = icon("play")) ,
          br()
          # ),
        ),



        fluidRow(
          box(
            title = "Data Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,  # Show by default so users can see their data

            DT::dataTableOutput("data_preview")
          )
        ),

        fluidRow(
          box(
            title = "Progress & Status",
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
            title = "Forecast Results",
            width = 12,

            tabPanel("Summary by Year",
                     DT::dataTableOutput("summary_table")),

            tabPanel("Species by Year",
                     DT::dataTableOutput("species_table")),

            tabPanel("DBH Groups by Year",
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
            title = "Forest Metrics Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            fluidRow(
              column(6,
                     selectInput("plot_metric",
                                 "Select Metric:",
                                 choices = list("Basal Area" = "BA_total",
                                                "Tree Density" = "N_total"),
                                 selected = "BA_total")
              ),
              column(6,
                     checkboxInput("log_scale", "Use Log Scale", value = FALSE)
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
              column(3, downloadButton("download_dgp", "DBH Groups CSV", class = "btn-info")),
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

  options(shiny.maxRequestSize = 100 * 1024^2)  # 100MB max file size

  # Reactive values to store results
  values <- reactiveValues(
    data = NULL,
    results = NULL,
    progress = 0,
    planting_init = NULL,
    planting_post = NULL
  )

  # Check if models are loaded - removed problematic reactive output

  # Load main data file
  observeEvent(input$data_file, {
    req(input$data_file)

    tryCatch({
      values$data <- read.csv(input$data_file$datapath)

      # Find PlotID column
      plot_cols <- c("PlotID", "plot_id", "Plot_ID", "plot", "Plot", "ID", "id")
      plot_col <- intersect(plot_cols, names(values$data))

      if (length(plot_col) > 0) {
        plot_ids <- unique(values$data[[plot_col[1]]])
        plot_ids <- sort(plot_ids[!is.na(plot_ids) & plot_ids != ""])

        updateSelectizeInput(session, "plot_id",
                             choices = plot_ids,
                             selected = if(length(plot_ids) > 0) plot_ids[1] else "")
      }

      showNotification("Data loaded successfully!", type = "message")

    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      values$data <- NULL
    })
  })

  # Load planting files
  observeEvent(input$plant_init, {
    req(input$plant_init)
    tryCatch({
      values$planting_init <- read.csv(input$plant_init$datapath)
      if (!all(c("SPCD", "TPH") %in% names(values$planting_init))) {
        stop("Planting file must have SPCD and TPH columns")
      }
      showNotification("Initial planting file loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading initial planting:", e$message), type = "error")
      values$planting_init <- NULL
    })
  })

  observeEvent(input$plant_post, {
    req(input$plant_post)
    tryCatch({
      values$planting_post <- read.csv(input$plant_post$datapath)
      if (!all(c("SPCD", "TPH") %in% names(values$planting_post))) {
        stop("Planting file must have SPCD and TPH columns")
      }
      showNotification("Post-clearcut planting file loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading post-clearcut planting:", e$message), type = "error")
      values$planting_post <- NULL
    })
  })

  # Show output location
  output$output_location_info <- renderText({
    output_path <- file.path(getwd(), input$output_folder)
    paste("Output will be saved to:\n", output_path)
  })

  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$data)

    DT::datatable(
      head(values$data, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ltip'
      ),
      style = "bootstrap",
      class = "table-bordered table-condensed"
    ) %>%
      formatStyle(columns = names(values$data),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  # Main forecast execution
  observeEvent(input$run_forecast, {
    req(values$data, input$plot_id)

    # Check if models are available
    if (!exists("models_loaded") || !models_loaded()) {
      showNotification("Models not loaded. Please ensure the forecast_biomass function and models are available.",
                       type = "error")
      return()
    }

    # Reset progress
    values$progress <- 0
    updateProgressBar(session, "forecast_progress", value = 0)

    tryCatch({
      output$progress_text <- renderText({"Starting forecast simulation..."})
      values$progress <- 10
      updateProgressBar(session, "forecast_progress", value = 10)

      # Prepare parameters exactly as the function expects them
      clear_year_param <- if (is.na(input$clear_year)) NULL else input$clear_year

      values$progress <- 30
      updateProgressBar(session, "forecast_progress", value = 30)
      output$progress_text <- renderText({"Running forecast model..."})

      # Call the forecast_biomass function with proper parameters
      values$results <- forecast_biomass(
        save_to = getwd(),
        data = values$data,
        plot_id = input$plot_id,
        years = input$years,
        clear_start = input$clear_start,
        clear_year = clear_year_param,
        planting_init = values$planting_init,
        planting_post = values$planting_post,
        allow_colonization = input$allow_colonization,
        minimal_if_clearcut = TRUE,
        m_model = NULL,  # Use package defaults
        u_model = NULL,  # Use package defaults
        r_model = NULL,  # Use package defaults
        output_folder_name = input$output_folder
      )

      values$progress <- 100
      updateProgressBar(session, "forecast_progress", value = 100)
      output$progress_text <- renderText({
        paste("Forecast completed successfully for Plot:", values$results$plot_id)
      })

      showNotification("Forecast completed successfully!", type = "message")
      updateTabItems(session, "tabs", "results")

    }, error = function(e) {
      showNotification(paste("Forecast error:", e$message), type = "error")
      values$progress <- 0
      updateProgressBar(session, "forecast_progress", value = 0)
      output$progress_text <- renderText({paste("Error:", e$message)})
    })
  })

  # Progress text output
  output$progress_text <- renderText({
    if (values$progress == 0) {
      "Ready to run forecast. Upload data and click 'Run Forest Forecast' to begin."
    } else if (values$progress < 100) {
      "Processing forecast... Please wait."
    } else if (!is.null(values$results)) {
      paste("Forecast completed for Plot:", values$results$plot_id)
    } else {
      "Ready to run forecast."
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
    ba_val <- if (!is.null(values$results) && !is.null(values$results$summary)) {
      final_ba <- tail(values$results$summary$BA_total, 1)
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

  # Results tables - fixed to match actual function output
  output$summary_table <- DT::renderDataTable({
    req(values$results$summary)

    DT::datatable(
      values$results$summary,
      options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap"
    ) %>%
      formatRound(c("BA_total", "N_total"), 2) %>%
      formatStyle(columns = names(values$results$summary),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  output$species_table <- DT::renderDataTable({
    req(values$results$species_year)

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
    req(values$results$dgp_year)

    DT::datatable(
      values$results$dgp_year,
      options = list(pageLength = 15, scrollX = TRUE),
      style = "bootstrap"
    ) %>%
      formatRound(c("BA_total", "N_total"), 2) %>%
      formatStyle(columns = names(values$results$dgp_year),
                  backgroundColor = "#34495e", color = "#ffffff")
  })


  # Data preview with clickable rows
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

  # Handle table row clicks to select plot ID
  observeEvent(input$data_preview_cell_clicked, {
    click <- input$data_preview_cell_clicked
    req(click, values$data, click$row > 0)

    if (click$row <= nrow(values$data)) {
      row_data <- values$data[click$row, ]

      # Find plot ID column
      plot_cols <- c("PlotID", "plot_id", "Plot_ID", "plot", "Plot", "ID", "id")
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

  # Time series plot
  output$time_series_plot <- renderPlotly({
    req(values$results$summary)

    y_title <- if (input$plot_metric == "BA_total") "Basal Area (m2/ha)" else "Tree Density (trees/ha)"

    p <- plot_ly(values$results$summary,
                 x = ~Year,
                 y = ~get(input$plot_metric),
                 type = 'scatter',
                 mode = 'lines+markers',
                 line = list(color = '#3498db', width = 3),
                 marker = list(color = '#e74c3c', size = 8)) %>%
      layout(
        title = paste("Forest", gsub("_", " ", input$plot_metric), "Over Time"),
        xaxis = list(title = "Year", color = "white", gridcolor = "#4a6572"),
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
    req(values$results$species_year)

    species_data <- values$results$species_year %>%
      group_by(Year) %>%
      mutate(BA_percent = BA_total / sum(BA_total, na.rm = TRUE) * 100)

    p <- plot_ly(species_data,
                 x = ~Year,
                 y = ~BA_percent,
                 color = ~SpeciesGroup,
                 colors = "Set3",
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
      req(values$results$summary)
      write.csv(values$results$summary, file, row.names = FALSE)
    }
  )

  output$download_species <- downloadHandler(
    filename = function() paste0("species_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$results$species_year)
      write.csv(values$results$species_year, file, row.names = FALSE)
    }
  )

  output$download_dgp <- downloadHandler(
    filename = function() paste0("dgp_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$results$dgp_year)
      write.csv(values$results$dgp_year, file, row.names = FALSE)
    }
  )

  output$download_predictions <- downloadHandler(
    filename = function() paste0("full_predictions_", Sys.Date(), ".csv"),
    content = function(file) {
      req(values$results$predictions)
      write.csv(values$results$predictions, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
