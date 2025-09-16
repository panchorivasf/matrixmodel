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

            textInput("plot_id",
                      "Plot ID",
                      value = "",
                      placeholder = "Enter plot identifier"),

            numericInput("years",
                         "Simulation Years",
                         value = 50,
                         min = 1,
                         max = 100,
                         step = 5),

            textInput("output_folder",
                      "Output Folder Name",
                      value = "simulation_output"),

            br(),

            actionButton("run_forecast",
                         "Run Forecast",
                         class = "btn-primary btn-lg",
                         icon = icon("play"))
          ),

          box(
            title = "Model Files",
            status = "info",
            solidHeader = TRUE,
            width = 6,

            textInput("m_model",
                      "Mortality Model Path",
                      value = "./models/model_mortality.rds"),

            textInput("u_model",
                      "Upgrowth Model Path",
                      value = "./models/model_upgrowth.rds"),

            textInput("r_model",
                      "Recruitment Model Path",
                      value = "./models/model_recruitment.rds"),

            textInput("save_to",
                      "Output Directory",
                      value = "",
                      placeholder = "Leave empty for current directory")
          )
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

  output$data_preview <- DT::renderDataTable({
    req(values$data)

    DT::datatable(
      head(values$data, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'ltip',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      style = "bootstrap",
      class = "table-bordered table-condensed"
    ) %>%
      formatStyle(columns = names(values$data),
                  backgroundColor = "#34495e", color = "#ffffff")
  })

  # Run forecast
  observeEvent(input$run_forecast, {
    req(values$data)

    if (input$plot_id == "") {
      showNotification("Please enter a Plot ID", type = "error")
      return()
    }

    # Reset progress
    updateProgressBar(session, "forecast_progress", value = 0)
    values$progress <- 0

    # Simulate progress updates
    progress_timer <- reactiveTimer(500)

    observe({
      progress_timer()
      if (values$progress < 90 && !is.null(values$results)) {
        values$progress <- min(values$progress + 10, 90)
        updateProgressBar(session, "forecast_progress", value = values$progress)
      }
    })

    tryCatch({
      showNotification("Starting forecast simulation...", type = "message")

      # Prepare parameters
      save_path <- if (input$save_to == "") NULL else input$save_to

      # Run the forecast
      values$results <- project_biomass(
        save_to = save_path,
        data = values$data,
        plot_id = input$plot_id,
        years = input$years,
        m_model = input$m_model,
        u_model = input$u_model,
        r_model = input$r_model,
        output_folder_name = input$output_folder
      )

      # Complete progress
      values$progress <- 100
      updateProgressBar(session, "forecast_progress", value = 100)

      showNotification("Forecast completed successfully!", type = "success")

      # Switch to results tab
      updateTabItems(session, "tabs", "results")

    }, error = function(e) {
      showNotification(paste("Forecast error:", e$message), type = "error")
      values$progress <- 0
      updateProgressBar(session, "forecast_progress", value = 0)
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
        title = paste("Forest", gsub("_", " ", gsub("_mean", "", input$plot_metric)), "Over Time"),
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
