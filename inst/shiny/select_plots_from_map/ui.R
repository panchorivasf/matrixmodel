ui <- fluidPage(
  shinyjs::useShinyjs(),
  shiny::titlePanel("Interactive Point Selection from Map"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
      shiny::actionButton("add_button", "Add Selected Points", class = "btn-primary"),
      shiny::actionButton("drop_button", "Drop Selected Rows", class = "btn-danger"),
      shiny::downloadButton("download_button", "Download Table"),
      shiny::br(), br(),
      shiny::h4("Instructions:"),
      shiny::p("1. Upload a CSV file with coordinates (lat/lon)"),
      shiny::p("2. Click on individual points or use rectangle tool to select multiple points"),
      shiny::p("3. Click 'Add Selected Points' to add them to the table"),
      shiny::p("4. Select rows in the table and click 'Drop Selected Rows' to remove them"),
      shiny::p("5. Use 'Download Table' to save the selected PlotIDs")
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
