ui <- fluidPage(
  useShinyjs(),
  titlePanel("Interactive Point Selection from Map"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      actionButton("add_button", "Add Selected Points", class = "btn-primary"),
      actionButton("drop_button", "Drop Selected Rows", class = "btn-danger"),
      downloadButton("download_button", "Download Table"),
      br(), br(),
      h4("Instructions:"),
      p("1. Upload a CSV file with coordinates (lat/lon)"),
      p("2. Click on individual points or use rectangle tool to select multiple points"),
      p("3. Click 'Add Selected Points' to add them to the table"),
      p("4. Select rows in the table and click 'Drop Selected Rows' to remove them"),
      p("5. Use 'Download Table' to save the selected PlotIDs")
    ),

    mainPanel(
      width = 9,
      leafletOutput("map", height = "500px"),
      br(),
      h4("Selected Points:"),
      DTOutput("selected_table")
    )
  )
)
