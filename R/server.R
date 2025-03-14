#' Shiny server logic for the app
#' @export
server <- function(input, output, session) {
  library(shiny)
  library(DBI)
  library(pool)
  library(RMySQL)
  library(dplyr)
  library(glue)
  library(adminKraken)
  
  # Retrieve database credentials
  x <- adminKraken::sharpen(adminKraken::triton())

  # Establish database connection
  db_pool <- dbPool(
    drv = RMySQL::MySQL(),
    dbname = x['dbname'],
    host = x['host'],
    username = x['user'],
    password = x['password'],
    port = 3306
  )

  # Ensure pool is closed when the app stops
  onStop(function() {
    poolClose(db_pool)
  })

  # Fetch all lot data on startup
  lotstuff <- coefs::lots()

  # Update Lot dropdown dynamically
  observe({
    filtered_lots <- lotstuff

    if (input$Type != "All") {
      filtered_lots <- filtered_lots[filtered_lots$LotNumber %in% grep(paste0("^", input$Type), filtered_lots$LotNumber, value = TRUE), ]
    }

    updateSelectInput(session, 'Lot', choices = filtered_lots$LotNumber)
  })

  # Fetch and display data when a lot is selected
  observeEvent(input$Lot, {
    if (!is.null(input$Lot) && input$Lot != "SELECT") {
      BMID <- lotstuff$BMID[lotstuff$LotNumber == input$Lot]

      info <- coefs::fetch(BMID)
      output$Lottable <- renderTable(data.frame(Lot = input$Lot))

      # If checkbox is checked, find all lots with the same BMID
      if (input$show_matching_lots) {
        output$matching_lots_table <- renderTable({
          yourPackageName::get_matching_lots(db_pool, BMID)
        })
      } else {
        output$matching_lots_table <- renderTable(NULL)
      }
    }
  })
}
