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
      filtered_lots <- filtered_lots[grep(paste0("^", input$Type), filtered_lots$LotNumber), ]
    }

    # Debugging: Print filtered lots
    cat("Filtered Lots:\n")
    print(filtered_lots)

    updateSelectInput(session, 'Lot', choices = filtered_lots$LotNumber)
  })

  # Fetch and display data when a lot is selected
  observeEvent(input$Lot, {
    if (!is.null(input$Lot) && input$Lot != "SELECT") {
      BMID <- lotstuff$BMID[lotstuff$LotNumber == input$Lot]

      # Debugging: Print BMID
      cat("Selected BMID:", BMID, "\n")

      info <- coefs::fetch(BMID)

      # Debugging: Print fetched data
      cat("Fetched Data for BMID:", BMID, "\n")
      print(info)

      output$Lottable <- renderTable(data.frame(Lot = input$Lot))

      output$pHtable <- renderTable({
        if (is.null(info) || nrow(info) == 0) {
          return(data.frame(Message = "No data available"))
        }
        dplyr::select(info, dplyr::contains("PH")) %>%
          dplyr::mutate(PH_A = as.character(round(PH_A, 0)))
      }, digits = 6)

      output$oxtable <- renderTable({
        if (is.null(info) || nrow(info) == 0) {
          return(data.frame(Message = "No data available"))
        }
        dplyr::select(info, dplyr::contains("O2")) %>%
          dplyr::mutate(O2_A = as.character(round(O2_A, 0)))
      }, digits = 6)

      output$bftbl <- renderTable({
        if (is.null(info) || nrow(info) == 0 || is.na(info$BF)) {
          return(data.frame(Cartridge_BufferFactor = "No Data"))
        }
        dplyr::select(info, Cartridge_BufferFactor = BF)
      }, digits = 6)

      # If checkbox is checked, find all lots with the same BMID
      if (input$show_matching_lots) {
        output$matching_lots_table <- renderTable({
          coefs::get_matching_lots(db_pool, BMID)
        })
      } else {
        output$matching_lots_table <- renderTable(NULL)
      }
    }
  })
}
