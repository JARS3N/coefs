#' server function
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

  # Fetch all lot data once on startup (No repeated DB queries)
  lotstuff <- coefs::lots()

  # ✅ **Update Lot dropdown when filters change (NO new DB query)**
  observe({
    filtered_lots <- lotstuff  # Work with in-memory data

    # Apply Type filter
    if (input$Type != "All") {
      filtered_lots <- filtered_lots[grep(paste0("^", input$Type), filtered_lots$LotNumber), ]
    }

    # Apply Year filter (last two digits of Lot Number)
    if (nchar(input$Year) == 2 && grepl("^[0-9]{2}$", input$Year)) {
      year_value <- as.numeric(input$Year)
      lot_years <- as.numeric(substr(filtered_lots$LotNumber, nchar(filtered_lots$LotNumber) - 1, nchar(filtered_lots$LotNumber)))

      if (input$YearFilter == "=") {
        filtered_lots <- filtered_lots[lot_years == year_value, ]
      } else if (input$YearFilter == "≥") {
        filtered_lots <- filtered_lots[lot_years >= year_value, ]
      } else if (input$YearFilter == "≤") {
        filtered_lots <- filtered_lots[lot_years <= year_value, ]
      }
    }

    # Apply Experimental Lots filter
    if (!input$include_experimental) {
      filtered_lots <- filtered_lots[!grepl("^.E", filtered_lots$LotNumber), ]
    }

    # Debugging Output
    cat("Updated Lot Dropdown:\n")
    print(filtered_lots$LotNumber)

    # Update Lot dropdown (without hitting the DB)
    updateSelectInput(session, 'Lot', choices = filtered_lots$LotNumber)
  })

  # ✅ **Trigger updates when Lot is selected OR checkbox is toggled**
  observeEvent(c(input$Lot, input$show_matching_lots), {
    if (!is.null(input$Lot) && input$Lot != "SELECT") {
      BMID <- lotstuff$BMID[lotstuff$LotNumber == input$Lot]

      # Debugging output
      cat("Selected BMID:", BMID, "\n")
      cat("Checkbox State:", input$show_matching_lots, "\n")

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

      # ✅ **Trigger update when checkbox is checked/unchecked**
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
