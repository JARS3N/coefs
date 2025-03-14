#' Shiny server logic
#' @export
server <- function(input, output, session) {
  library(shiny)
library(DBI)
library(pool)
library(RMySQL)
library(dplyr)
library(glue)

# Function to retrieve lots that share the same BMID
get_matching_lots <- function(db_pool, bmid) {
  if (missing(bmid) || is.null(bmid)) {
    warning("Invalid BMID provided.")
    return(data.frame(Error = "Invalid BMID"))
  }
  
  query <- sprintf(
    "SELECT CONCAT(`Type`, `Lot Number`) AS LotID
     FROM lotview
     WHERE `Barcode Matrix ID` = '%s'
     LIMIT 100;",
    bmid
  )
  
  cat("Executing Query:", query, "\n")  # Debugging output
  
  tryCatch({
    matching_lots <- DBI::dbGetQuery(db_pool, query)
    
    if (nrow(matching_lots) == 0) {
      return(data.frame(Message = "No other lots with this BMID"))
    }
    
    return(matching_lots)
  }, error = function(e) {
    warning("Database query failed: ", e$message)
    return(data.frame(Error = "Failed to retrieve matching lots"))
  })
}

# Define server logic
server <- function(input, output, session) {
  # Retrieve database credentials from adminKraken
  x <- adminKraken::sharpen(triton())
  
  # Establish database connection using pool
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
  
  # Update Lot selectInput dynamically with filtering options
  observe({
    filtered_lots <- lotstuff
    
    # Apply Type filter if selected
    if (input$Type != "All") {
      filtered_lots <- filtered_lots[filtered_lots$LotNumber %in% grep(paste0("^", input$Type),
                                                                       filtered_lots$LotNumber,
                                                                       value = TRUE), ]
    }
    
    # Apply Year filter if entered
    if (nchar(input$Year) == 2 && grepl("^[0-9]{2}$", input$Year)) {
      year_value <- as.numeric(input$Year)
      lot_years <- as.numeric(substr(
        filtered_lots$LotNumber,
        nchar(filtered_lots$LotNumber) - 1,
        nchar(filtered_lots$LotNumber)
      ))
      
      if (input$YearFilter == "=") {
        filtered_lots <- filtered_lots[lot_years == year_value, ]
      } else if (input$YearFilter == "≥") {
        filtered_lots <- filtered_lots[lot_years >= year_value, ]
      } else if (input$YearFilter == "≤") {
        filtered_lots <- filtered_lots[lot_years <= year_value, ]
      }
    }
    
    # Exclude Experimental Lots unless checkbox is checked
    if (!input$include_experimental) {
      filtered_lots <- filtered_lots[!grepl("^.E", filtered_lots$LotNumber), ]
    }
    
    updateSelectInput(session, 'Lot', choices = filtered_lots$LotNumber)
  })
  
  # Fetch and display data when a lot is selected
  observeEvent(input$Lot, {
    if (!is.null(input$Lot) && input$Lot != "SELECT") {
      BMID <- lotstuff$BMID[lotstuff$LotNumber == input$Lot]
      
      info <- coefs::fetch(BMID)  # Fetch data using BMID
      
      # Display selected Lot
      output$Lottable <- renderTable(data.frame(Lot = input$Lot))
      
      # Display pH data
      output$pHtable <- renderTable({
        dplyr::select(info, dplyr::contains("PH")) %>%
          dplyr::mutate(PH_A = as.character(round(PH_A, 0)))
      }, digits = 6)
      
      # Display O2 data
      output$oxtable <- renderTable({
        dplyr::select(info, dplyr::contains("O2")) %>%
          dplyr::mutate(O2_A = as.character(round(O2_A, 0)))
      }, digits = 6)
      
      # Display Buffer Factor table
      output$bftbl <- renderTable({
        if (info$BF == 0) {
          data.frame(Cartridge_BufferFactor = NA)
        } else {
          dplyr::select(info, Cartridge_BufferFactor = BF)
        }
      }, digits = 6)
      
      # If checkbox is checked, find all lots with the same BMID
      if (input$show_matching_lots) {
        output$matching_lots_table <- renderTable({
          get_matching_lots(db_pool, BMID)
        })
      } else {
        output$matching_lots_table <- renderTable(NULL)
      }
    }
  })
}

}
