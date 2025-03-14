#' Retrieve lots with the same BMID
#' @export
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
