#' Retrieve lot information
#' @export
lots <- function() {
  query <- "SELECT CONCAT(Type, `Lot Number`) AS LotNumber, `Barcode Matrix ID` AS BMID FROM lotview;"
  tryCatch({
    DBI::dbGetQuery(db_pool, query)
  }, error = function(e) {
    warning("Database query failed: ", e$message)
    return(NULL)
  })
}
