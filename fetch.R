#' fetch coef data
#' @export
fetch <- function(BMID) {
  if (!is.numeric(BMID)) stop("BMID must be numeric.")

  query <- "SELECT PH_A, PH_B, PH_C, O2_A, O2_B,
            CAST(CONCAT(SUBSTR(`Sensor 2 C1`, 1, 4), 'e',
            SUBSTR(`Sensor 2 C1`, 5, 6)) AS DECIMAL(3,2)) AS BF
            FROM barcodematrixview WHERE ID = ?id"

  safe_query <- DBI::sqlInterpolate(db_pool, query, id = BMID)  # ✅ Safe interpolation

  tryCatch({
    result <- DBI::dbGetQuery(db_pool, safe_query)
    return(result)
  }, error = function(e) {
    warning("Database query failed: ", e$message)
    return(NULL)
  })
}
