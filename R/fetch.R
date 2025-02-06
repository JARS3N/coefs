#' Fetch coefficient data
#' @export
fetch <- function(BMID) {
  if (!is.numeric(BMID)) stop("BMID must be numeric.")

  query <- sprintf("SELECT 
                      CAST(PH_A AS SIGNED) AS PH_A, 
                      CAST(PH_B AS DECIMAL(10,6)) AS PH_B, 
                      CAST(PH_C AS DECIMAL(10,6)) AS PH_C, 
                      CAST(O2_A AS SIGNED) AS O2_A, 
                      CAST(O2_B AS DECIMAL(10,6)) AS O2_B, 
                      CAST(CONCAT(SUBSTR(`Sensor 2 C1`, 1, 4), 'e', 
                      SUBSTR(`Sensor 2 C1`, 5, 6)) AS DECIMAL(3,2)) AS BF 
                    FROM barcodematrixview 
                    WHERE ID = %d", BMID)

  tryCatch({
    conn <- pool::poolCheckout(db_pool)  # Retrieve connection from pool
    
    result <- suppressWarnings(DBI::dbGetQuery(conn, query))  # Execute query with suppressed warnings
    
    pool::poolReturn(conn)  # Return connection to pool

    # Ensure correct types in R
    result <- result %>%
      dplyr::mutate(
        PH_A = as.integer(PH_A),   # Convert PH_A to integer
        O2_A = as.integer(O2_A),   # Convert O2_A to integer
        PH_B = as.double(PH_B),    # Keep PH_B as double
        PH_C = as.double(PH_C),    # Keep PH_C as double
        O2_B = as.double(O2_B),    # Keep O2_B as double
        BF = as.double(BF)         # Keep BF as double
      )

    return(result)
  }, error = function(e) {
    warning("Database query failed: ", e$message)
    return(NULL)
  })
}
