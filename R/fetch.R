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
    suppressWarnings({
      result <- DBI::dbGetQuery(db_pool, query)

      # Convert correct data types in R
      result <- result %>%
        dplyr::mutate(
          PH_A = as.integer(PH_A),
          O2_A = as.integer(O2_A),
          PH_B = as.double(PH_B),
          PH_C = as.double(PH_C),
          O2_B = as.double(O2_B),
          BF = as.double(BF)
        )

      return(result)
    })
  }, error = function(e) {
    warning("Database query failed: ", e$message)
    return(NULL)
  })
}
