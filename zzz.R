.onLoad <- function(libname, pkgname) {
  message("Loading coefs package...")

  # Ensure required packages are available
  if (!requireNamespace("pool", quietly = TRUE)) stop("Package 'pool' is required but not installed.")
  if (!requireNamespace("DBI", quietly = TRUE)) stop("Package 'DBI' is required but not installed.")
  if (!requireNamespace("RMySQL", quietly = TRUE)) stop("Package 'RMySQL' is required but not installed.")
  # load pipe operator
  utils::globalVariables(c("%>%"))
  # Assign global database connection pool
  db_pool <<- pool::dbPool(
    drv = RMySQL::MySQL(),
    dbname = adminKraken::sharpen(adminKraken::triton())["dbname"],
    host = adminKraken::sharpen(adminKraken::triton())["host"],
    user = adminKraken::sharpen(adminKraken::triton())["user"],
    password = adminKraken::sharpen(adminKraken::triton())["password"]
  )
}

.onUnload <- function(libpath) {
  message("Unloading coefs package...")
  if (exists("db_pool", envir = globalenv())) {
    pool::poolClose(db_pool)
    rm("db_pool", envir = globalenv())
  }
}
