library(PivotalR)

# Greenplum host address
greenplum_host <- "192.168.1.100"


# Connect Greenplum
connectGreenplum <- function(database_name = "", user = "", password = "") {
  
  connection_result <- list(conn_id = NULL, msg = "")
  
  tryCatch({
    # Connect greenplum database
    conn_id <- db.connect(host = greenplum_host, user = user, 
                          password = password, dbname = database_name,
                          default.schema= "public, madlib", verbose = FALSE)
    connection_result$conn_id <- conn_id
    connection_result$msg <- "Connection okay."
  }, warning = function(war) {
    war <- as.character(war)
    connection_result$msg <<- war
  }, error = function(err) {
    err <- as.character(err)
    connection_result$msg <<- err
  })
  
  return(connection_result)
  
}


# Add data frame into greenplum database
addTable <- function(conn_id = NULL, db_object = NULL, table_name = "") {
  
  adding_result <- list(summary = NULL, msg = "")
  
  tryCatch({
    gp_df <- as.db.data.frame(conn.id = conn_id, x = db_object, 
                              table.name = table_name, verbose = FALSE)
    # adding_result$summary <- capture.output({madlib.summary(gp_df, interactive = F)})
    adding_result$msg <- "Import okay."
    adding_result$succ <- TRUE
  }, error = function(err) {
    err <- as.character(err)
    adding_result$msg <- err
    adding_result$succ <- FALSE
  })
  
  return(adding_result)
  
}
