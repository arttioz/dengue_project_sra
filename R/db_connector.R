require(RPostgreSQL)
db_connector <- setRefClass(Class="db_connector",
                            fields = list(
                              driver = "PostgreSQLDriver",
                              credentials = "character",
                              conn = function(conn=NULL) {
                                if (!is.null(conn)) return("no.")
                                return(get_connection())
                              }
                            ),
                            methods = list(
                              initialize = function(credentials, driver=dbDriver("PostgreSQL")) {
                                "Checks for RPostgreSQL, uses credentials to generate a connection and test it."
                                if (!require(RPostgreSQL)) stop("RPostgreSQL required.")
                                driver <<- driver
                                credentials <<- credentials
                                add_connection()
                                check_crd()
                                connection_in_use(get_connection())
                                clean_connections()
                                clear_connections()
                              },
                              get_credentials = function(crd) {
                                rds_credentials <- isTRUE(grepl('\\.rds$', crd))
                                json_credentials <- isTRUE(grepl('\\.json$', crd))
                                if (rds_credentials) {
                                  return(readRDS(crd))
                                } else if (json_credentials) {
                                  return(fromJSON(crd))
                                } else {
                                  stop("Credentials must be a single file in .json or .rds format.")
                                }
                              },
                              check_crd = function(crd=NULL) {
                                "Checks whether credentials are available and complete."
                                if (is.null(crd)) {
                                  crd <- get_credentials(.self$credentials)
                                } else {
                                  crd <- get_credentials(crd)
                                }
                                # strictly speaking, none of these are required.
                                #			required_elements <- c('port','host','user','password','dbname')
                                #			missing <- sapply(required_elements, function(x,crd) !(x %in% crd), crd=names(crd) )
                                #			if (any(missing)) {
                                #				paste0('Credentials list must include elements for:\n ',
                                #					paste('\t',required_elements[missing],sep='',collapse=',\n'))
                                #			}
                                rm(crd)
                                return("Credentials ok.\n")
                              },
                              add_connection = function() {
                                "Adds a connection to the pool if necessary."
                                crd <- c(list(drv=driver), get_credentials(credentials))
                                status <- tryCatch(
                                  expr = do.call(what=dbConnect, args=crd),
                                  error=function(e) return("FAIL"))
                                rm(crd)
                                if (identical(status, "FAIL")) {
                                  dbListTables(dbListConnections(driver)[[length(dbListConnections(driver))]])
                                  warning(paste0(
                                    "Was not able to allocate a new connection, returned\n",
                                    "connection has lost its previously pending result."))
                                }
                                connections <- dbListConnections(driver)
                                if (length(connections) == 0)
                                  stop("No connections available.  Check your credentials.")
                                return(connections[[length(connections)]])
                              },
                              get_connection=function() {
                                "Yields an unused connection from the pool."
                                if (identical(list(),dbListConnections(driver)))
                                  add_connection()
                                for ( i in 1:length(dbListConnections(driver)) ) {
                                  connection <- dbListConnections(driver)[[i]]
                                  if (!connection_in_use(connection)) {
                                    return(connection)
                                  } 
                                }
                                add_connection()
                                return(dbListConnections(driver)[[length(dbListConnections(driver))]])
                              },
                              connection_in_use = function(connection=NULL) {
                                "Tests whether a connection is in use."
                                if (is.null(connection)) return(NA)
                                temp_result <- tryCatch(
                                  expr=dbSendQuery(connection,"SELECT * FROM generate_series(1,2);"),
                                  error=function(e) return("FAIL")
                                )
                                test <- (identical(temp_result,"FAIL")) 
                                if (!test) dbClearResult(temp_result)
                                return(test)
                              },
                              clean_connections = function() {
                                "Cleans out available connections."
                                for (i in 1:length(dbListConnections(driver))) {
                                  if (!isPostgresqlIdCurrent(dbListConnections(driver)[[i]])) {
                                    dbDisconnect(dbListConnections(driver)[[i]])
                                  }
                                }
                              },
                              clear_connections = function() {
                                "Runs a simple command to clear pending results on available connections."
                                lapply(dbListConnections(driver),dbListTables)
                                return(TRUE)
                              }
                            )
)
