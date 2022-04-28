#' @title Get (Postgres) Connection
#' @description
#' Aux function to retrieve env variables and create a pqConn connection.
#' @param env character(1) The user to use for the connection.
#' @param user character(1) The environment to connect to.
get_db_connection <- function(env = "staging", user = "postgres") {
  assert_choice(env, c("prod", "staging"))
  assert_choice(user, c("arrival", "postgres"))

  items <- c("PGUSER", "PGPASSWORD", "PGPORT", "PGHOST", "PGDATABASE")
  items_full <- sprintf("%s_%s_%s", items, toupper(user), toupper(env))

  creds <- lapply(items_full, Sys.getenv)
  names(creds) <- items

  if (any(creds == "")) {
    missing <- sprintf(items_full[creds == ""])
    stop(sprintf(
      "The following env variables are not found/defined:\n - %s",
      paste(missing, collapse = "\n - ")
    ))
  }

  with(creds, {
    db <<- dbConnect(
      drv = Postgres(),
      host = PGHOST,
      user = PGUSER,
      password = PGPASSWORD,
      port = PGPORT,
      dbname = PGDATABASE
    )
  })
  return(db)
}

