file_exists <- function(file) {
  # check one file at a time
  !is.null(file) && file.exists(file)
}


file_find <- function(file, configkey, instruction, wait = 2) {

  if (file_exists(file))
    return(file)

  file <- config::get(configkey)
  if (file_exists(file))
    return(file)

  # Choose and copy if missing.
  usethis::ui_todo(instruction)
  Sys.sleep(wait)
  file <- file.choose()

  file.copy(file, config::get(configkey))

  file
}


