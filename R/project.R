project_root <- function() {
  root <- try(
    rprojroot::find_root(rprojroot::has_file("config.yml")),
    silent = TRUE
  )

  if (inherits(root, "try-error")) {
    root <- NA_character_
  }
  root
}

#' Create kyomu project folder
#'
#' @param name character. Name of the directory
#' @param parent_dir character. Name of the parent directory.
#'
#' @return logical. TRUE when success.
#' @export
#'
project_create <- function(name = "econkyomu", parent_dir = NULL) {
  dcf <- get_dcf()

  if (is.null(parent_dir)) {
    usethis::ui_todo(dcf$`parent-dir`)
    Sys.sleep(1)
    parent_dir <- try(choose_dir(), silent = TRUE)
    if (length(parent_dir) == 0) {
      usethis::ui_info(dcf$cancelled)
      return(invisible(FALSE))
    }
  }

  if (name %in% list.files(parent_dir)) {
    usethis::ui_oops(str_glue("{name} {dcf$`dir-exists`}"))
    usethis::ui_info(dcf$cancelled)
    return(invisible(FALSE))
  }

  file.copy(system.file("econkyomu", package = .packageName),
            to = parent_dir, overwrite = FALSE, recursive = TRUE)
  cat("Created by omukyomu package.",
      file = file.path(parent_dir, name, ".here"))

  usethis::ui_done(dcf$`complete-creation`)
  setwd(file.path(parent_dir, name))
  usethis::ui_done(dcf$`complete-setwd`)

  if (rstudioapi::isAvailable()) {
    usethis::ui_info(dcf$`advice-rstudio`)
  } else{
    usethis::ui_info(dcf$`advice-r`)
  }

  invisible(TRUE)
}


project_choose <- function() {
  dcf <- get_dcf()

  dir <- try(choose_dir(), silent = TRUE)
  if (length(dir) == 0) {
    usethis::ui_info(dcf$cancelled)
    return(invisible(FALSE))
  }

  setwd(dir)
  usethis::ui_done(dcf$moved)

  invisible(TRUE)
}

project_ensure <- function(){
  dcf <- get_dcf()

  if (!is.na(root <- project_root())) {
    return(root)
  }

  usethis::ui_oops(dcf$`not-in-project`)

  num_selected <- utils::menu(
    c(dcf$`create`, dcf$`choose`, dcf$`cancel`)
  )

  if (num_selected == 1) {
    project_create()

  } else if (num_selected == 2) {
    project_choose()
  } else {
    usethis::ui_info(dcf$`cancelled`)
    rlang::interrupt()
  }

  project_root()
}

