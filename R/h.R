extract_description <- function(file, pattern = "^#\\|\\s+(.+)") {
  extract_one <- function(f) {
    . <- NULL

    .f <- readLines(f)
    .f %>%
      str_detect(pattern) %>%
      `[`(.f, .) %>%
      str_match(pattern) %>%
      (function(x) x[, 2])() %>%
      str_flatten(collapse = " ") %>%
      str_trim()
  }
  map_vec(file, extract_one)
}

list_scripts <- function(dir, ...) {
  dcf <- get_dcf()
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE, ...)
  if (length(files) == 0) {
    ui_oops(dcf$nothing)
    rlang::interrupt()
  }

  tibble::tibble(
    path = files,
    description = extract_description(.data$path)
  ) %>%
    dplyr::mutate(basename = basename(.data$path)) %>%
    dplyr::arrange(.data$basename) %>%
    dplyr::mutate(id = dplyr::row_number(),
                  msg = paste0(
                    sprintf("%2d", .data$id),
                    sprintf(": %s\n     %s", .data$basename,
                            crayon::magenta(.data$description))
                  ))
}

#' Help to run R script
#'
#' @param dir character. Directory path for which R scripts are searched for.
#'
#' @return NULL
#' @export
#'
h <- function(dir = "R") {
  dcf <- get_dcf()
  df_scripts <- list_scripts(dir)

  while(TRUE) {

    ui_instruction(dcf$prompt)

    msg <- c(
      "",
      df_scripts$msg,
      paste(rep("-", 10), collapse = ""),
      paste0(
        sprintf("%2d", 0),
        sprintf(": %s ", dcf$quit)
      ),
      ""
    )

    writeLines(msg)
    selected <- try(as.integer(readline()))

    if (is.na(selected)) {
      message(dcf$not_a_number, "\n")
      next()
    }

    if (selected > nrow(df_scripts)) {
      message(sprintf(dcf$out_of_range,
                      nrow(df_scripts)))
      next()
    }

    break()
  }

  if (selected == 0L) {
    writeLines(c(
      "",
      dcf$goodbye1,
      dcf$goodbye2,
      "",
      str_glue("   h(\"{dir}\")"),
      ""
    ))
  } else {
    usethis::ui_todo("{dcf$instruction}")
    usethis::ui_code_block("source(\"{df_scripts$path[[selected]]}\")")
  }

  invisible()
}

