
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
  tibble::tibble(
    path = list.files(dir, pattern = "\\.R$", full.names = TRUE, ...),
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

ask <- function(dir = "R", lang = "ja") {

  dct_file <- paste0(lang, ".dct")
  dct_file <- system.file("dct", dct_file, package = .packageName,
                          mustWork = TRUE)
  dct <- yaml::read_yaml(dct_file)

  df_scripts <- list_scripts(dir)

  msg <- c(
    "",
    crayon::underline(crayon::blue(dct$prompt)),
    df_scripts$msg,
    paste(rep("-", 10), collapse = ""),
    paste0(
      sprintf("%2d", 0),
      sprintf(": %s ", dct$quit)
    ),
    ""
  )

  while(TRUE) {

    writeLines(msg)
    selected <- try(as.integer(readline()))

    if (is.na(selected)) {
      message(dct$not_a_number, "\n")
      next()
    }

    if (selected > nrow(df_scripts)) {
      message(sprintf(dct$out_of_range,
                      nrow(df_scripts)))
      next()
    }

    if (selected == 0L) {
      writeLines(c(
        "",
        dct$goodbye1,
        dct$goodbye2,
        "",
        str_glue("   ask(\"{dir}\")"),
        ""
      ))
      break()
    } else {
      message("Running ", df_scripts$path[[selected]])
      source(df_scripts$path[[selected]])
      message("Done.")
    }
  }
}
