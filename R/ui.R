
get_dcf <- function() {
  lang <- getOption("omukyomu.lang", "ja")

  dcf_file <- system.file("translation", paste0(lang, ".dcf"),
                          package = .packageName,
                          mustWork = TRUE)
  calling_command <- deparse(sys.calls()[[sys.nframe() - 1]])
  caller <- str_extract(calling_command, "(.+)\\(", 1)
  yaml::read_yaml(dcf_file)[[caller]]
}

ui_instruction <- function(x) {
  writeLines(crayon::underline(crayon::blue(x)))
}
