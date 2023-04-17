#' Link page resources on Moodle site and faculty ID
#'
#' @param zemi_html character. Path to the HTML file.
#' @param faculty_xlsx character. Path to the faculty xlsx file.
#' @param name_course character. Course name.
#' @param project_id character. Project id.
#'
#' @return matched result as tibble.
#' @export
#'
moodle_shinsei <- function(zemi_html = NULL,
                           faculty_xlsx = NULL,
                           name_course = "", project_id = "") {

  project_ensure()
  dcf <- get_dcf()

  # File handling (HTML)
  zemi_html <- file_find(zemi_html, "zemi_html",
                         instruction = dcf$`no-html`)
  html <- read_html(zemi_html)

  # File handling (Excel)
  faculty_xlsx <- file_find(faculty_xlsx, "faculty_xlsx",
                            instruction = dcf$`no-xlsx`)
  excel <- readxl::read_xlsx(faculty_xlsx)

  # Start scraping
  li <- html %>% html_elements("li.modtype_page")

  extract_page_info <- function(elem) {
    id <- elem %>% html_attr("id") %>% str_replace("module-", "")
    link <- elem %>% html_elements(".aalink") %>% html_attr("href")
    .pagename <- elem %>% html_element(".instancename")
    .accesshide <- .pagename %>% html_element(".accesshide")
    xml2::xml_remove(.accesshide)
    pagename <- .pagename %>% html_text()

    tibble::tibble(
      link = link,
      pagename = pagename,
      type = "add",
      role = "teacheredit"
    )
  }

  info <- extract_page_info(li)

  result <- info %>%
    dplyr::left_join(excel,by = "pagename") %>%
    dplyr::select(.data$link, .data$pagename, .data$type,
                  .data$role, .data$OMUID, .data$Email) %>%
    dplyr::mutate(name = .data$pagename, name_course = name_course,
                  project_id = project_id)

  out_file <- config::get("out_shinsei")
  writexl::write_xlsx(result, path = out_file)
  usethis::ui_done(str_glue("{dcf$`complete`}\t{out_file}"))

  invisible(result)
}
