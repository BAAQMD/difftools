#' Compare Two Tabular Datasets
#'
#' @description
#' `diff_and_render()` uses the `daff` package to compare two tabular datasets.
#' The difference is rendered as HTML, which is also (optionally) written to a file.
#'
#' @param a `tibble`, `data.frame`, or path to file
#' @param b `tibble`, `data.frame`, or path to file
#' @param title rendered in HTML
#' @param outfile if not `NULL`, then write to this file
#' @param summary see help for [daff::render_diff()]
#' @param ignore_whitespace see help for [daff::diff_data()]
#' @param never_show_order see help for [daff::diff_data()]
#' @param unchanged_context see help for [daff::diff_data()]
#' @param ... further arguments to [daff::diff_data()]
#' @param verbose display informative messages
#'
#' @return a `diff` object, invisibly
#'
#' @export
diff_and_render <- function (
  a,
  b,
  title = NULL,
  outfile = NULL,
  summary = TRUE,
  ignore_whitespace = FALSE,
  never_show_order = TRUE,
  unchanged_context = 0L,
  ...,
  verbose = TRUE
) {

  require(daff)

  msg <- function (...) if(isTRUE(verbose)) message("[diff_and_render] ", ...)

  try_to_read_csv_or_xlsx <- function (path) {
    path <- normalizePath(path, mustWork = TRUE)
    is_csv <- stringr::str_detect(path, regex("\\.csv$", ignore_case = TRUE))
    is_excel <- stringr::str_detect(path, regex("\\.xls(x?)$", ignore_case = TRUE))
    if (is_csv) {
      readr::read_csv(path)
    } else if (is_excel) {
      readxl::read_excel(path)
    }
  }

  if (is.character(a)) {
    a <- try_to_read_csv_or_xlsx(a)
  }

  if (is.character(b)) {
    b <- try_to_read_csv_or_xlsx(b)
  }


  diff_object <-
    diff_data(
      a, b,
      ignore_whitespace = ignore_whitespace,
      never_show_order = never_show_order,
      unchanged_context = unchanged_context,
      ...)

  if (is.null(outfile)) {
    file <- tempfile(fileext = ".html")
  } else {
    file <- glue::glue(outfile)
  }

  diff_object %>%
    render_diff(
      title = title,
      file = file,
      summary = summary)

  return(invisible(diff_object))

}
