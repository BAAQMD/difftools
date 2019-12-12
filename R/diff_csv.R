#' Diff two CSV files
#'
#' @note migrated from `inventory` package, 2019-12-12
#'
diff_csv <- function (
  file1,
  file2,
  include = everything(),
  exclude = NULL,
  ...
) {

  data1 <-
    read_csv(file1) %>%
    select_(
      .dots = include)

  data2 <-
    read_csv(file2)

  this_diff <-
    daff::diff_data(
      file1,
      file2,
      ...)

  if (interactive()) {
    render_diff(this_diff)
  }

  return(invisible(this_diff))

}
