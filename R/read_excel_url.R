#' Read excel files from URL
#'
#' This function use readxl::read_excel to read files directly from a URL.
#' Other arguments are passed directly to read_excel.
#' See \code{\link[readxl]{read_excel}}.
#' The function use \code{\link[httr]{GET}} from the httr-package to download the file
#' and \code{\link[httr]{write_disk}} from the same package to write the file. The downloaded file is deleted after use
#'
#' @param path String to the path, Will only work with valid and open URLs
#' @inheritParams readxl::read_excel
#'
#' @return tibble based on the excel sheet read
#' @export
#' @examples
#' read_excel_url(path = "https://file-examples.com/wp-content/uploads/2017/02/file_example_XLSX_100.xlsx")
read_excel_url <- function(path, sheet = NULL, range = NULL, col_names = TRUE,
                           col_types = NULL, na = "", trim_ws = TRUE, skip = 0,
                           n_max = Inf, guess_max = min(1000, n_max),
                           progress = readxl::readxl_progress(), .name_repair = "unique"){


  assertthat::is.string(path)


  # Downloading file and storing it in temp dir
  httr::GET(path, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))


  # Reading the file
  out <- readxl::read_excel(path = tf, sheet = sheet, range = range, col_names = col_names,
                            col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max,
                            guess_max = guess_max, progress = progress, .name_repair = .name_repair)

  # Deleting temp file after use
  file.remove(tf)


  return(out)
}



