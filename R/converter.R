#' Convert XML to CSV
#'
#' This function extracts IRS form data from XML files and saves it as a CSV file.
#'
#' @param year The year of the data.
#' @param form_type The IRS form type to process.
#' @param recipient Boolean to extract recipient data.
#' @param schedule The schedule to extract, default is the index data.
#' @param output_file The path where the CSV file will be saved.
#' @param nrows The number of rows to read from the index file. Default is unlimited (NULL).
#' @importFrom utils write.csv
#' @examples
#' # Example CSV file paths (ensure these files exist in your package's inst/variables/ directory)
#' all_variables_file <- system.file("variables/all_variables.csv", package = "ConverterIRS990")
#' recipient_variables_file <- system.file("variables/recipient_variables.csv", package = "ConverterIRS990")
#' schedule_c_variables_file <- system.file("variables/schedule_c_variables.csv", package = "ConverterIRS990")
#'
#' # If the files exist, run the example
#' if (file.exists(all_variables_file) && file.exists(recipient_variables_file) && file.exists(schedule_c_variables_file)) {
#'   convert_csv(2024, "990", FALSE, "", "output_file.csv", nrows = 100)
#' }
#' @export
convert_csv <- function(year, form_type, recipient = FALSE, schedule = "", output_file, nrows = NULL) {
  if (year < 2018) {
    stop("Year must be 2018 or later. IRS does not have data before 2018.")
  }

  if (form_type != "990") {
    stop("Only form 990 is supported in this version.")
  }

  if (recipient) {
    df <- extract_recipient_data(year, nrows)
  } else if (schedule == "") {
    df <- extract_index_data(year, form_type, nrows)
  } else if (schedule == "C") {
    df <- extract_schedule_c_data(year, nrows)
  }

  write.csv(df, file = output_file, row.names = FALSE)
  message("Data extraction completed. Output saved to ", output_file)
}



#' Convert XML to RDA
#'
#' This function extracts IRS form data from XML files and saves it as an RDA file.
#'
#' @param year The year of the data.
#' @param form_type The IRS form type to process.
#' @param recipient Boolean to extract recipient data.
#' @param schedule The schedule to extract, default is the index data.
#' @param output_file The path where the RDA file will be saved.
#' @param nrows The number of rows to read from the index file. Default is unlimited (NULL).
#' @importFrom utils write.csv
#' @examples
#' convert_rda(2024, "990", FALSE, "", "output_file.rda", nrows = 100)
#' @export
convert_rda <- function(year, form_type, recipient = FALSE, schedule = "", output_file, nrows = NULL) {
  if (year < 2018) {
    stop("Year must be 2018 or later. IRS does not have data before 2018.")
  }

  if (form_type != "990") {
    stop("Only form 990 is supported in this version.")
  }

  if (recipient) {
    df <- extract_recipient_data(year, nrows)
  } else if (schedule == "") {
    df <- extract_index_data(year, form_type, nrows)
  } else if (schedule == "C") {
    df <- extract_schedule_c_data(year, nrows)
  }

  save(df, file = output_file)
  message("Data extraction completed. Output saved to ", output_file)
}
