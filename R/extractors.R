#' Extract Index Data
#'
#' This function extracts data from the index file for a specific year and form type. # nolint: line_length_linter.
#'
#' @param year The year of the data.
#' @param form_type The form type to filter on.
#' @param nrows The number of rows to read from the index file. Default is unlimited (NULL).
#' @return A data frame containing the extracted data.
#' @keywords internal
extract_index_data <- function(year, form_type, nrows = NULL) {
  all_variables <- read_variables_from_csv(system.file("variables/all_variables.csv", package = "ConverterIRS990"))

  index_csv_path <- paste0("data/index_file/index_", year, ".csv")

  if (!file.exists(index_csv_path)) {
    index_csv_path <- download_index_csv(year)
  }

  # Only pass nrows if it is not NULL
  if (!is.null(nrows)) {
    index_df <- read.csv(index_csv_path, 
                     nrows = nrows, 
                     colClasses = c(RETURN_ID = "character",
                                    FILING_TYPE = "character",
                                    EIN = "character",
                                    TAX_PERIOD = "character",
                                    SUB_DATE = "character",
                                    TAXPAYER_NAME = "character",
                                    RETURN_TYPE = "character",
                                    DLN = "character",
                                    OBJECT_ID = "character",
                                    XML_BATCH_ID = "character"))
  } else {
    index_df <- read.csv(index_csv_path, 
                     colClasses = c(RETURN_ID = "character",
                                    FILING_TYPE = "character",
                                    EIN = "character",
                                    TAX_PERIOD = "character",
                                    SUB_DATE = "character",
                                    TAXPAYER_NAME = "character",
                                    RETURN_TYPE = "character",
                                    DLN = "character",
                                    OBJECT_ID = "character",
                                    XML_BATCH_ID = "character"))
  }

  index_df <- index_df[index_df$RETURN_TYPE == form_type, ]

  xml_files_path_prefix <- paste0("data/xml_files/", year, "/")

  all_extracted_data <- list()

  if (year < 2024) {
    for (i in seq_len(nrow(index_df))) {
      row <- index_df[i, ]
      xml_folder_path <- xml_files_path_prefix
      xml_file <- paste0(xml_folder_path, row$OBJECT_ID, "_public.xml")

      if (!dir.exists(xml_folder_path)) {
        download_and_extract_zip_legacy(xml_files_path_prefix, year)
      }

      tryCatch({
        extracted_data <- extract_variables_and_attr_from_xml(xml_file, all_variables)
        combined_data <- c(as.list(row), extracted_data)
        all_extracted_data <- append(all_extracted_data, list(combined_data))
      }, error = function(e) {
        message("Error processing file: ", xml_file)
      })
    }
  } else {
    for (i in seq_len(nrow(index_df))) {
      row <- index_df[i, ]

      print(row)

      object_id <- trimws(row$OBJECT_ID)
      print(paste("Correct OBJECT_ID from index:", object_id))
      
      # Reconstruct the file path
      xml_folder_path <- paste0(xml_files_path_prefix, row$XML_BATCH_ID, "/")
      xml_file <- paste0(xml_folder_path, object_id, "_public.xml")
      

      if (!dir.exists(xml_folder_path)) {
        download_and_extract_zip(xml_files_path_prefix, row$XML_BATCH_ID, year)
      }

      tryCatch({
        extracted_data <- extract_variables_and_attr_from_xml(xml_file, all_variables)
        combined_data <- c(as.list(row), extracted_data)
        print("combined_data processed")
        all_extracted_data <- append(all_extracted_data, list(combined_data))
        print("all_extracted_data processed")
      }, error = function(e) {
        message("Error occurred during data extraction:")
        message("File: ", xml_file)
        message("Error message: ", e$message)
      })
    }
  }

  combined_df <- do.call(rbind, lapply(all_extracted_data, as.data.frame))
  return(combined_df)
}


#' Extract Schedule C Data
#'
#' This function extracts Schedule C data from XML files for a specific year.
#'
#' @param year The year of the data.
#' @param nrows The number of rows to read from the index file. Default is unlimited (NULL).
#' @return A data frame containing the extracted Schedule C data.
#' @importFrom utils read.csv
#' @keywords internal
extract_schedule_c_data <- function(year, nrows = NULL) {
  schedule_c_variables <- read_variables_from_csv(system.file("variables/schedule_c_variables.csv", package = "ConverterIRS990"))

  index_csv_path <- paste0("data/index_file/index_", year, ".csv")

  if (!file.exists(index_csv_path)) {
    index_csv_path <- download_index_csv(year)
  }

  # Only pass nrows if it is not NULL
  if (!is.null(nrows)) {
    index_df <- read.csv(index_csv_path, 
                     nrows = nrows, 
                     colClasses = c(RETURN_ID = "character",
                                    FILING_TYPE = "character",
                                    EIN = "character",
                                    TAX_PERIOD = "character",
                                    SUB_DATE = "character",
                                    TAXPAYER_NAME = "character",
                                    RETURN_TYPE = "character",
                                    DLN = "character",
                                    OBJECT_ID = "character",
                                    XML_BATCH_ID = "character"))
  } else {
    index_df <- read.csv(index_csv_path, 
                     colClasses = c(RETURN_ID = "character",
                                    FILING_TYPE = "character",
                                    EIN = "character",
                                    TAX_PERIOD = "character",
                                    SUB_DATE = "character",
                                    TAXPAYER_NAME = "character",
                                    RETURN_TYPE = "character",
                                    DLN = "character",
                                    OBJECT_ID = "character",
                                    XML_BATCH_ID = "character"))
  }

  index_df <- index_df[index_df$RETURN_TYPE == "990", ]

  xml_files_path_prefix <- paste0("data/xml_files/", year, "/")

  valid_schedule_c_data <- list()

  if (year < 2024) {
    for (i in seq_len(nrow(index_df))) {
      row <- index_df[i, ]
      xml_folder_path <- xml_files_path_prefix
      xml_file <- paste0(xml_folder_path, row$OBJECT_ID, "_public.xml")

      if (!dir.exists(xml_folder_path)) {
        download_and_extract_zip_legacy(xml_files_path_prefix, year)
      }

      tryCatch({
        extracted_data <- extract_variables_and_attr_from_xml(xml_file, schedule_c_variables)

        if (any(sapply(extracted_data, function(x) !is.na(x)))) {
          combined_data <- c(as.list(row), extracted_data)
          valid_schedule_c_data <- append(valid_schedule_c_data, list(combined_data))
        } else {
          message("Skipping OBJECT_ID ", row$OBJECT_ID, " due to empty Schedule C fields.")
        }
      }, error = function(e) {
        message("Error processing file: ", xml_file)
      })
    }
  } else {
    for (i in seq_len(nrow(index_df))) {
      row <- index_df[i, ]
      xml_folder_path <- paste0(xml_files_path_prefix, row$XML_BATCH_ID, "/")
      xml_file <- paste0(xml_folder_path, row$OBJECT_ID, "_public.xml")

      if (!dir.exists(xml_folder_path)) {
        download_and_extract_zip(xml_files_path_prefix, row$XML_BATCH_ID, year)
      }

      tryCatch({
        extracted_data <- extract_variables_and_attr_from_xml(xml_file, schedule_c_variables)

        if (any(sapply(extracted_data, function(x) !is.na(x)))) {
          combined_data <- c(as.list(row), extracted_data)
          valid_schedule_c_data <- append(valid_schedule_c_data, list(combined_data))
        } else {
          message("Skipping OBJECT_ID ", row$OBJECT_ID, " due to empty Schedule C fields.")
        }
      }, error = function(e) {
        message("Error processing file: ", xml_file)
      })
    }
  }

  valid_schedule_c_df <- do.call(rbind, lapply(valid_schedule_c_data, as.data.frame))
  return(valid_schedule_c_df)
}

#' Extract Recipient Data
#'
#' This function extracts recipient data from XML files for a specific year.
#'
#' @param year The year of the data.
#' @param nrows The number of rows to read from the index file. Default is unlimited (NULL).
#' @return A data frame containing the extracted recipient data.
#' @importFrom utils read.csv
#' @keywords internal
extract_recipient_data <- function(year, nrows = NULL) {
  recipient_variables <- read_variables_from_csv(system.file("variables/recipient_variables.csv", package = "ConverterIRS990"))

  index_csv_path <- paste0("data/index_file/index_", year, ".csv")

  # Download the index CSV if it does not exist
  if (!file.exists(index_csv_path)) {
    index_csv_path <- download_index_csv(year)
  }

  # Only pass nrows if it is not NULL
  if (!is.null(nrows)) {
    index_df <- read.csv(index_csv_path, 
                     nrows = nrows, 
                     colClasses = c(RETURN_ID = "character",
                                    FILING_TYPE = "character",
                                    EIN = "character",
                                    TAX_PERIOD = "character",
                                    SUB_DATE = "character",
                                    TAXPAYER_NAME = "character",
                                    RETURN_TYPE = "character",
                                    DLN = "character",
                                    OBJECT_ID = "character",
                                    XML_BATCH_ID = "character"))
  } else {
    index_df <- read.csv(index_csv_path, 
                     colClasses = c(RETURN_ID = "character",
                                    FILING_TYPE = "character",
                                    EIN = "character",
                                    TAX_PERIOD = "character",
                                    SUB_DATE = "character",
                                    TAXPAYER_NAME = "character",
                                    RETURN_TYPE = "character",
                                    DLN = "character",
                                    OBJECT_ID = "character",
                                    XML_BATCH_ID = "character"))
  }

  # Filter the index DataFrame to include only rows with the specified form type
  index_df <- index_df[index_df$RETURN_TYPE == '990', ]

  all_recipient_data <- list()

  xml_files_path_prefix <- paste0("data/xml_files/", year, "/")

  if (year >= 2024) {
    for (i in seq_len(nrow(index_df))) {
      row <- index_df[i, ]
      xml_folder_path <- paste0(xml_files_path_prefix, row$XML_BATCH_ID, "/")
      xml_file <- paste0(xml_folder_path, row$OBJECT_ID, "_public.xml")

      # Download and extract the ZIP file if the XML folder does not exist
      if (!dir.exists(xml_folder_path)) {
        download_and_extract_zip(xml_files_path_prefix, row$XML_BATCH_ID, year)
      }

      tryCatch({
        recipient_data <- extract_recipient_table(xml_file, row$OBJECT_ID, recipient_variables)
        all_recipient_data <- append(all_recipient_data, recipient_data)
      }, error = function(e) {
        message("Error processing file: ", xml_file)
      })
    }
  } else {
    for (i in seq_len(nrow(index_df))) {
      row <- index_df[i, ]
      xml_folder_path <- xml_files_path_prefix
      xml_file <- paste0(xml_folder_path, row$OBJECT_ID, "_public.xml")

      # Download and extract the ZIP file if the XML folder does not exist
      if (!dir.exists(xml_folder_path)) {
        download_and_extract_zip_legacy(xml_files_path_prefix, year)
      }

      tryCatch({
        recipient_data <- extract_recipient_table(xml_file, row$OBJECT_ID, recipient_variables)
        all_recipient_data <- append(all_recipient_data, recipient_data)
      }, error = function(e) {
        message("Error processing file: ", xml_file)
      })
    }
  }

  recipient_df <- do.call(rbind, lapply(all_recipient_data, as.data.frame))

  # Remove the "irs:" prefix from the column names
  recipient_df <- as.data.frame(recipient_df)
  colnames(recipient_df) <- gsub("irs:", "", colnames(recipient_df))

  return(recipient_df)
}
