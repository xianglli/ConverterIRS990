#' Read Variables from CSV
#'
#' This function reads a list of variables from a CSV file.
#'
#' @param file_path Path to the CSV file.
#' @return A character vector of variables.
#' @keywords internal
read_variables_from_csv <- function(file_path) {
  df <- read.csv(file_path)
  return(as.character(df$Variables))
}

#' Extract Variables and Attributes from XML
#'
#' This function extracts specific variables and their attributes from an XML file.
#'
#' @param xml_file The path to the XML file.
#' @param variables A vector of variables to extract.
#' @return A list of extracted variables and attributes.
#' @importFrom XML xmlParse xmlRoot getNodeSet xmlValue xmlAttrs
#' @keywords internal
extract_variables_and_attr_from_xml <- function(xml_file, variables) {
  doc <- xmlParse(xml_file)
  root <- xmlRoot(doc)
  ns <- c(irs = "http://www.irs.gov/efile")

  extracted_data <- list()

  for (var in variables) {
    xpath_expr <- gsub("/text\\(\\)", "", var)
    xpath_parts <- unlist(strsplit(xpath_expr, "/"))
    xpath_expr <- paste0("irs:", xpath_parts, collapse = "/")

    node <- getNodeSet(root, xpath_expr, namespaces = ns)[[1]]
    if (!is.null(node)) {
      extracted_data[[var]] <- xmlValue(node)
      attrs <- xmlAttrs(node)
      if (!is.null(attrs)) {
        for (attr_name in names(attrs)) {
          extracted_data[[paste(var, attr_name, sep = "/")]] <- attrs[[attr_name]]
        }
      }
    } else {
      extracted_data[[var]] <- NA
    }
  }

  return(extracted_data)
}

#' Read Variables from CSV
#'
#' This function reads a list of variables from a CSV file.
#'
#' @param file_path Path to the CSV file.
#' @return A character vector of variables.
#' @keywords internal
read_variables_from_csv <- function(file_path) {
  df <- read.csv(file_path)
  return(as.character(df$Variables))
}
