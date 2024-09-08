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
  # Check if the file exists
  if (!file.exists(xml_file)) {
    stop("File does not exist: ", xml_file)
  }
  
  # Check if the file is empty
  if (file.info(xml_file)$size == 0) {
    stop("File is empty: ", xml_file)
  }
  
  # Try to parse the XML content
  tryCatch({
    doc <- xmlParse(xml_file)
  }, error = function(e) {
    stop("Failed to parse XML file: ", xml_file, " - Error: ", e$message)
  })

  doc <- xmlParse(xml_file)
  root <- xmlRoot(doc)
  ns <- c(irs = "http://www.irs.gov/efile")

  extracted_data <- list()

  for (var in variables) {
    xpath_expr <- gsub("/text\\(\\)", "", var)
    xpath_parts <- unlist(strsplit(xpath_expr, "/"))
    xpath_expr <- paste0("irs:", xpath_parts, collapse = "/")

    nodes <- getNodeSet(root, xpath_expr, namespaces = ns)

    if (length(nodes) == 0) {
      extracted_data[[var]] <- NA
    } else {
      node <- nodes[[1]]
      extracted_data[[var]] <- xmlValue(node)
      # # get the attribute "ReturnData/IRS990/Organization501cInd/organization501cTypeTxt"
      
      if(grepl("Organization501cInd", xpath_expr)){
        attribute_xpath <- paste0(xpath_expr, "/@organization501cTypeTxt")
        attribute_nodes <- getNodeSet(root, attribute_xpath, namespaces = ns)

        attribute_node <- attribute_nodes[[1]]

        print(attribute_node["organization501cTypeTxt"])
        extracted_data[[var]] <-attribute_node["organization501cTypeTxt"]
        
      }
    }
  }
  return(extracted_data)
}

#' Extract Recipient Table from XML
#'
#' This function extracts recipient table data from a specific XML file.
#'
#' @param xml_file The path to the XML file.
#' @param object_id The OBJECT_ID from the index file, used as an identifier.
#' @param recipient_variables A vector of recipient variables to extract.
#' @return A list containing the extracted recipient data.
#' @importFrom XML xmlParse xmlRoot getNodeSet xmlValue
#' @keywords internal
extract_recipient_table <- function(xml_file, object_id, recipient_variables) {
  doc <- xmlParse(xml_file)
  root <- xmlRoot(doc)
  ns <- c(irs = "http://www.irs.gov/efile")
  
  recipient_data <- list()
  
  recipient_elements <- getNodeSet(root, "//irs:RecipientTable", namespaces = ns)

  if (length(recipient_elements) == 0) {
    message("No recipient elements found in file: ", xml_file)
    return(NULL)
  }
  
  for (element in recipient_elements) {
    recipient <- list(OBJECT_ID = object_id)

    print(element)
    
    for (var in recipient_variables) {
      xpath_expr <- gsub("/text\\(\\)", "", var)
      xpath_parts <- unlist(strsplit(xpath_expr, "/"))
      xpath_expr <- paste0("irs:", xpath_parts, collapse = "/")

      node_set <- getNodeSet(element, xpath_expr, namespaces = ns)
      
      if (length(node_set) > 0) {
        value <- xmlValue(node_set[[1]])
        recipient[[var]] <- value
      } else {
        recipient[[var]] <- NA  # Or any other default value
        message("No data found for variable: ", var, " in OBJECT_ID: ", object_id)
      }
    }
    
    recipient_data <- append(recipient_data, list(recipient))
  }
  
  return(recipient_data)
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
