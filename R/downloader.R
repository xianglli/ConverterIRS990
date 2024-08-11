#' Download Index CSV
#'
#' This function downloads the index CSV file for a specific year.
#'
#' @param year The year of the data to download.
#' @return The local path to the downloaded CSV file.
#' @keywords internal
download_index_csv <- function(year) {
  url <- paste0("https://apps.irs.gov/pub/epostcard/990/xml/", year, "/index_", year, ".csv")
  local_path <- paste0("data/index_file/index_", year, ".csv")

  if (!dir.exists(dirname(local_path))) {
    dir.create(dirname(local_path), recursive = TRUE)
  } else {
    message("Using existing index CSV for year ", year, ".")
  }

  download.file(url, local_path)

  message("Downloaded index CSV for year ", year, ".")
  return(local_path)
}

#' Download and Extract ZIP
#'
#' This function downloads and extracts ZIP files from IRS servers.
#'
#' @param xml_files_path_prefix The local path prefix for XML files.
#' @param xml_batch_id The batch ID for the XML files.
#' @param year The year of the data.
#' @importFrom utils download.file unzip
#' @keywords internal
download_and_extract_zip <- function(xml_files_path_prefix, xml_batch_id, year) {
  zip_url <- paste0("https://apps.irs.gov/pub/epostcard/990/xml/", year, "/", xml_batch_id, ".zip")
  local_zip_path <- paste0(xml_files_path_prefix, xml_batch_id, ".zip")

  if (!dir.exists(xml_files_path_prefix)) {
    dir.create(xml_files_path_prefix, recursive = TRUE)
  }

  download.file(zip_url, local_zip_path, mode = "wb")

  unzip(local_zip_path, exdir = paste0(xml_files_path_prefix, xml_batch_id))

  message("Downloaded and extracted ", xml_batch_id, ".zip.")
}

#' Download and Extract ZIP for Legacy Data
#'
#' This function downloads and extracts ZIP files for legacy data (pre-2024).
#'
#' @param xml_files_path The local path for XML files.
#' @param year The year of the data.
#' @keywords internal
download_and_extract_zip_legacy <- function(xml_files_path, year) {
  zip_batches <- list(
    `2023` = c(
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_01A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_02A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_03A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_04A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_05A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_05B.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_06A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_07A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_08A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_09A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_10A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_11A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_11B.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_11C.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2023/2023_TEOS_XML_12A.zip"
    ),
    `2022` = c(
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_01A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_01B.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_01C.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_01D.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_01E.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_01F.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_11A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_11B.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2022/2022_TEOS_XML_11C.zip"
    ),
    `2021` = c(
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01A.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01B.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01C.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01D.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01E.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01F.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01G.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2021/2021_TEOS_XML_01H.zip"
    ),
    `2020` = c(
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/2020_TEOS_XML_CT1.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_1.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_2.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_3.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_4.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_5.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_6.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_7.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2020/download990xml_2020_8.zip"
    ),
    `2019` = c(
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/2019_TEOS_XML_CT1.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_1.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_2.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_3.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_4.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_5.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_6.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_7.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2019/download990xml_2019_8.zip"
    ),
    `2018` = c(
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/2018_TEOS_XML_CT1.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/2018_TEOS_XML_CT2.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/2018_TEOS_XML_CT3.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/download990xml_2018_1.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/download990xml_2018_2.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/download990xml_2018_3.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/download990xml_2018_4.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/download990xml_2018_5.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/download990xml_2018_6.zip",
      "https://apps.irs.gov/pub/epostcard/990/xml/2018/download990xml_2018_7.zip"
    )
  )

  if (!year %in% names(zip_batches)) {
    stop("No ZIP batch URLs defined for the year ", year, ".")
  }

  for (zip_url in zip_batches[[as.character(year)]]) {
    local_zip_path <- file.path(xml_files_path, basename(zip_url))

    if (!dir.exists(xml_files_path)) {
      dir.create(xml_files_path, recursive = TRUE)
    }

    download.file(zip_url, local_zip_path, mode = "wb")
    unzip(local_zip_path, exdir = xml_files_path)

    message("Downloaded and extracted ", basename(zip_url), ".")
  }
}
