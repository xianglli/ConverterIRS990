# ConverterIRS990

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

**ConverterIRS990** is an R package designed to facilitate the extraction and conversion of IRS Form 990 XML data into CSV or RDA formats. This package is particularly useful for researchers, analysts, and developers who need to process and analyze large volumes of IRS data efficiently.

## Features

- Convert IRS XML data to CSV format using `convert_csv()`.
- Convert IRS XML data to RDA format using `convert_rda()`.
- Supports extraction of specific schedules, such as Schedule C.
- Allows extraction of data for a specified number of rows from the IRS index file.

## Installation

You can install the package from GitHub using the following command:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install ConverterIRS990 package from GitHub
devtools::install_github("xianglli/ConverterIRS990")
```

## Usage

Once installed, you can load the package and start using its functions to process IRS XML data.

### Example: Convert XML to CSV

```r
library(ConverterIRS990)

# Convert IRS Form 990 XML data for the year 2024 to a CSV file
convert_csv(year = 2024, 
            form_type = "990", 
            recipient = FALSE, 
            schedule = "", 
            output_file = "output_file.csv", 
            nrows = 100)
```

### Example: Convert XML to RDA

```r
# Convert IRS Form 990 XML data for the year 2024 to an RDA file
convert_rda(year = 2024, 
            form_type = "990", 
            recipient = FALSE, 
            schedule = "", 
            output_file = "output_file.rda", 
            nrows = 100)
```

### Parameters

- **year**: The year of the data to process (e.g., `2024`).
- **form_type**: The IRS form type to process (e.g., `"990"`).
- **recipient**: Boolean flag to extract recipient organization data (`TRUE` or `FALSE`).
- **schedule**: The schedule to extract (e.g., `"C"` for Schedule C data). Leave as an empty string for default extraction.
- **output_file**: The path where the output file (CSV or RDA) will be saved.
- **nrows**: The number of rows to read from the IRS index file. Set to `NULL` for unlimited rows.

## Contributing

Contributions are welcome! If you find a bug or have a feature request, please open an issue on the [GitHub Issues page](https://github.com/xianglli/ConverterIRS990/issues).

If you'd like to contribute code, please fork the repository and create a pull request with your changes. Make sure to include tests for any new functionality.

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## Acknowledgments

This package was developed to assist in the analysis of IRS Form 990 data, making it easier for users to process and convert large datasets into manageable formats for analysis.

