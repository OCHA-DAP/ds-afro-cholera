get_distribution_list <- function(test_list = FALSE) {
  # Define blob path
  prefix <- "ds-afro-cholera"

  blob_name <- if (test_list) {
    file.path(prefix, "monitoring", "test_distribution_list.csv")
  } else {
    file.path(prefix, "monitoring", "distribution_list.csv")
  }

  # Connect to blob container
  blob_endpoint <- AzureStor$storage_endpoint(Sys.getenv("DSCI_AZ_ENDPOINT_DEV"), sas=Sys.getenv("DSCI_AZ_BLOB_DEV_SAS"))
  container <- AzureStor$storage_container(blob_endpoint, "projects")

  # Download and read the CSV
  temp_file <- tempfile(fileext = ".csv")
  AzureStor$storage_download(container, src = blob_name, dest = temp_file, overwrite = TRUE)

  readr$read_csv(temp_file, show_col_types = FALSE)
}
