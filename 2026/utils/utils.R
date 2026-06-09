library(tidytuesdayR)
library(readr)

# DRY function for any TidyTuesday dataset
tt_cache <- function(year, week, folder = "data") {
  # Ensure folder exists
  if (!dir.exists(folder)) {
    dir.create(folder)
  }

  # Construct filename
  file_path <- file.path(folder, paste0("tt_", year, "_", week, ".csv"))

  # Return cached CSV if it exists
  if (file.exists(file_path)) {
    message("Reading cached CSV: ", file_path)
    return(read_csv(file_path, show_col_types = FALSE))
  }

  # Download from TidyTuesday and save CSV
  message("Downloading TidyTuesday ", year, " week ", week)
  tt <- tidytuesdayR::tt_load(year, week)

  # Take the first dataset (or specify one later)
  first_dataset <- tt[[1]]

  write_csv(first_dataset, file_path)
  message("Saved CSV: ", file_path)

  return(first_dataset)
}
