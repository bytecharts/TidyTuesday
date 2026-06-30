library(tidytuesdayR)
library(readr)

tt_cache <- function(year, week, folder = "data") {
  # Create folder if needed
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  # Download TT data
  tt <- tidytuesdayR::tt_load(year, week)

  # Save every dataset in the list
  for (nm in names(tt)) {
    # Skip non-data-frame objects
    if (!is.data.frame(tt[[nm]])) {
      next
    }

    file_path <- file.path(
      folder,
      paste0("tt_", year, "_", week, "_", nm, ".csv")
    )

    if (!file.exists(file_path)) {
      write_csv(tt[[nm]], file_path)
      message("Saved: ", file_path)
    } else {
      message("Already exists: ", file_path)
    }
  }

  invisible(tt)
}
