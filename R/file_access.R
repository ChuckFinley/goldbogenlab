#' Get the file path to tag data
#'
#' \code{get_tag_path} gets the path to tag data (raw or processed) in the CATS
#' folder.
#'
#' The COPYCATS drive should have all the processed data but only the most
#' recent raw data. If \code{get_tag_path} is throwing an error unexpectedly
#' double check that the raw data you're looking for is on the hard drive.
#'
#' @param tagid A string with the tagid (e.g. "mn180105-22a")
#' @param raw A logical indicating whether to get the path to the raw or
#'   processed tag data. FALSE by default.
#' @return A string with the file path to the tag data.
get_tag_path <- function(tagid, raw = FALSE) {
  if(raw) {
    result <- dir(file.path(get_cats_path(), "tag_data_raw"),
                  pattern = stringr::str_glue(tagid, "$"),
                  full.names = TRUE,
                  recursive = TRUE,
                  include.dirs = TRUE)
  } else {
    result <- dir(file.path(get_cats_path(), "tag_data"),
                  pattern = tagid,
                  full.names = TRUE)
  }
  if(length(result) != 1) {
    stop("tag_id must exist and be unique")
  }
  result
}

#' List all available deployments with raw data
#'
#' @return A string vector of all the tagids with available raw data.
list_raw_tags <- function() {
  # Lists all files n steps into the directory structure
  dir_n <- function(path, n) {
    if (n < 1)
      stop("n must be at least 1.")
    if (n == 1)
      dir(path, full.names = TRUE)
    else {
      contents <- dir(path, full.names = TRUE)
      is_file <- purrr::map_lgl(contents, ~ file_test("-f", .x))
      files <- contents[is_file]
      dirs <- contents[!is_file]
      unlist(purrr::map(dirs, dir_n, n = n - 1))
    }
  }

  dir_n(file.path(get_cats_path(), "tag_data_raw"), 3) %>%
    basename
}

#' Import cats data
#'
#' \code{import_cats} reads and concatenates all the raw data CSVs from a CATS
#' tag. May take a long time and return a massive result (> 1GB) because it
#' hasn't been decimated yet.
#'
#' @param tagid A string with the tagid (e.g. "mn180105-22a"). Make sure the raw
#'   data for the tag is on your hard drive!
#'
#' @return A tibble with the raw data from the tag.
#'
#' @seealso \code{\link{list_raw_tags}}
import_cats <- function(tagid) {
  if (!(tagid %in% list_raw_tags()))
    stop("Raw tag data not available.")
  cats_types <- "ccccddddddddddcciiddiicdddiciilci"
  cats_names <- c("dateUTC",
                  "timeUTC",
                  "dateLocal",
                  "timeLocal",
                  "accX",
                  "accY",
                  "accZ",
                  "gyrX",
                  "gyrY",
                  "gyrZ",
                  "magX",
                  "magY",
                  "magZ",
                  "tempIMU",
                  "gpsDate",
                  "gpsTime",
                  "gps3",
                  "gpsSats",
                  "depthM",
                  "depthDegC",
                  "light1",
                  "light2",
                  "syerr",
                  "battV",
                  "battmA",
                  "battmAh",
                  "camera",
                  "flags",
                  "led",
                  "camtime",
                  "gpsL",
                  "ccStatus",
                  "ccVidSz")
  raw_csvs <- dir(file.path(get_tag_path(tagid, raw = TRUE), "raw"),
      full.names = TRUE,
      pattern = "csv$")
  # Set up a progress bar
  pb <- dplyr::progress_estimated(length(raw_csvs))
  purrr::map(raw_csvs,
             # Update progress bar and call read_csv
             function(x, ...) {
               pb$tick()$print()
               readr::read_csv(x, ...)
             },
             locale = readr::locale(encoding = "latin1"),
             skip = 1,
             col_names = cats_names,
             col_types = cats_types,
             progress = FALSE) %>%
    dplyr::bind_rows()
}
