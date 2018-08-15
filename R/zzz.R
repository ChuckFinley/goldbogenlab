#' @importFrom dplyr %>%

.onLoad <- function(libname, pkgname) {
  if (!file.exists(config_path())) {
    packageStartupMessage("No config file found, creating blank one. Use set_cats_path to update config file.")
    write_config(list())
  } else {
    config <- read_config()
    if (!("cats_path" %in% names(config)) ||
        !dir.exists(config$cats_path)) {
      packageStartupMessage("CATS path not found. Use set_cats_path to update config file.")
    }
  }
}

num_to_POSIX <- function(num) {
  as.POSIXct(num,
             origin = as.POSIXct("1970-01-01", tz = "UTC"),
             tz = "UTC")
}
