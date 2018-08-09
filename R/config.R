#' Set path to CATS folder
#'
#' \code{set_cats_path} updates the config file with the location of the CATS
#' folder.
#'
#' The CATS folder is typically on the COPYCATS hard drive. On a Mac the path is
#' usually /Volumes/COPYCATS/CATS and on Windows it's usually D:/CATS. The path
#'
#' @param cats_path A string with the path to the CATS folder.
#' @return NULL
#' @examples
#' \dontrun{
#' set_cats_path("/Volumes/COPYCATS/CATS")
#' }
set_cats_path <- function(cats_path) {
  if (!file_test("-d", cats_path)) {
    stop("cats_path must be a valid directory.")
  }
  config <- read_config()
  config$cats_path <- cats_path
  write_config(config)
}

#' Get path to CATS folder
#'
#' \code{get_cats_path} gets the file path to the CATS folder from the config
#' file.
#'
#' @return a string with the path to the CATS folder
get_cats_path <- function() {
  result <- read_config()$cats_path
  if (is.null(result))
    stop("CATS path not set.")
  result
}

config_path <- function() file.path(system.file(package = "goldbogenlab"),
                                    "_config.yml")

read_config <- function() yaml::read_yaml(config_path())

write_config <- function(config) yaml::write_yaml(config, config_path())
