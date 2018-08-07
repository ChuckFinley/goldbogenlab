.onLoad <- function(libname, pkgname) {
  if (!file.exists(config_path())) {
    message("No config file found, creating blank one. Use set_cats_path to update config file.")
    write_config(list())
  } else {
    config <- read_config()

    if (!("cats_path" %in% names(config)) ||
        !file.exists(config$cats_path)) {
      message("CATS not found. Use set_cats_path to update config file.")
    }
  }
}

set_cats_path <- function(cats_path) {
  if (!file_test("-d", cats_path)) {
    stop("cats_path must be a valid directory.")
  }
  config <- read_config()
  config$cats_path <- cats_path
  write_config(config)
}

config_path <- function() file.path(system.file(package = "goldbogenlab"),
                                    "_config.yml")

read_config <- function()  yaml::read_yaml(config_path())

write_config <- function(config) yaml::write_yaml(config, config_path())
