#' Apply bench calibrations
#'
#' \code{bench_cal} applies bench calibrations to raw data. Calibrates inertial
#' sensors and fills in the At, Gt, and Mt slots. At, Mt, and Gt are in units of
#' g, microteslas, and radians per second, respectively, in the tag's frame.
#' Then calibrates the pressure sensor and fills in the p field in the data
#' slot. Also fills in the t and camon fields in the data slot.
#'
#' @param prh A PRH object. Should already be decimated (see
#'   \code{\link{decimate_prh}}) and trimmed (see \code{\link{trim_prh}})
#' @return A PRH object with updated rawA, rawG, rawM, At, Gt, and Mt slots.
bench_cal <- function(prh) {
  if (nrow(prh@rawdata) == 0)
    stop("PRH data is empty - have you initiated it?")
  if (length(prh@freq) == 0)
    stop("PRH frequency not set - have you decimated it?")
  if (prh@tagon == num_to_POSIX(0) ||
      prh@tagoff == num_to_POSIX(0))
    stop("PRH tagon/tagoff not set - have you trimmed it?")

  cal <- load_cal(prh@tagnum)

  # Fill in column t in prh@data from prh@rawdata
  prh@data <- prh@rawdata %>%
    dplyr::select(t = datetimeUTC)

  # Apply bench calibrations
  apply_cal <- function(sensor) {
    if (!(sensor %in% c("a", "g", "mon", "moff")))
      stop("sensor must be one of c(\"a\", \"g\", \"mon\", \"moff\")")

    # Get calibration slopes and constants
    cal_const <- cal[[paste0(sensor, "const")]]
    cal_slope <- cal[[paste0(sensor, "cal")]]
    if (is.null(cal_const) || is.null(cal_slope)) browser()

    # Get raw intertial sensor values
    sensor_name <- switch(sensor,
                          a = "acc",
                          g = "gyr",
                          mon = "mag",
                          moff = "mag")
    vals <- dplyr::select(prh@rawdata,
                          dplyr::starts_with(sensor_name)) %>%
      as.matrix

    # Apply calibration
    (vals %*% cal_slope) + matrix(rep(cal_const, each = nrow(vals)),
                                  nrow = nrow(vals))
  }
  cal_mats <- purrr::map(c("a", "g", "mon", "moff"), apply_cal)
  prh@At <- cal_mats[[1]]
  prh@Gt <- cal_mats[[2]]
  # Use different magnetometer calibrations for when the camera is on and off
  prh@data$camon <- prh@rawdata$ccStatus != "---"
  mon <- cal_mats[[3]]
  mon[!prh@data$camon,] <- 0
  moff <- cal_mats[[4]]
  moff[prh@data$camon,] <- 0
  prh@Mt <- mon + moff

  # Calibrate pressure
  prh@data$p <- prh@rawdata$depthM * cal$pcal + cal$pconst

  prh
}

# In situ calibration for accelerometer

