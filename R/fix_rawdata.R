#' Fix backwards timestamp skips
#'
#' \code{fix_backskip} attempts to fix out-of-order timesteps.
#'
#' Sometimes CATS timesteps skip forwards and then backwards, which is trickier
#' than a simple gap. \code{fix_backskip} uses a running diff of the timestamps
#' to look for phases with a large positive diff followed by a negative diff.
#' These skips are then removed with a warning if longer than a threshold (2s by
#' default).
#'
#' @param prh A PRH object.
#' @param thr A numeric defining the duration threshold (in sec) for warning
#'   about out-of-order phases.
#' @return A PRH object with the rawdata slot updated
#' @examples
#' \dontrun{
#' # There's an example from 2018-06-07 15:49:46.46 UTC to 2018-06-07
#' # 15:49:49.352 UTC in deployment mn180607-44
#' # Issues a warning there's out-of-order timestamps
#' prh <- init_prh(PRH("mn180607-44", "44"))
#' # Decimation fails
#' prh_deci <- decimate_prh(prh)
#' # Fix it!
#' prh_fix <- fix_backskip(prh)
#' # Now decimation works
#' prh_deci <- decimate_prh(prh_fix, 10)
#' }
#' @seealso \code{\link{import_cats}}
fix_backskip <- function(prh, thr = 2) {
  periods <- as.numeric(diff(prh@rawdata$datetimeUTC), units = "secs")
  # Assumes programmed frequency was a whole number e.g. 400 Hz
  freq <- round(1 / median(periods))
  period <- 1 / freq

  # Identify where the backwards and forwards skips are and match up the closest
  # ones to calculate duration of each skip.
  back_skips <- which(periods < 0)
  fore_skips <- which(periods > 2 * period)
  which_fore <- findInterval(back_skips, fore_skips)
  skip_dur <- as.numeric(prh@rawdata$datetimeUTC[back_skips + 1] -
                           prh@rawdata$datetimeUTC[fore_skips[which_fore]],
                         units = "secs")
  if (any(skip_dur > thr))
    warning("Gap greater than threshold found.")
  message(stringr::str_glue("{length(back_skips)} backwards skip(s) found, longest duration {format(max(skip_dur), digits = 3)} s."))

  # Remove skips
  skip_i <- purrr::map2(which_fore, back_skips, ~ (fore_skips[.x] + 1):.y)
  noskips <- dplyr::slice(prh@rawdata, -unlist(skip_i))
  # Add filler
  filler_fun <- function(fore, back) {
    fill_ts <- seq(prh@rawdata$datetimeUTC[fore],
                   prh@rawdata$datetimeUTC[back + 1],
                   by = period)
    # Drop the first timestamp to avoid duplication
    dplyr::tibble(datetimeUTC = fill_ts[-1])
  }
  fillers <- purrr::map2_dfr(fore_skips[which_fore], back_skips, filler_fun)
  newdata <- dplyr::bind_rows(noskips, fillers) %>%
    dplyr::arrange(datetimeUTC)

  result <- prh
  result@rawdata <- newdata
  result
}

#' Fix timestamp gaps
#'
#' \code{fix_gap} attempts to fix gaps in timesteps. Fix backwards time skips
#' (\code{\link{fix_backskip}}) before filling gaps.
#'
#' @param prh A PRH object.
#' @param thr A numeric defining the duration threshold (in sec) for warning
#'   about out-of-order phases (2 s by default).
#' @return A PRH object with the rawdata slot updated
#' @examples
#' \dontrun{
#' # There's an example at 2018-06-07 17:31:03.04 UTC in deployment mn180607-44
#' # Issues a warning there's out-of-order timestamps
#' prh <- init_prh(PRH("mn180607-44", "44"))
#' # Decimation fails
#' prh_deci <- decimate_prh(prh)
#' # Fix it!
#' prh_fix <- fix_backskip(prh)
#' # Now decimation works
#' prh_deci <- decimate_prh(prh_fix, 10)
#' }
#' @seealso \code{\link{import_cats}}, \link{\code{fix_backskip}}
fix_gap <- function(prh, thr = 2) {
  periods <- as.numeric(diff(prh@rawdata$datetimeUTC), units = "secs")
  # Assumes programmed frequency was a whole number e.g. 400 Hz
  freq <- round(1 / median(periods))
  period <- 1 / freq

  # Identify gaps
  back_skips <- which(periods < 0)
  if (length(back_skips) > 0)
    stop("Backward skips present. Use fix_backskips first.")
  gaps <- which(periods > 2 * period)
  gap_dur <- as.numeric(prh@rawdata$datetimeUTC[gaps + 1] -
                          prh@rawdata$datetimeUTC[gaps],
                        units = "secs")
  if (any(gap_dur > thr))
    warning("Gap greater than threshold found.")
  message(stringr::str_glue("{length(gaps)} gap(s) found, longest duration {format(max(gap_dur), digits = 3)} s."))

  # Fill in gaps
  filler_fun <- function(gap) {
    fill_ts <- seq(prh@rawdata$datetimeUTC[gap],
                   prh@rawdata$datetimeUTC[gap + 1],
                   by = period)
    # Drop the first (and last if necessary) timestamp to avoid duplication
    if (fill_ts[length(fill_ts)] == prh@rawdata$datetimeUTC[gap + 1])
      fill_ts <- fill_ts[-length(fill_ts)]
    dplyr::tibble(datetimeUTC = fill_ts[-1])
  }
  fillers <- purrr::map_dfr(gaps, filler_fun)
  newdata <- dplyr::bind_rows(prh@rawdata, fillers) %>%
    dplyr::arrange(datetimeUTC)

  result <- prh
  result@rawdata <- newdata
  result
}

#' Linearly interpolate gaps
#'
#' \code{interp_gaps} interpolates gaps in sensor readings. Time skips and gaps
#' should be corrected first.
#'
#' @param prh A PRH object.
#' @param thr A numeric defining the duration threshold (in sec) for maximum
#'   interpolation window.
#' @return A PRH object with the rawdata slot updated
interp_gaps <- function(prh, thr = 2) {
  stopifnot("PRH" %in% class(prh),
            is.numeric(thr),
            length(thr) == 1)

  # Look for longest gaps in acc, mag, gyr, and depth.
  longest_na <- function(vec) {
    vec_rl <- rle(is.na(vec))
    max(vec_rl$lengths[vec_rl$values])
  }
  # Convert number of records to duration in seconds
  to_secs <- function(n) n / prh@freq
  # Longest NA sequence within inertial (e.g. accX) and pressure sensors
  sensor_gaps <- prh@rawdata %>%
    dplyr::summarize_at(dplyr::vars(dplyr::matches("[a-z]{3}[XYZ]"),
                                    depthM),
                        dplyr::funs(longest_na)) %>%
    dplyr::mutate_all(dplyr::funs(to_secs))
  if (any(sensor_gaps > thr))
    stop("Sensor gap greater than threshold.")

  # Apply interpolation
  interp <- function(vec) {
    approx(seq_along(vec), vec, xout = seq_along(vec))$y
  }
  prh@rawdata <- prh@rawdata %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("[a-z]{3}[XYZ]"),
                                 depthM),
                     dplyr::funs(interp))
  prh
}
