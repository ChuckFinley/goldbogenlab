#' Fix out-of-order timestamps
#'
#' \code{fix_ooo} attempts to fix out-of-order timesteps.
#'
#' Sometimes CATS timesteps skip around. \code{fix_ooo} uses a running diff of
#' the timestamps to look for phases with a large positive diff followed by a
#' negative diff. These skips are then removed and re-interpolated if shorter
#' than a threshold (2s by default).
#'
#' @param prh A PRH object.
#' @param thr A numeric defining the duration threshold (in sec) for
#'   re-interpolating out-of-order phases.
#' @return A PRH object with the rawdata slot updated
#' @examples
#' \dontrun{
#' # Issues a warning there's out-of-order timestamps
#' prh <- init_prh(PRH("mn180607-44", "44"))
#' # Decimation fails
#' prh_deci <- decimate_prh(prh)
#' # Fix it!
#' prh_fix <- fix_ooo(prh)
#' # Now decimation works
#' prh_deci <- decimate_prh(prh_fix, 10)
#' }
#' @seealso \link{\code{import_cats}}
fix_ooo <- function(prh, thr = 2) {
  # Infer period as the median timestamp diff
  ts_diff <- as.numeric(diff(prh@rawdata$datetimeUTC), units = "secs")
  period <- median(ts_diff)

  # Identify where the backwards and forwards skips are and match up the closest
  # ones to calculate duration of each skip.
  back_skips <- which(ts_diff < 0)
  fore_skips <- which(ts_diff > 2 * period)
  which_fore <- findInterval(back_skips, fore_skips)
  skip_dur <- (back_skips - fore_skips[which_fore]) * period
  if (any(skip_dur > thr))
    stop("Gap greater than threshold found.")
  message(stringr::str_glue("{length(back_skips)} backwards skip(s) found, longest duration {format(max(skip_dur), digits = 3)} s."))

  # Remove skips
  skip_i <- purrr::map2(back_skips, which_fore, ~ fore_skips[.y]:.x)
  noskips <- dplyr::slice(prh@rawdata, -unlist(skip_i))
  # Add filler
  filler_fun <- function(lims) {
    fill_ts <- seq(prh@rawdata$datetimeUTC[first(lims) - 1],
                   prh@rawdata$datetimeUTC[last(lims) + 1],
                   by = period)
    dplyr::tibble(datetimeUTC = fill_ts)
  }
  fillers <- purrr::map_dfr(skip_i, filler_fun)
  newdata <- dplyr::bind_rows(noskips, fillers) %>%
    dplyr::arrange(datetimeUTC)

  result <- prh
  result@rawdata <- newdata
  result
}
