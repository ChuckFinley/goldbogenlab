#' S4 class definition for PRH
#'
#' @include zzz.R
#' @slot tagid A string with the tag ID e.g. "mn180607-44".
#' @slot tagnum A string with the tag number e.g. "44"
#' @slot freq A double with the sampling frequency after decimation in Hz
#'   (typically 10 Hz).
#' @slot tagon,tagoff A POSIXct with the date and time of tag attach and detach
#'   in local time.
#' @slot t A POSIXct with the date and time of each record.
#' @slot depth A numeric with the depth records.
#' @slot rawA,rawM,rawG An nx3 numeric matrix with the raw accelerometer,
#'   magnetometer, and gyroscope records in enineering units.
#' @slot At,Mt,Gt As rawA, rawM, and rawG, but calibrated in units of g,
#'   microteslas, and radians per second, respectively in the tag's frame.
#' @slot Aw,Mw,Gw As At, Mt, and Gt, but rotated into the whale's frame.
#' @slot W A 3x3 numeric matrix to rotate tag frame into whale frame.
#' @slot pitch,roll,head Numeric vectors with the pitch, roll, and heading.
#' @slot speedJJ,speedFN Numeric vectors with speed estimates derived from
#'   jiggle and flow noise, respectively.
#' @slot rawdata A tibble with the (decimated) raw data. Removed at the end of
#'   the PRH creation process.
.PRH <- setClass("PRH",
  slots = c(tagid = "character",
            tagnum = "character",
            freq = "numeric",
            tagon = "POSIXct",
            tagoff = "POSIXct",
            t = "POSIXct",
            depth = "numeric",
            rawA = "matrix",
            rawM = "matrix",
            rawG = "matrix",
            At = "matrix",
            Mt = "matrix",
            Gt = "matrix",
            Aw = "matrix",
            Mw = "matrix",
            Gw = "matrix",
            W = "matrix",
            pitch = "numeric",
            roll = "numeric",
            head = "numeric",
            speedJJ = "numeric",
            speedFN = "numeric",
            rawdata = "data.frame"),
  prototype = list(tagon = num_to_POSIX(0),
                   tagoff = num_to_POSIX(0),
                   t = num_to_POSIX(0))
)

#' Constructor for PRH class
#'
#' @param tagid A string with the tagid (e.g. "mn180105-22a").
#'
#' @return An object of class PRH with the tagid and tagnum slots filled.
PRH <- function(tagid, tagnum) {
  if (!is.character(tagid) ||
      length(tagid) != 1) {
    stop("tagid must be a length one string.")
  }
  if (!is.character(tagnum) ||
      length(tagnum) != 1) {
    stop("tagnum must be a length one string.")
  }
  if (all(is.na(stringr::str_match(tagid, tagnum)))) {
    warning(stringr::str_glue("WARNING: tagnum \"{tagnum}\" not found in tagid \"{tagid}\"."))
  }

  .PRH(tagid = tagid,
       tagnum = tagnum)
}

#' Initiate a PRH \code{init_prh} tries to load raw tag data and decimate it (by
#' default to 10 Hz).
#'
#' Initiating the PRH process requires importing the raw data and decimating it
#' to a manageable frequency. The next step is trimming to the tag on/off times.
#'
#' @param prh A PRH object.
#' @param new_freq The decimated sample frequency in Hz (by default 10 Hz).
#' @return A PRH object with the following additional slots filled:
#' \itemize{
#'   \item freq
#'   \item rawdata
#' }
#' @seealso \link{\code{trim_data}}, \link{\code{import_cats}}, \link{\code{decimate}}
init_prh <- function(prh, new_freq) {
  if (!("PRH" %in% class(prh)))
    stop("prh must be an object of class PRH.")

  tryCatch({
    prh@freq <- new_freq
    imported <- import_cats(prh@tagid)
    decimated <- decimate(imported, new_freq)
    prh@rawdata <- decimated
  }, error = function(e) {
    stop(stringr::str_glue("Error initiating tagid {prh@tagid}:\n{e$call}\n{e$message}"))
  })
}

#' Decimate raw data
#'
#' \code{decimate} returns a subset of the raw data at a lower sampling
#' frequency.
#'
#' Decimation chooses every nth record, so the new sampling frequency must be a
#' factor of the original. E.g. 400 Hz can be decimated to 10 Hz but not to 12
#' Hz.
#'
#' @param raw_data A tibble of the raw data (e.g. returned from
#'   \code{import_cats})
#' @param new_freq The new sampling frequency. Must be a factor of the old
#'   sampling frequency.
#' @return A tibble with the same fields as the input.
#' @seealso \link{\code{import_cats}}
decimate <- function(raw_data, new_freq) {
  if (!("data.frame" %in% class(raw_data)))
    stop("raw_data must be a tibble.")
  if (nrow(raw_data) < 2)
    stop("raw_data must have at least two rows.")
  if (!all(c("dateUTC", "timeUTC") %in% colnames(raw_data)))
    stop("Columns dateUTC, timeUTC not found in raw_data.")
  if (class(new_freq) != "numeric" ||
      length(new_freq) != 1)
    stop("new_freq must be a length one numeric vector.")

  two_times <- with(raw_data, lubridate::dmy_hms(paste(dateUTC[1:2],
                                                       timeUTC[1:2])))
  old_period <- raw_data$datetimeUTC[2] - raw_data$datetimeUTC[1]
  old_freq <- (1 / as.numeric(old_period, units = "secs")) %>%
    round
  message(stringr::str_glue("Old frequency inferred to be {old_freq} Hz."))

  if (old_freq %% new_freq != 0)
    stop("New frequency is not a factor of old frequency.")

  slice(raw_data, seq(1, nrow(raw_data), by = old_freq / new_freq))
}

#' Trim data to tag on period
#'
#' \code{trim_data} removes data from before tag on time and after tag off time.
#' Optionally displays a GUI for interactive tag on/off time selection.
#'
#' @import shiny
#'
#' @param raw_data A tibble of the raw data (e.g. returned from
#'   \code{import_cats})
#' @param use_gui A logical indicating whether to use the interactive plot for
#'   tag on/off time selection.
#' @param tagon,tagoff A POSIXct with the tag on/off time. Ignored if
#'   \code{use_gui} is TRUE (default).
#' @return A tibble with rows before tag on and after tag off removed.
trim_data <- function(rawdata, use_gui = TRUE, tagon = NA, tagoff = NA) {
  if (!("data.frame" %in% class(raw_data)))
    stop("raw_data must be a tibble.")
  if (nrow(raw_data) == 0)
    stop("raw_data is empty.")
  if (!use_gui) {
    if (!("POSIXct" %in% class(tagon)) ||
        length(tagon) > 1)
      stop("tagon must be a POSIXct of length 1.")
    if (!("POSIXct" %in% class(tagoff)) ||
        length(tagoff) > 1)
      stop("tagoff must be a POSIXct of length 1.")
    if (!all(c("dateUTC", "timeUTC") %in% colnames(rawdata)))
      stop("Columns dateUTC, timeUTC not found in raw_data.")
    list(tagon = tagon,
         tagoff = tagoff,
         data = rawdata %>%
           dplyr::filter(between(datetimeUTC, tagon, tagoff)))
  } else {
    result <- tagonoff(rawdata)
    list(tagon = result[1],
         tagoff = result[2],
         data = rawdata %>%
           dplyr::filter(between(datetimeUTC, result[1], result[2])))
  }
}
