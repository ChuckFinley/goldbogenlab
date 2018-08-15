#' S4 class definition for PRH
#'
#' @include zzz.R
#' @slot tagid A string with the tag ID e.g. "mn180607-44".
#' @slot tagnum A string with the tag number e.g. "44"
#' @slot freq A double with the sampling frequency after decimation in Hz
#'   (typically 10 Hz).
#' @slot tagon,tagoff A POSIXct with the date and time of tag attach and detach
#'   in local time.
#' @slot slips A POSIXct vector with the timestamps of each tag slip.
#' @slot W A list of 3x3 numeric matrices to rotate tag frame into whale frame.
#'   One matrix for each tag orientation (changes on slips).
#' @slot data A tibble with the following columns: \itemize{ \item t A POSIXct
#'   with the date and time of each record. \item p A numeric with the depth
#'   records. \item rawA,rawM,rawG List columns with nx3 numeric matrices with
#'   the raw accelerometer, magnetometer, and gyroscope records in engineering
#'   units. \item At,Mt,Gt As rawA, rawM, and rawG, but calibrated in units of
#'   g, microteslas, and radians per second, respectively in the tag's frame.
#'   \item Aw,Mw,Gw As At, Mt, and Gt, but rotated into the whale's frame. \item
#'   pitch,roll,head Numerics with the pitch, roll, and heading. \item
#'   speedJJ,speedFN Numerics with speed estimates derived from jiggle and flow
#'   noise, respectively.}
#' @slot rawdata A tibble with the (decimated) raw data. Removed at the end of
#'   the PRH creation process.
.PRH <- setClass("PRH",
  slots = c(tagid = "character",
            tagnum = "character",
            freq = "numeric",
            tagon = "POSIXct",
            tagoff = "POSIXct",
            slips = "POSIXct",
            W = "list",
            data = "data.frame",
            rawdata = "data.frame"),
  prototype = list(tagon = num_to_POSIX(0),
                   tagoff = num_to_POSIX(0),
                   slips = num_to_POSIX(0))
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

#' Initiate a PRH
#'
#' \code{init_prh} tries to load raw tag data.
#'
#' Initiating the PRH process finds the raw data on the CATS drive, imports it,
#' and tidies up the names. The next step is decimating the data to a manageable
#' frequency.
#'
#' @param prh A PRH object.
#' @return A PRH object with the rawdata slot filled
#' @examples
#' \dontrun{
#' prh0 <- PRH("mn180607-44", "44")
#' prh_rawdata <- init_prh(prh0)
#' }
#' @seealso \link{\code{import_cats}}, \link{\code{decimate}}
init_prh <- function(prh) {
  if (!("PRH" %in% class(prh)))
    stop("prh must be a PRH object.")

  tryCatch({
    prh@rawdata <- import_cats(prh@tagid)
    prh
  }, error = function(e) {
    stop(stringr::str_glue("Error initiating tagid {prh@tagid}:\n{e$call}\n{e$message}"))
  })
}

#' Decimate raw data
#'
#' \code{decimate_prh} returns a subset of the raw data at a lower sampling
#' frequency.
#'
#' Decimation chooses every nth record, so the new sampling frequency must be a
#' factor of the original. E.g. 400 Hz can be decimated to 10 Hz but not to 12
#' Hz.
#'
#' @param prh A PRH object.
#' @param new_freq The new sampling frequency. Must be a factor of the old
#'   sampling frequency.
#' @return A PRH with the freq slot filled and rawdata decimated to the new
#'   frequency.
#' @seealso \link{\code{import_cats}}
decimate_prh <- function(prh, new_freq) {
  if (!("PRH" %in% class(prh)))
    stop("prh must be a PRH object.")
  if (nrow(prh@rawdata) < 2)
    stop("PRH raw data must have at least two records.")
  if (!("datetimeUTC" %in% colnames(prh@rawdata)))
    stop("Column datetimeUTC not found in raw data.")
  if (class(new_freq) != "numeric" ||
      length(new_freq) != 1)
    stop("new_freq must be a length one numeric vector.")

  rawdata <- prh@rawdata

  # Test for timestamp issues
  if (is.unsorted(rawdata$datetimeUTC))
    stop("Raw data timestamps out of order, fix before continuing.")
  if (any(as.numeric(diff(rawdata$datetimeUTC)) > 120))
    stop("Raw data has gap(s) greater than two minutes, fix before continuing.")

  # Infer original sampling frequency over a small window
  infer_period <- 1:min(nrow(rawdata), 100)
  old_period <- median(diff(rawdata$datetimeUTC[infer_period]))
  old_freq <- round(1 / as.numeric(old_period, units = "secs"))
  message(stringr::str_glue("Old frequency inferred to be {old_freq} Hz."))

  if (old_freq %% new_freq != 0)
    stop("New frequency is not a factor of old frequency.")

  prh@rawdata <- dplyr::slice(rawdata, seq(1, nrow(rawdata), by = old_freq / new_freq))
  prh
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
           dplyr::filter(dplyr::between(datetimeUTC, tagon, tagoff)))
  } else {
    result <- tagonoff(rawdata)
    list(tagon = result[1],
         tagoff = result[2],
         data = rawdata %>%
           dplyr::filter(dplyr::between(datetimeUTC, result[1], result[2])))
  }
}
