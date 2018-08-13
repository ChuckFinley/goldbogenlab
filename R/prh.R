#' S4 class definition for PRH
#'
#' @slot tagid A string with the tag ID e.g. "mn180607-44".
#' @slot tagnum A string with the tag number e.g. "44"
#' @slot freq A double with the sampling frequency after decimation in Hz
#'   (typically 10 Hz).
#' @slot tagon,tagoff A POSIXct with the date and time of tag attach and detach
#'   in local time.
#' @slot t A POSIXct with the date and time of each record.
#' @slot rawA,rawM,rawG An nx3 numeric matrix with the raw accelerometer,
#'   magnetometer, and gyroscope records in enineering units.
#' @slot At,Mt,Gt As rawA, rawM, and rawG, but calibrated in units of g,
#'   microteslas, and radians per second, respectively in the tag's frame.
#' @slot Aw,Mw,Gw As At, Mt, and Gt, but rotated into the whale's frame.
#' @slot W A 3x3 numeric matrix to rotate tag frame into whale frame.
#' @slot pitch,roll,head Numeric vectors with the pitch, roll, and heading.
#' @slot speedJJ,speedFN Numeric vectors with speed estimates derived from
#'   jiggle and flow noise, respectively.
.PRH <- setClass("PRH",
  slots = c(tagid = "character",
            tagnum = "character",
            freq = "numeric",
            tagon = "POSIXct",
            tagoff = "POSIXct",
            t = "POSIXct",
            p = "numeric",
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
            speedFN = "numeric")
)

#' Constructor for PRH class
#'
#' @param tagid A string with the tagid (e.g. "mn180105-22a").
#'
#' @return An object of class PRH.
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

  .PRH(tagid = tagid)
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

#' Apply bench calibrations
#'
#' \code{bench_calib} applies bench calibrations to the raw accelerometer,
#' magnetometer, and gyroscope data. Outputs will be in units of g, microtesla,
#' and radians per second, respectively. For efficiency's sake, should only be
#' applied to decimated data.
#'
#' @param raw_data A tibble of the raw data (e.g. returned from \code{decimate})
#' @param cal A list with elements acal, mcalon, mcaloff, and gcal. Each element
#'   is a 2x3 matrix with the calibration slopes and constants in the (first and
#'   second) rows and the x, y, and z values in the columns.
#' @return A list of three nx3 matrices (acc, mag, and gyr) with the calibrated
#'   records.
bench_calib <- function(raw_data, cal = NA) {
  if (!("data.frame" %in% class(raw_data)))
    stop("raw_data must be a tibble.")
  if (nrow(raw_data) == 0)
    stop("raw_data must not be empty.")

  # Hard-coding calibration matrices for now (this for tag 44)
  cal <- list(acal = matrix(c(0.1016, 0.1017, 0.1000,
                              0.1109, -0.0901, -0.2223),
                            byrow = TRUE,
                            nrow = 2),
              mcalon = matrix(c(1.0074, 1.0041, 0.9097,
                                14.3739, -46.9811, -35.1598),
                              byrow = TRUE,
                              nrow = 2),
              mcaloff = matrix(c(1.0525, 1.0116, 0.9218,
                                 14.1094, -45.9193, -27.8027),
                               byrow = TRUE,
                               nrow = 2),
              gcal = matrix(c(0.0010, 0.0010, 0.0010,
                              -5.8200, -30.0188, 5.4928),
                            byrow = TRUE,
                            nrow = 2))

  all_cal_valid <- cal %>%
    purrr::map(function(mat) {
      class(mat) == "matrix" &&
        typeof(mat) == "double" &&
        all(dim(mat) == c(2, 3))
    }) %>%
    as.logical %>%
    all
  if (!all_cal_valid)
    stop("All calibrations must be 2x3 numeric matrices.")

  apply_cal <- function(records, cal) {
    t(apply(records, 1, function(row) row * cal[1,] + cal[2,]))
  }

  mag_camon <- apply_cal(dplyr::select(raw_data, magX:magZ),
                         cal$mcalon)
  mag_camoff <- apply_cal(dplyr::select(raw_data, magX:magZ),
                          cal$mcaloff)
  mag <- mag_camoff
  mag[raw_data$ccStatus == "R--",] <- mag_camon[raw_data$ccStatus == "R--",]

  list(acc = apply_cal(dplyr::select(raw_data, accX:accZ),
                       cal$acal),
       mag = mag,
       gyr = apply_cal(dplyr::select(raw_data, gyrX:gyrZ),
                       cal$gcal))
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
#' @param tagon,tagoff A POSIXct with the tag on/off time. Ignored if
#'   \code{use_gui} is TRUE.
#' @param use_gui A logical indicating whether to use the interactive plot for
#'   tag on/off time selection.
#' @return A tibble with rows before tag on and after tag off removed.
trim_data <- function(raw_data, tagon, tagoff, use_gui = FALSE) {
  if (!("data.frame" %in% class(raw_data)))
    stop("raw_data must be a tibble.")
  if (nrow(raw_data) == 0)
    stop("raw_data is empty.")
  if (!("POSIXct" %in% class(tagon)) ||
      length(tagon) > 1)
    stop("tagon must be a POSIXct of length 1.")
  if (!("POSIXct" %in% class(tagoff)) ||
      length(tagoff) > 1)
    stop("tagoff must be a POSIXct of length 1.")
  if (!all(c("dateUTC", "timeUTC") %in% colnames(raw_data)))
    stop("Columns dateUTC, timeUTC not found in raw_data.")

  if (!use_gui) {
    raw_data %>%
      dplyr::filter(between(datetimeUTC, tagon, tagoff))
  } else {
    # THANK YOU:
    # https://gallery.shinyapps.io/105-plot-interaction-zoom/
    # https://github.com/daattali/advanced-shiny/blob/master/close-window/app.R

    # For performance, downsample to 2e3 points per plot
    resolution <- 2e3
    raw_data2 <- slice(raw_data, seq(1, nrow(raw_data), length.out = resolution))

    close_window <- "shinyjs.close_window = function() { window.close(); }"

    ui <- shiny::fluidPage(
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = close_window, functions = "close_window"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::tags$p("Tag on time:"),
          shiny::textOutput("tagon"),
          shiny::tags$p("Tag off time:"),
          shiny::textOutput("tagoff"),
          shiny::actionButton("close", "Close"),
          width = 3
        ),
        shiny::mainPanel(
          shiny::plotOutput("whole_depth_plot",
                            brush = "zoom_depth_brush",
                            height = 150),
          shiny::plotOutput("whole_acc_plot",
                            brush = "zoom_acc_brush",
                            height = 150),
          shiny::plotOutput("zoom_depth_plot",
                            click = "tagon_click",
                            dblclick = "tagoff_click",
                            hover = "zoom_hover",
                            height = 150),
          shiny::plotOutput("zoom_acc_plot",
                            click = "tagon_click",
                            dblclick = "tagoff_click",
                            hover = "zoom_hover",
                            height = 150)
        )
      )
    )

    server <- function(input, output) {
      tagtimes <- shiny::reactiveValues(tagon = NULL,
                                        tagoff = NULL)
      zoom_range <- shiny::reactiveValues(x = NULL,
                                          depth_y = NULL,
                                          acc_y = NULL,
                                          zoom_data = raw_data2)
      zoom_guide <- shiny::reactiveValues(x = NULL)

      # Overview plots
      output$whole_depth_plot <- shiny::renderPlot({
        raw_data2 %>%
          ggplot2::ggplot(ggplot2::aes(datetimeUTC, depthM)) +
          ggplot2::geom_line() +
          ggplot2::scale_y_reverse() +
          ggplot2::theme_classic() +
          ggplot2::labs(x = "Time (UTC)",
                        y = "Depth (m)")
      })
      output$whole_acc_plot <- shiny::renderPlot({
        raw_data2 %>%
          tidyr::gather(axis, value, accX:accZ) %>%
          ggplot2::ggplot(ggplot2::aes(datetimeUTC, value, color = axis)) +
          ggplot2::geom_line() +
          ggplot2::scale_y_reverse() +
          ggplot2::theme_classic() +
          ggplot2::labs(x = "Time (UTC)",
                        y = "Acceleration (g)") +
          ggplot2::theme(legend.position = "none")
      })

      # Zoomed plots
      output$zoom_depth_plot <- shiny::renderPlot({
        vline <- if (!is.null(zoom_guide$x)) {
          ggplot2::geom_vline(xintercept = zoom_guide$x)
        } else {
          NULL
        }
        zoom_range$zoom_data %>%
          ggplot2::ggplot(ggplot2::aes(datetimeUTC, depthM)) +
          ggplot2::geom_line() +
          vline +
          ggplot2::scale_y_reverse() +
          ggplot2::theme_classic() +
          ggplot2::labs(x = "Time (UTC)",
                        y = "Depth (m)") +
          ggplot2::coord_cartesian(xlim = zoom_range$x,
                                   ylim = zoom_range$depth_y,
                                   expand = FALSE)
      })
      output$zoom_acc_plot <- shiny::renderPlot({
        vline <- if (!is.null(zoom_guide$x)) {
          ggplot2::geom_vline(xintercept = zoom_guide$x)
        } else {
          NULL
        }
        zoom_range$zoom_data %>%
          tidyr::gather(axis, value, accX:accZ) %>%
          ggplot2::ggplot(ggplot2::aes(datetimeUTC, value, color = axis)) +
          ggplot2::geom_line() +
          vline +
          ggplot2::scale_y_reverse() +
          ggplot2::theme_classic() +
          ggplot2::labs(x = "Time (UTC)",
                        y = "Acceleration (g)") +
          ggplot2::theme(legend.position = "none") +
          ggplot2::coord_cartesian(xlim = zoom_range$x,
                                   ylim = zoom_range$acc_y,
                                   expand = FALSE)
      })

      # Tag on/off times
      output$tagon <- shiny::renderText({
        tagon <- input$tagon_click$x
        if (!is.null(tagon)) {
          tagtimes$tagon <- tagon %>%
            num_to_POSIX
        }
        if (!is.null(tagtimes$tagon))
          format(tagtimes$tagon, "%Y-%m-%d %H:%M:%OS3")
      })
      output$tagoff <- shiny::renderText({
        tagoff <- input$tagoff_click$x
        if (!is.null(tagoff)) {
          tagtimes$tagoff <- tagoff %>%
            num_to_POSIX
        }
        if (!is.null(tagtimes$tagoff))
          format(tagtimes$tagoff, "%Y-%m-%d %H:%M:%OS3")
      })

      # Link overview and zoom plots with brushing
      observe({
        brush <- input$zoom_depth_brush
        if (!is.null(brush)) {
          zoom_range$x <- num_to_POSIX(c(brush$xmin, brush$xmax))
          zoom_range$depth_y <- c(brush$ymin, brush$ymax)
          zoom_range$acc_y <- NULL
          zoom_i <- findInterval(zoom_range$x, raw_data$datetimeUTC)
          zoom_range$zoom_data <- raw_data %>%
            slice(seq(zoom_i[1], zoom_i[2], length.out = resolution))
        } else {
          zoom_range$x <- NULL
          zoom_range$depth_y <- NULL
          zoom_range$acc_y <- NULL
          zoom_range$zoom_data <- raw_data2
        }
      })
      observe({
        brush <- input$zoom_acc_brush
        if (!is.null(brush)) {
          zoom_range$x <- num_to_POSIX(c(brush$xmin, brush$xmax))
          zoom_range$acc_y <- c(brush$ymin, brush$ymax)
          zoom_range$depth_y <- NULL
          zoom_i <- findInterval(zoom_range$x, raw_data$datetimeUTC)
          zoom_range$zoom_data <- raw_data %>%
            slice(seq(zoom_i[1], zoom_i[2], length.out = resolution))
        } else {
          zoom_range$x <- NULL
          zoom_range$depth_y <- NULL
          zoom_range$acc_y <- NULL
          zoom_range$zoom_data <- raw_data2
        }
      })

      # Hovering on zoom plots brings up a vertical guide
      observe({
        hover_x <- input$zoom_hover$x
        if (!is.null(hover_x))
          zoom_guide$x <- num_to_POSIX(hover_x)
      })

      # Close window
      observeEvent(input$close, {
        shinyjs::js$close_window()
        shiny::stopApp(reactiveValuesToList(tagtimes))
      })
    }

    result <- shiny::runApp(shiny::shinyApp(ui, server))
  }
}
