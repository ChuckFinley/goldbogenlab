#' Tag on/off GUI
#'
#' \code{tagonoff} is a Shiny app that allows the user to zoom in on pressure
#' and acceleration to choose the tag on and off times.
#'
#' @param rawdata A tibble of raw tag data (e.g. returned from
#'   \code{import_cats})
tagonoff <- function(rawdata) {

  # THANK YOU:
  # https://gallery.shinyapps.io/105-plot-interaction-zoom/
  # https://github.com/daattali/advanced-shiny/blob/master/close-window/app.R

  if(is.unsorted(rawdata$datetimeUTC))
    stop("Out-of-order timestamps")

  # For performance, downsample to 2e3 points per plot
  res <- 2e3
  rawdata2 <- dplyr::slice(rawdata, seq(1, nrow(rawdata), length.out = res))

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
                                        zoom_data = rawdata2)
    zoom_guide <- shiny::reactiveValues(x = NULL)

    # Overview plots
    output$whole_depth_plot <- shiny::renderPlot({
      rawdata2 %>%
        ggplot2::ggplot(ggplot2::aes(datetimeUTC, depthM)) +
        ggplot2::geom_line() +
        ggplot2::scale_y_reverse() +
        ggplot2::theme_classic() +
        ggplot2::labs(x = "Time (UTC)",
                      y = "Depth (m)")
    })
    output$whole_acc_plot <- shiny::renderPlot({
      rawdata2 %>%
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
        zoom_i <- findInterval(zoom_range$x, rawdata$datetimeUTC)
        zoom_range$zoom_data <- rawdata %>%
          slice(seq(zoom_i[1], zoom_i[2], length.out = res))
      } else {
        zoom_range$x <- NULL
        zoom_range$depth_y <- NULL
        zoom_range$acc_y <- NULL
        zoom_range$zoom_data <- rawdata2
      }
    })
    observe({
      brush <- input$zoom_acc_brush
      if (!is.null(brush)) {
        zoom_range$x <- num_to_POSIX(c(brush$xmin, brush$xmax))
        zoom_range$acc_y <- c(brush$ymin, brush$ymax)
        zoom_range$depth_y <- NULL
        zoom_i <- findInterval(zoom_range$x, rawdata$datetimeUTC)
        zoom_range$zoom_data <- rawdata %>%
          slice(seq(zoom_i[1], zoom_i[2], length.out = res))
      } else {
        zoom_range$x <- NULL
        zoom_range$depth_y <- NULL
        zoom_range$acc_y <- NULL
        zoom_range$zoom_data <- rawdata2
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
  if (!("POSIXct" %in% class(result)) ||
      length(result != 2))
    stop("Error in tagonoff, result is not a POSIXct of length 2.")
  if (result[2] < result[1])
    stop("Error in tagonoff, tag on time is after tag off time.")
  result
}
