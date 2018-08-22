### These functions are scratch for now
downsample <- function(df, by = NULL, length.out = NULL) {
  if (!is.null(by)) {
    i <- seq(1, nrow(df), by = by)
  } else if (!is.null(length.out)) {
    i <- seq(1, nrow(df), length.out = length.out)
  } else {
    stop("Either by or length.out must be not null.")
  }
  dplyr::slice(df, i)
}

plot_sensor <- function(prh, sensor) {
  stopifnot(sensor %in% c("acc", "mag", "gyr"))
  sensor_mat <- switch(sensor,
                       acc = prh@At,
                       mag = prh@Mt,
                       gyr = prh@Gt)
  sensor_lbl <- switch(sensor,
                       acc = "Acceleration (g)",
                       mag = "Magnetism (microtesla)",
                       gyr = "Rotation (rad/s)")

  sensor_mat %>%
    dplyr::as_tibble() %>%
    dplyr::rename(x = V1, y = V2, z = V3) %>%
    cbind(prh@data) %>%
    # Downsample to 5e3 records
    downsample(length.out = 5e3) %>%
    tidyr::gather(axis, value, x:z) %>%
    ggplot2::ggplot(ggplot2::aes(t, value, color = axis)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = sensor_lbl) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.direction = "horizontal",
                   legend.position = c(0.1, 0.1))
}

plot_depth <- function(prh) {
  prh@data %>%
    # Downsample to 5e3 records
    downsample(length.out = 5e3) %>%
    ggplot2::ggplot(ggplot2::aes(t, p)) +
    ggplot2::geom_line() +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = "", y = "Depth (m)") +
    ggplot2::theme_classic()
}

plot_sensor2 <- function(prh, sensor) {
  gridExtra::grid.arrange(plot_depth(prh),
                          plot_sensor(prh, sensor),
                          nrow = 2,
                          heights = c(2,3))
}

plot_vec_mat <- function(vec, mat) {
  data.frame(mat) %>%
    dplyr::mutate(x = vec) %>%
    tidyr::gather("key", "value", -x) %>%
    ggplot2::ggplot(ggplot2::aes(x, value, color = key)) +
    ggplot2::geom_line() +
    ggplot2::theme_classic()
}
