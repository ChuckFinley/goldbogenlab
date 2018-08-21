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

plot_acc <- function(prh) {
  prh@At %>%
    data.frame %>%
    dplyr::rename(x = X1, y = X2, z = X3) %>%
    cbind(prh@data) %>%
    # Downsample to 5e3 records
    downsample(length.out = 5e3) %>%
    tidyr::gather(axis, value, x:z) %>%
    ggplot2::ggplot(ggplot2::aes(t, value, color = axis)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "", y = "Acceleration (g)") +
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

plot_acc2 <- function(prh) {
  gridExtra::grid.arrange(plot_depth(prh),
                          plot_acc(prh),
                          nrow = 2,
                          heights = c(2,3))
}
