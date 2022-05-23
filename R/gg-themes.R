#' Themes for ggplot2
#'
#' @inheritParams ggplot2::theme_minimal
#' @param grid panel grid to be shown ("none" or a combination of "X", "Y", "x", and "y")
#'
#' @export
theme_bas <- function(base_size = 14,
                      base_line_size = base_size / 28,
                      base_rect_size = base_size / 28,
                      grid = "XY") {
  if (length(grid) != 1L || !grepl("^(none|[XYxy]+)$", grid)) {
    stop('`grid` must be a string: "none" or any combination of "X", "Y", "x", and "y"')
  }

  structure(
    list(),
    class = "bas_theme",
    fn = "theme_bas_",
    args = list(
      base_size = base_size,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size,
      grid = grid
    )
  )
}

theme_bas_ <- function(p, base_size, base_line_size, base_rect_size, grid) {
  if (isTRUE(nzchar(p$labels$x))) {
    p$labels$x <- paste(p$labels$x, "\u2192")
  }

  if (isTRUE(nzchar(p$labels$y))) {
    p$labels$y <- paste(p$labels$y, "\u2191")
  }

  t <- ggplot2::theme_minimal(
    base_size = base_size,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = "Inter tnum",
        colour = "grey10"
      ),
      line = ggplot2::element_line(colour = "grey80"),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.5),
        margin = ggplot2::margin(b = base_size * 2 / 3),
      ),
      plot.subtitle = ggplot2::element_text(
        lineheight = 1.2,
        margin = ggplot2::margin(b = base_size)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        margin = ggplot2::margin(t = base_size * 2 / 3)
      ),
      axis.title.x = ggplot2::element_text(
        face = "bold",
        hjust = 1,
        margin = ggplot2::margin(t = base_size * 2 / 3)
      ),
      axis.title.y = ggplot2::element_text(
        face = "bold",
        angle = 0,
        vjust = 1.07,
        margin = grid::unit.c(
          grid::unit(base_size * 2 / 3, "pt"), # FIXME: doesn't have any effect
          grid::unit(-1, "strwidth", data = p$labels$y),
          grid::unit(base_size * 2 / 3, "pt"),
          grid::unit(0, "pt")
        )
      ),
      axis.text.x = ggplot2::element_text(
        colour = "grey45",
        margin = ggplot2::margin(t = base_size / 3)
      ),
      axis.text.y = ggplot2::element_text(
        colour = "grey45",
        margin = ggplot2::margin(r = base_size / 3)
      ),
      legend.key.size = grid::unit(base_size * 1.1, "pt"),
      legend.title = ggplot2::element_text(
        face = "bold",
        colour = "grey45",
        vjust = grid::unit(1, "npc") - grid::unit(base_size / 14, "pt")
      ),
      legend.text = ggplot2::element_text(colour = "grey45"),
      legend.position = "top",
      legend.justification = "left",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = ggplot2::rel(1.05)),
      panel.spacing = ggplot2::unit(base_size, "pt"),
      panel.grid.major = ggplot2::element_line(size = ggplot2::rel(.7)),
      panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(.7)),
      panel.background = ggplot2::element_rect(
        fill = "transparent", colour = "transparent"
      ),
      plot.background = ggplot2::element_rect(
        fill = "transparent", colour = "transparent"
      ),
      plot.margin = ggplot2::margin(
        t = base_size * 1.4, r = base_size * 1.4,
        b = base_size * 1.4, l = base_size * 1.4
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )

  if (!grepl("X", grid)) {
    t <- t + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  }
  if (!grepl("Y", grid)) {
    t <- t + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  }
  if (!grepl("x", grid)) {
    t <- t + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
  }
  if (!grepl("y", grid)) {
    t <- t + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  }

  p + t
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.bas_theme <- function(object, p, objectname) {
  do.call(
    attr(object, "fn"),
    c(list(p), attr(object, "args"))
  )
}
