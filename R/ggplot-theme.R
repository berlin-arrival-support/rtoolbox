#' @title Color Palette
bas_color_palette <- list(
  qualitative = c(),
  gradient_1 = c(),
  gradient_2 = c()
)


#' @title Add BAS Logo
#'
#' @description
#' @export
add_bas_logo <- function() {
  structure(
    list(),
    class = "logo_patch",
    fn = "add_bas_logo_"
  )
}

#' @importFrom rlang `%||%`
add_bas_logo_ <- function(p) {
  base_size <- p$theme$text$size %||% 11
  logo_width <- base_size * 2.1
  p$labels$caption <- p$labels$caption %||% ""

  logo <- readPicture(system.file("img", "bas-icon.svg", package = "rtoolbox"))
  logo_grob <- symbolsGrob(
    logo,
    x = unit(1, "npc") - unit(logo_width / 3, "pt"),
    y = unit(0, "npc") + unit(logo_width / 3, "pt"),
    size = unit(logo_width, "pt")
  )

  plot_margin <- unit(base_size * 1.4, "pt")

  p + inset_element(
    logo_grob,
    left = unit(1, "npc") - plot_margin - unit(logo_width, "pt"),
    bottom = unit(0, "npc") + plot_margin,
    right = unit(1, "npc") - plot_margin,
    top = unit(0, "npc") + plot_margin + unit(logo_width, "pt"),
    align_to = "full",
    clip = FALSE,
    on_top = FALSE
  )
}
