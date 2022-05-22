register_inter_tnum <- function() {
  if (inter_is_installed()) {
    family <- "Inter"

    fonts <- systemfonts::system_fonts()
    fonts <- fonts[fonts$family == family &
                     fonts$style %in% c("Regular", "Bold", "Italic", "Bold Italic"), ]
    fonts <- fonts[order(fonts$style), ]
    fonts <- as.list(stats::setNames(fonts$path, c("bold", "bolditalic", "italic", "plain")))

    do.call(
      systemfonts::register_font,
      c(fonts, list(
        name = "Inter tnum",
        features = systemfonts::font_feature(numbers = "tabular")
      ))
    )
  }
}

inter_is_installed <- function() {
  family <- "Inter"
  matched <- match_font(family)
  installed <- matched == family

  if (!installed) {
    message(
      "Inter (https://rsms.me/inter) is not installed.\n",
      "`theme_bas` will use system's default font (",
      match_font(""),
      ") instead.\n"
    )
  }

  installed
}

match_font <- function(x) systemfonts::font_info(x)[["family"]][[1]]
