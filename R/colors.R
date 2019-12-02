
# do not @export
get_colors <- function(x) {

  files <- list.files(path = "~/GoogleDrive/lab/R/color_schemes/", pattern = ".txt")
  file.names <- gsub(".txt", "", files)

  bro_pals <- list()
  for (i in 1:length(files)) {

    lines <- readLines(paste0("~/GoogleDrive/lab/R/color_schemes/",files[i]))

    library(stringr)
    cols <- str_match(lines, "#\\w{6}")[, 1]
    names(cols) <- str_match(lines, "\\$(.*):")[, 2]

    bro_pals[[i]] <- toupper(cols)
    names(bro_pals)[i] <- file.names[i]
  }
  return(bro_pals)
}

# bro_pals <- get_colors()
# bro_colors <- unlist(unname(bro_pals))
# usethis::use_data(bro_pals, bro_colors, overwrite = TRUE)

# construction of ggplot2 palettes inspired by this blog post:
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# do not @export
bro_pal <- function(palette = "metro_ui", reverse = FALSE, ...) {
  pal <- bro_pals[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

#' @export
bro_scale_color_c <- function(palette = "blue_pink_yellow", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  scale_color_gradientn(colours = pal(256), ...)
}

#' @export
bro_scale_color_d <- function(palette = "metro_ui", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  discrete_scale("colour", paste0("bro_", palette), palette = pal, ...)
}

#' @export
bro_scale_fill_c <- function(palette = "blue_pink_yellow", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  scale_fill_gradientn(colours = pal(256), ...)
}

#' @export
bro_scale_fill_d <- function(palette = "metro_ui", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  discrete_scale("fill", paste0("bro_", palette), palette = pal, ...)
}

#' @export
bro_pals_show <- function(pals = bro_pals, show_labels = FALSE) {
  tidy_pals <-
    tibble::enframe(pals, name = "pal") %>%
    tidyr::unnest(cols = c(value)) %>%
    tibble::rownames_to_column("nr") %>%
    mutate(
      nr = factor(nr, levels = row_number()),
      label_color = if_else(as(colorspace::hex2RGB(value), "HLS")@coords[,2] > 0.6, "#000000", "#FFFFFF"))

  ggplot(tidy_pals, aes(x = nr, y = 1)) +
    geom_tile(fill = tidy_pals$value) +
    { if (show_labels) geom_text(aes(label = value), angle = 90, size = 2.5, color = tidy_pals$label_color) else NULL
    } +
    scale_x_discrete(expand = c(0, 0)) +
    facet_wrap(vars(pal), scales = "free", ncol = 2) +
    bro_style_font_size() +
    theme_void() +
    theme(axis.ticks = element_line(colour = "#FFFFFF"))
}

#' @export
bro_modify_HLS <- function(x, H = 1, L = 1, S = 1) {
  hls_cols <- as(colorspace::hex2RGB(x), "HLS")
  hls_cols@coords[,1] <- hls_cols@coords[,1] * H
  hls_cols@coords[,2] <- hls_cols@coords[,2] * L
  hls_cols@coords[,3] <- hls_cols@coords[,3] * S
  return(colorspace::hex(as(hls_cols, "sRGB")))
}

